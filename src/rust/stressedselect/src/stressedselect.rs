#![feature(mpsc_select)]

extern crate time;

use std::thread;
use std::collections::HashMap;
use std::sync::mpsc;
use std::sync::mpsc::{SyncSender, Receiver};
use std::sync::mpsc::Select;
use std::io::prelude::*;
use std::fs::File;
use std::vec::Vec;
use time::PreciseTime;

static N : i32 = 2;

struct StressedPacket {
    writer : i32,
    n : i32,
}

fn stressedwriter(out : SyncSender<StressedPacket>, writer : i32, WRITES : i32) {
    for n in 0..WRITES {
        out.send(StressedPacket{writer : writer, n : n}).unwrap();
    }
}

fn stressedreader(chans : Vec<Receiver<StressedPacket>>, writers_per_channel : i32, writes : i32) {
    let mut results = Vec::new();
    let mut counter : i32 = N.pow(16);
    let mut total = chans.len() * writers_per_channel as usize * writes as usize;
    println!("total={}, writes={}, writers per channel={}", total, writes, writers_per_channel);
    let mut start = time::precise_time_ns();
    let sel = Select::new();
    let mut handles = HashMap::new();
    for i in 0..chans.len() {
        let mut h = sel.handle(&chans[i]);
        unsafe { h.add(); }
        handles.insert(h.id(), h);
    }
    println!("...");
    for _ in 0..(total + 1) {
        if counter == 0 {
            let total_time = time::precise_time_ns() - start;
            results.push(total_time / N.pow(16) as u64);
            start = time::precise_time_ns();
            counter = N.pow(16);
        }
        println!("...");
        let id : usize = sel.wait();
        println!("{}", id);
        let c = handles.get_mut(&id).unwrap();
        println!("...");
        let n = c.recv().unwrap();
        println!("{} {}", id, n.n);
        counter -= 1;
    }
    let mut buffer = File::create(format!("ss-rust-{}-{}.csv", chans.len(), writers_per_channel)).unwrap();
    for r in results {
        write!(buffer, "{}\n", r);
    }
    buffer.flush();
}

fn main() {
    let mut c = 2;
    let mut w = 1;
    while c <= 1024 {
        while w <= 1024 {
            let writes = (N.pow(26) / c) / w;
            println!("{}:{}", c, w);
            let mut writer = 0;
            let mut receivers = Vec::new();
            let mut senders = Vec::new();
            for _ in 0..c {
                let (c_tx, c_rx) = mpsc::sync_channel(0);
                for _ in 0..w {
                    let tmp = c_tx.clone();
                    thread::spawn(move || { stressedwriter(tmp, 1, writes); });
                    writer += 1;
                }
                receivers.push(c_rx);
                senders.push(c_tx);
            }
            stressedreader(receivers, w, writes);
            w *= 2;
        }
        c *= 2;
    }
}

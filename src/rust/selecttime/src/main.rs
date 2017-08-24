#![feature(mpsc_select)]

extern crate time;

use std::thread;
use std::sync::mpsc;
use std::sync::mpsc::{SyncSender, Receiver};
use std::sync::mpsc::Select;
use std::io::prelude::*;
use std::fs::File;
use std::vec::Vec;
use time::PreciseTime;

struct StressedPacket {
    writer : i32,
    n : i32,
}

fn writer(out : SyncSender<StressedPacket>, writer : i32, writes : i32) {
    for i in 0..writes {
        out.send(StressedPacket{writer : writer, n : i}).unwrap();
    }
}

fn save_results(N : usize, results : Vec<u64>) {
    let mut buffer = File::create(format!("st-rust-{}.csv", N)).unwrap();
    for r in results {
        write!(buffer, "{}\n", r);
    }
    buffer.flush();
}

fn do_select1(in0 : &Receiver<StressedPacket>) {
    select! {
        _ = in0.recv() => println!("")
    }
}

fn do_select2(in0 : &Receiver<StressedPacket>, in1 : &Receiver<StressedPacket>) {
    select! {
        _ = in0.recv() => (),
        _ = in1.recv() => ()
    }
}

fn do_select4(in0 : &Receiver<StressedPacket>, in1 : &Receiver<StressedPacket>, in2 : &Receiver<StressedPacket>, in3 : &Receiver<StressedPacket>) {
    select! {
        _ = in0.recv() => (),
        _ = in1.recv() => (),
        _ = in2.recv() => (),
        _ = in3.recv() => ()
    }
}

fn do_select8(in0 : &Receiver<StressedPacket>, in1 : &Receiver<StressedPacket>, in2 : &Receiver<StressedPacket>, in3 : &Receiver<StressedPacket>,
              in4 : &Receiver<StressedPacket>, in5 : &Receiver<StressedPacket>, in6 : &Receiver<StressedPacket>, in7 : &Receiver<StressedPacket>) {
    select! {
        _ = in0.recv() => (),
        _ = in1.recv() => (),
        _ = in2.recv() => (),
        _ = in3.recv() => (),
        _ = in4.recv() => (),
        _ = in5.recv() => (),
        _ = in6.recv() => (),
        _ = in7.recv() => ()
    }
}
fn do_select(N : i32, input : &Vec<Receiver<StressedPacket>>) {
    match N {
        1 => do_select1(&input[0]),
        2 => do_select2(&input[0], &input[1]),
        4 => do_select4(&input[0], &input[1], &input[2], &input[3]),
        8 => do_select8(&input[0], &input[1], &input[2], &input[3], &input[4], &input[5], &input[6], &input[7]),
        _ => ()
    }
}

fn reader(N : i32, input : Vec<Receiver<StressedPacket>>, total : i32) {
    let mut results = Vec::new();
    let mut start = time::precise_time_ns();
    let mut i = 0;
    for count in 0..total {
        if count % 65536 == 0 {
            let total = (time::precise_time_ns() - start) / 65536;
            results.push(total);
            println!("{}", i);
            println!("{} ns", total);
            start = time::precise_time_ns();
            i += 1;
        }
        do_select(N, &input);
    }
    save_results(input.len(), results);
}

fn experiment(iterations : i32, threads : i32) {
    let mut chans = Vec::new();
    for i in 0..threads {
        let (tx, rx) : (SyncSender<StressedPacket>, Receiver<StressedPacket>) = mpsc::sync_channel(0);
        chans.push(rx);
        thread::spawn(move || { writer(tx, i, iterations / threads); } );
    }
    reader(threads, chans, iterations);
}

fn main() {
    let x : i32 = 2;
    experiment(x.pow(24), 1);
    experiment(x.pow(24), 2);
    experiment(x.pow(24), 4);
    experiment(x.pow(24), 8);
}
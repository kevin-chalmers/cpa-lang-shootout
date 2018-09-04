extern crate time;

use std::thread;
use std::sync::mpsc;
use std::sync::mpsc::{SyncSender, Receiver};
use std::io::prelude::*;
use std::fs::File;
use std::vec::Vec;
use time::PreciseTime;

const TOTAL : i32 = 10000000;

fn id(input : Receiver<i32>, output : SyncSender<i32>) {
    for _ in 1..TOTAL {
        let x = input.recv().unwrap();
        output.send(x).unwrap();
    }
}

fn prefix(n : i32, input : Receiver<i32>, output : SyncSender<i32>) {
    output.send(n).unwrap();
    id(input, output);
}

fn delta(input : Receiver<i32>, out0 : SyncSender<i32>, out1 : SyncSender<i32>) {
    for _ in 1..TOTAL {
        let x = input.recv().unwrap();
        out0.send(x).unwrap();
        out1.send(x).unwrap();
    }
}

fn succ(input : Receiver<i32>, output : SyncSender<i32>) {
    for _ in 1..TOTAL {
        let x = input.recv().unwrap();
        output.send(x + 1).unwrap();
    }
}

fn consume(input : Receiver<i32>) {
    let mut vec = Vec::new();
    for _ in 1..(TOTAL / 10000) {
        let start = time::precise_time_ns();
        for _ in 1..10000 {
            input.recv().unwrap();
        }
        let total = (time::precise_time_ns() - start) / 10000;
        println!("{} ns", total);
        vec.push(total);
    }
    let mut buffer = File::create("ct-rust.csv").unwrap();
    for v in vec {
        write!(buffer, "{},", v);
    }
    buffer.flush();
}

fn main() {
    let (a_tx, a_rx) = mpsc::sync_channel(0);
    let (b_tx, b_rx) = mpsc::sync_channel(0);
    let (c_tx, c_rx) = mpsc::sync_channel(0);
    let (d_tx, d_rx) = mpsc::sync_channel(0);
    let p = thread::spawn(|| { prefix(0, a_rx, b_tx); });
    let d = thread::spawn(|| { delta(b_rx, c_tx, d_tx); });
    let s = thread::spawn(|| { succ(c_rx, a_tx); });
    consume(d_rx);
}

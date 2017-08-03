extern crate time;
extern crate rand;

use std::thread;
use std::sync::mpsc;
use std::sync::mpsc::{SyncSender, Receiver};
use std::io::prelude::*;
use std::fs::File;
use std::vec::Vec;
use time::PreciseTime;
use rand::distributions::{IndependentSample, Range};

fn montecarlopi(iterations : i32, master : SyncSender<f64>) {
    let between = Range::new(0., 1.);
    let mut rng = rand::thread_rng();
    let mut in_circle = 0;
    for _ in 0..iterations {
        let x = between.ind_sample(&mut rng);
        let y = between.ind_sample(&mut rng);
        if x * x + y * y <= 1. {
            in_circle += 1;
        }
    }
    master.send((4. * in_circle as f64) / iterations as f64).unwrap();
}

fn experiment(iterations : i32, threads : i32) {
    let mut buffer = File::create(format!("mcp-rust-{}.csv", threads)).unwrap();
    let (c_tx, c_rx) = mpsc::sync_channel(0);
    for i in 0..100 {
        let start = time::precise_time_ns();
        let mut vec = Vec::new();
        for _ in 0..threads {
            let child_tx = c_tx.clone();
            vec.push(thread::spawn(move || {montecarlopi(iterations / threads, child_tx); }));
        }
        let mut pi = 0.;
        for _ in 0..threads {
            pi += c_rx.recv().unwrap();
        }
        pi /= threads as f64;
        let total = time::precise_time_ns() - start;
        println!("{} {} {}", i, pi, total);
        write!(buffer, "{}\n", total);
    }
    buffer.flush();
}

fn main() {
    let x : i32 = 2;
    experiment(x.pow(24), 1);
    experiment(x.pow(24), 2);
    experiment(x.pow(24), 4);
    experiment(x.pow(24), 8);
    experiment(x.pow(24), 16);
}

#![feature(mpsc_select)]

use std::thread;
use std::sync::mpsc;
use std::sync::mpsc::{SyncSender, Receiver};

fn tester(input1 : Receiver<i32>, input2 : Receiver<i32>) {
    select! {
        _ = input1.recv() => println!("Input 1"),
        _ = input2.recv() => println!("Input 2")
    }
}

fn send(output : SyncSender<i32>) {
    output.send(0).unwrap();
}

fn main() {
    let (a_tx, a_rx) = mpsc::sync_channel(0);
    let (b_tx, b_rx) = mpsc::sync_channel(0);
    let s1 = thread::spawn(|| { sender(a_tx); });
    let s2 = thread::spawn(|| { sender(b_tx); });
    tester(a_rx, b_rx)
}

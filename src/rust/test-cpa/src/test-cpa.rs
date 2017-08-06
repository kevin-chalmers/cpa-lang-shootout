#![feature(mpsc_select)]

use std::thread;
use std::collections::HashMap;
use std::sync::mpsc;
use std::sync::mpsc::{SyncSender, Receiver};
use std::sync::mpsc::Select;

fn tester(inputs : Vec<Receiver<i32>>) {
    let sel = Select::new();
    let mut handles = HashMap::new();
    for i in 0..inputs.len() {
        let mut h = sel.handle(&inputs[i]);
        unsafe { h.add(); }
        println!("{}->{} ({})", i, h.id(), &inputs[i].id());
        handles.insert(h.id(), h);
    }
    for i in 0..1000 {
        let mut id = sel.wait();
        let mut c = handles.get_mut(&id).unwrap();
        let mut n = c.recv().unwrap();
        println!("{} {} {}", i, id, n);
    }
}

fn send(idx : i32, output : SyncSender<i32>) {
    println!("{}", idx);
    for i in 0..10 {
        output.send(i).unwrap();
        println!("{} sent {}", idx, i);
    }
}

fn main() {
    let mut senders = Vec::new();
    let mut receivers = Vec::new();
    let mut threads = Vec::new();
    for i in 0..3 {
        let (c_tx, c_rx) = mpsc::sync_channel(0);
        let mut tmp = c_tx.clone();
        let t = thread::spawn(move || { send(i, tmp); });
        tmp = c_tx.clone();
        threads.push(t);
        senders.push(tmp);
        receivers.push(c_rx);
        println!("{}", receivers.len());
    }
    tester(receivers);
}

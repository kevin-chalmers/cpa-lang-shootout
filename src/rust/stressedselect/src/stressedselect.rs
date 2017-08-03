extern crate time;

use std::thread;
use std::sync::mpsc;
use std::sync::mpsc::{SyncSender, Receiver};
use std::io::prelude::*;
use std::fs::File;
use std::vec::Vec;
use time::PreciseTime;

static mut WRITES : i32;

struct StressedPacket {
    writer : i32,
    n : i32,
}

fn stressedwriter(out : SyncSender<StressedPacket>, writer : i32) {
    for n in 1..WRITES {
        out.send(StressedPacket{writer : writer, n : n}).unwrap();
    }
}

fn stressedreader(chans : Vec<Receiver<StressedPacket>, WRITERS_PER_CHANNEL) {
    let mut counter : i32 = 2.pow(16);
}

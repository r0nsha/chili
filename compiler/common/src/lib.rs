pub mod build_options;
pub mod builtin;
pub mod compiler_info;
pub mod mem;
pub mod target;

use std::time::Duration;

use colored::{Color, Colorize};
use stopwatch::Stopwatch as SW;

pub struct Stopwatch<'s> {
    label: &'s str,
    sw: SW,
}

impl<'s> Stopwatch<'s> {
    pub fn new(label: &'s str) -> Self {
        Self {
            label,
            sw: SW::new(),
        }
    }

    pub fn start_new(label: &'s str) -> Self {
        Self {
            label,
            sw: SW::start_new(),
        }
    }

    pub fn start(&mut self) {
        self.sw.start();
    }

    pub fn stop(&mut self) {
        self.sw.stop();
    }

    pub fn elapsed(&self) -> Duration {
        self.sw.elapsed()
    }

    pub fn print(&self) {
        let value = (self.elapsed().as_micros() as f32) / 1000.0;
        let color = if value < 1.0 {
            Color::BrightGreen
        } else if value < 5.0 {
            Color::BrightYellow
        } else if value < 10.0 {
            Color::Yellow
        } else {
            Color::BrightRed
        };
        println!("{:<16}{:.2}ms", self.label.color(color).bold(), value);
    }
}

#[macro_export]
macro_rules! time {
    ($label: literal, $body: expr) => {{
        let sw = common::Stopwatch::start_new($label);
        let res = $body;
        sw.print();
        res
    }};
}

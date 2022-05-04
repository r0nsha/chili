pub mod build_options;
pub mod builtin;
pub mod mem;
pub mod scopes;
pub mod target;

use colored::{Color, Colorize};
use std::time::Duration;
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

    pub fn elapsed(&self) -> Duration {
        self.sw.elapsed()
    }

    pub fn print(&self) {
        let value = self.elapsed().as_micros();
        let color = if value < 100 {
            Color::BrightCyan
        } else if value < 1000 {
            Color::BrightGreen
        } else if value < 1000 * 100 {
            Color::BrightYellow
        } else {
            Color::BrightRed
        };
        println!("{:<16}{}μs", self.label.color(color).bold(), value);
    }
}

#[macro_export]
macro_rules! time {
    ($enabled:expr, $label:literal, $body:expr) => {{
        if $enabled {
            let sw = common::Stopwatch::start_new($label);
            let res = $body;
            sw.print();
            res
        } else {
            $body
        }
    }};
    ($enabled:expr, $label:expr, $body:expr) => {{
        if $enabled {
            let sw = common::Stopwatch::start_new($label);
            let res = $body;
            sw.print();
            res
        } else {
            $body
        }
    }};
}

#[macro_export]
macro_rules! debug {
    ($($e:expr),+) => {
        {
            #[cfg(debug_assertions)]
            {
                dbg!($($e),+)
            }
            #[cfg(not(debug_assertions))]
            {
                ($($e),+)
            }
        }
    };
}

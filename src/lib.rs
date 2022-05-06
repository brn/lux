extern crate enum_index;

#[macro_use]
extern crate enum_index_derive;

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate paste;

#[macro_use]
extern crate const_format;

#[macro_use]
extern crate static_assertions;

#[macro_use]
extern crate property;

extern crate backtrace;

#[macro_use]
extern crate itertools;

extern crate termion;

extern crate byteorder;

extern crate inkwell;

#[macro_use]
mod debug_tools;

pub mod def;

#[macro_use]
pub mod macros;

#[macro_use]
pub mod utility;

#[macro_use]
pub mod structs;

pub mod unicode;
#[macro_use]
pub mod context;

mod vm;

pub mod heap;

pub mod parser;

#[cfg(test)]
#[macro_use]
extern crate indoc;

extern crate threadpool;

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate static_assertions;

extern crate xxhash_rust;

#[macro_use]
pub mod macros;

pub mod def;
pub mod heap;

#[macro_use]
pub mod utility;

#[macro_use]
pub mod structs;

pub mod unicode;
#[macro_use]
pub mod context;

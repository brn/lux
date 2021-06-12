mod backend;
mod rope;
mod small_string;
mod string;
mod to_number;
mod u16_str;

pub use self::string::{FlatString, JsString};
pub use self::u16_str::{from_utf8, FixedU16CodePointArray, FixedU16CodePointArrayItearator};

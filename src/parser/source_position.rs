use property::Property;
use std::cmp::{Eq, Ordering, PartialEq, PartialOrd};
use std::fmt::{Debug, Formatter, Result as FmtResult};

#[derive(PartialEq, Eq, Property, Clone)]
pub struct SourcePosition {
  #[property(get(type = "copy"))]
  start_col: u32,
  #[property(get(type = "copy"))]
  end_col: u32,
  #[property(get(type = "copy"))]
  start_line_number: u32,
  #[property(get(type = "copy"))]
  end_line_number: u32,
}

impl SourcePosition {
  pub fn new() -> SourcePosition {
    return SourcePosition {
      start_col: 0,
      end_col: 0,
      start_line_number: 0,
      end_line_number: 0,
    };
  }

  pub fn with(
    start_col: Option<u32>,
    end_col: Option<u32>,
    start_line_number: Option<u32>,
    end_line_number: Option<u32>,
  ) -> SourcePosition {
    return SourcePosition {
      start_col: if start_col.is_some() { start_col.unwrap() } else { 0 },
      end_col: if end_col.is_some() { end_col.unwrap() } else { 0 },
      start_line_number: if start_line_number.is_some() {
        start_line_number.unwrap()
      } else {
        0
      },
      end_line_number: if end_line_number.is_some() {
        end_line_number.unwrap()
      } else {
        0
      },
    };
  }

  pub fn add_start_col(&mut self, v: u32) {
    self.start_col += v;
  }

  pub fn add_end_col(&mut self, v: u32) {
    self.end_col += v;
  }

  pub fn inc_end_col(&mut self) {
    self.end_col += 1;
  }

  pub fn inc_end_line_number(&mut self) {
    self.end_line_number += 1;
  }
}

impl Debug for SourcePosition {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    return write!(
      f,
      "SourcePosition: {{
  start_col: {},
  end_col: {},
  start_line_number: {},
  end_line_number: {}
}}",
      self.start_col, self.end_col, self.start_line_number, self.end_line_number
    );
  }
}

impl PartialOrd for SourcePosition {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    use Ordering::*;
    if self.start_line_number == other.start_line_number {
      return Some(if self.start_col > other.start_col {
        Greater
      } else {
        Less
      });
    }
    return Some(if self.start_line_number > other.start_line_number {
      Greater
    } else {
      Less
    });
  }
}

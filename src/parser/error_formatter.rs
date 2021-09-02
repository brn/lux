use super::super::structs::FixedU16CodePointArray;
use super::source_position::SourcePosition;

macro_rules! pos_range {
  ($start:expr, $end:expr) => {
    SourcePosition::with(
      Some($start.end_col()),
      Some($end.end_col() + 1),
      Some($start.end_line_number()),
      Some($end.end_line_number()),
    )
  };
  (@just $start:expr, $end:expr) => {
    SourcePosition::with(
      Some($start.end_col()),
      Some($end.end_col()),
      Some($start.end_line_number()),
      Some($end.end_line_number()),
    )
  };
}

pub fn format_error(
  filename: &str,
  source: FixedU16CodePointArray,
  message: &str,
  source_position: &SourcePosition,
) -> String {
  let utf8_source = source.to_utf8();
  let line_sources = utf8_source.split("\n").collect::<Vec<_>>();
  if source_position.start_line_number() == source_position.end_line_number() {
    if source_position.start_line_number() < line_sources.len() as u32 {
      let target_source = line_sources[source_position.start_line_number() as usize];
      let mut indicator_buffer = String::new();
      for _ in 0..source_position.start_col() {
        indicator_buffer.push_str(" ");
      }
      for _ in source_position.start_col()..source_position.end_col() {
        indicator_buffer.push_str("^");
      }
      return format!(
        "{} at {}:{}:{}\n{}\n{}",
        message,
        filename,
        source_position.start_col(),
        source_position.start_line_number() + 1,
        target_source,
        indicator_buffer
      );
    }
  } else {
    if source_position.start_line_number() < line_sources.len() as u32
      && source_position.end_line_number() < line_sources.len() as u32
    {
      let mut msgs = format!(
        "{} at {}:{}:{}",
        message,
        filename,
        source_position.start_col(),
        source_position.start_line_number() + 1
      );
      for line in source_position.start_line_number()..source_position.end_line_number() + 1 {
        msgs.push_str("\n");
        let target_source = line_sources[line as usize];
        let mut indicator_buffer = String::new();
        if line == 0 {
          for _ in 0..source_position.start_col() {
            indicator_buffer.push_str(" ");
          }
          for _ in source_position.start_col()..(target_source.len() as u32) {
            indicator_buffer.push_str("^");
          }
        } else if line == source_position.end_line_number() {
          for _ in 0..source_position.end_col() {
            indicator_buffer.push_str("^");
          }
        } else {
          for _ in 0..target_source.len() {
            indicator_buffer.push_str("^");
          }
        }
        msgs = format!("{}{}\n{}", msgs, target_source, indicator_buffer);
      }
      return msgs;
    }
  }

  return String::new();
}

#[cfg(test)]
mod error_formatter_test {
  use super::super::source_position::SourcePosition;
  use super::*;
  use crate::context::LuxContext;

  #[test]
  fn format_error_test() {
    let context = LuxContext::new_until_internal_object_records();
    let source = "function X() {return invalid;}";
    let indicator = "                     ^^^^^^^";
    let msg = format_error(
      "anonymous",
      FixedU16CodePointArray::from_utf8(context, source),
      "Invalid Token",
      &SourcePosition::with(Some(21), Some(28), Some(0), Some(0)),
    );
    assert_eq!(
      msg,
      format!("Invalid Token at anonymous:21:1\n{}\n{}", source, indicator)
    );
  }

  #[test]
  fn format_multiline_error_test() {
    let context = LuxContext::new_until_internal_object_records();
    let source = "function X() {\n  return invalid;\n}";
    let indicator_1 = "         ^^^^^";
    let indicator_2 = "^^^^^^^^^^^^^^^^^";
    let indicator_3 = "^";
    let msg = format_error(
      "anonymous",
      FixedU16CodePointArray::from_utf8(context, source),
      "Invalid Token",
      &SourcePosition::with(Some(9), Some(1), Some(0), Some(2)),
    );
    assert_eq!(
      msg,
      format!(
        "Invalid Token at anonymous:9:1\nfunction X() {{\n{}\n  return invalid;\n{}\n}}\n{}",
        indicator_1, indicator_2, indicator_3
      )
    );
  }
}

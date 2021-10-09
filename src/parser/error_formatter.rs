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
  (@start $start:expr, $end:expr) => {
    SourcePosition::with(
      Some($start.start_col()),
      Some($end.end_col()),
      Some($start.start_line_number()),
      Some($end.end_line_number()),
    )
  };
}

const ONE_LINE_LIMIT: u64 = 50;

fn calc_end_position(source_position: &SourcePosition) -> u64 {
  if source_position.end_col() > ONE_LINE_LIMIT {
    source_position.start_col() + ONE_LINE_LIMIT
  } else {
    source_position.end_col()
  }
}

fn format_one_line(target_source: &str, source_position: &SourcePosition) -> (String, String) {
  let mut indicator_buffer = String::new();
  let mut formatted_source = String::new();
  if target_source.len() > ONE_LINE_LIMIT as usize {
    let diff = source_position.end_col() - source_position.start_col();
    if diff > ONE_LINE_LIMIT {
      let end = calc_end_position(&source_position);
      for _ in source_position.start_col()..end + 3 {
        indicator_buffer.push_str("^");
      }
      let source = &target_source[(source_position.start_col() as usize)..(end as usize)];
      formatted_source = format!("{}...", source);
    } else {
      let remains = ONE_LINE_LIMIT - diff;
      let start = if source_position.start_col() > remains {
        source_position.start_col() - remains
      } else {
        0
      };
      let end = if source_position.start_col() > remains {
        source_position.start_col()
      } else {
        remains - (remains - source_position.start_col())
      };
      for _ in start..end {
        indicator_buffer.push_str(" ");
      }
      for _ in source_position.start_col()..source_position.end_col() {
        indicator_buffer.push_str("^");
      }
      let source = &target_source[(start as usize)..(source_position.end_col() as usize)];
      formatted_source = format!("{}...", source);
    }
  } else {
    for _ in 0..source_position.start_col() {
      indicator_buffer.push_str(" ");
    }

    for _ in source_position.start_col()..source_position.end_col() {
      indicator_buffer.push_str("^");
    }

    formatted_source = target_source.to_string();
  }

  return (indicator_buffer, formatted_source);
}

fn split_by_line(source: &str) -> Vec<String> {
  let mut ret = Vec::new();
  let mut buf = String::new();
  for ch in source.chars() {
    buf.push(ch);
    if ch == '\r' {
      ret.push(buf.to_owned());
      buf.clear();
      if ch == '\n' {
        continue;
      }
    } else if ch == '\n' {
      ret.push(buf.to_owned());
      buf.clear();
    }
  }
  if buf.len() > 0 {
    ret.push(buf.to_owned());
  }
  return ret;
}

pub fn format_error(
  filename: &str,
  source: FixedU16CodePointArray,
  message: &str,
  source_position: &SourcePosition,
  should_omit_position: bool,
) -> String {
  let utf8_source = source.to_utf8();
  let line_sources = split_by_line(&utf8_source);
  if source_position.start_line_number() == source_position.end_line_number() {
    if source_position.start_line_number() < line_sources.len() as u32 {
      let (indicator_buffer, target_source) = format_one_line(&line_sources[source_position.start_line_number() as usize], source_position);

      if !should_omit_position {
        return format!(
          "{} at {}:{}:{}\n{}\n{}",
          message,
          filename,
          source_position.start_col(),
          source_position.start_line_number() + 1,
          target_source,
          indicator_buffer
        );
      } else {
        return format!("{}\n{}\n{}", message, target_source, indicator_buffer);
      }
    }
  } else {
    if source_position.start_line_number() < line_sources.len() as u32 && source_position.end_line_number() < line_sources.len() as u32 {
      let mut msgs = if !should_omit_position {
        format!(
          "{} at {}:{}:{}",
          message,
          filename,
          source_position.start_col(),
          source_position.start_line_number() + 1
        )
      } else {
        format!("{}", message,)
      };

      for line in source_position.start_line_number()..source_position.end_line_number() + 1 {
        msgs.push_str("\n");
        let target_source = &line_sources[line as usize];
        let (indicator, source) = if line == 0 {
          format_one_line(
            target_source,
            &SourcePosition::with(
              Some(source_position.start_col()),
              Some(target_source.len() as u64),
              Some(0),
              Some(0),
            ),
          )
        } else if line != source_position.end_line_number() {
          format_one_line(
            target_source,
            &SourcePosition::with(Some(0), Some(target_source.len() as u64), Some(0), Some(0)),
          )
        } else {
          format_one_line(
            target_source,
            &SourcePosition::with(Some(0), Some(target_source.len() as u64), Some(0), Some(0)),
          )
        };
        msgs = format!("{}{}: {}\n   {}", msgs, line, source, indicator);
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
      false,
    );
    assert_eq!(msg, format!("Invalid Token at anonymous:21:1\n{}\n{}", source, indicator));
  }

  #[test]
  fn format_error_test_2() {
    let context = LuxContext::new_until_internal_object_records();
    let source = "(class {constructor() {} constructor() {}});PARSER_SENTINEL";
    let msg = format_error(
      "anonymous",
      FixedU16CodePointArray::from_utf8(context, source),
      "Invalid Token",
      &SourcePosition::with(Some(25), Some(36), Some(0), Some(0)),
      false,
    );
    assert_eq!(
      msg,
      "Invalid Token at anonymous:25:1\n(class {constructor() {} constructor...\n                         ^^^^^^^^^^^"
    );
  }

  #[test]
  fn format_multiline_error_test() {
    let context = LuxContext::new_until_internal_object_records();
    let source = "function X() {\n  return invalid;\n}";
    let msg = format_error(
      "anonymous",
      FixedU16CodePointArray::from_utf8(context, source),
      "Invalid Token",
      &SourcePosition::with(Some(9), Some(1), Some(0), Some(2)),
      false,
    );
    assert_eq!(
      msg,
      "Invalid Token at anonymous:9:1\n0: function X() {\n            ^^^^^\n1:   return invalid;\n   ^^^^^^^^^^^^^^^^^\n2: }\n   ^"
    );
  }

  #[test]
  fn format_error_exceeds_one_line_limit_test() {
    let context = LuxContext::new_until_internal_object_records();
    let source = "function X() {var aaaaaaaaaaaaabbbbbbbbbbbbbbbbbccccccccccceeeeeeeeeeeeeeeedddddddddd; return invalid;}";
    let indicator = "                                           ^^^^^^^";
    let msg = format_error(
      "anonymous",
      FixedU16CodePointArray::from_utf8(context, source),
      "Invalid Token",
      &SourcePosition::with(Some(94), Some(101), Some(0), Some(0)),
      false,
    );
    assert_eq!(
      msg,
      format!(
        "Invalid Token at anonymous:94:1\n{}\n{}",
        "cccccccceeeeeeeeeeeeeeeedddddddddd; return invalid...", indicator
      )
    );
  }

  #[test]
  fn format_error_exceeds_one_line_limit_test_2() {
    let context = LuxContext::new_until_internal_object_records();
    let source = "function X() {return invalid_invalid_invalid_invalid_invalid_invalid_invalid_invalid_invalid_invalid;}";
    let msg = format_error(
      "anonymous",
      FixedU16CodePointArray::from_utf8(context, source),
      "Invalid Token",
      &SourcePosition::with(Some(21), Some(100), Some(0), Some(0)),
      false,
    );
    assert_eq!(
      msg,
      "Invalid Token at anonymous:21:1\ninvalid_invalid_invalid_invalid_invalid_invalid_in...\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
    );
  }

  #[test]
  fn format_multiline_error_exceeds_one_line_limits_test() {
    let context = LuxContext::new_until_internal_object_records();
    let source = "function Xaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbdddddddddddddddddddd() {\n  var aaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbccccccccccccccce;return invalid;\n  var invalid;\n}";
    let msg = format_error(
      "anonymous",
      FixedU16CodePointArray::from_utf8(context, source),
      "Invalid Token",
      &SourcePosition::with(Some(9), Some(11), Some(0), Some(3)),
      false,
    );
    assert_eq!(
      msg,
      "Invalid Token at anonymous:9:1\n0: Xaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbddd...\n   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n1:   var aaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbccccccccccc...\n   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n2:   var invalid;\n   ^^^^^^^^^^^^^^\n3: }\n   ^"
    );
  }
}

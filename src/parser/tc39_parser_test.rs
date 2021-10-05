use super::parser::*;
use super::source::*;
use crate::context::LuxContext;
use std::fs;
use std::io;
use std::path::PathBuf;

fn parse(filename: &str, content: &str, parser_option: ParserOption, parser_type: ParserType, should_fail: bool) {
  let context = LuxContext::new_until_internal_object_records();
  let source = Source::new(context, filename, content);
  let mut parser = Parser::new(context, source.clone(), parser_option.clone());
  match parser.parse(parser_type) {
    Ok(ast) => {
      if should_fail {
        parser.print_stack_trace();
        panic!(format!(
          "Parsing {} succeeded, but failure expected\ncode is\n{}",
          filename, content
        ));
      }
    }
    Err(err) => {
      if !should_fail {
        parser.print_stack_trace();
        panic!(err.error_message());
      }
    }
  }
}

fn get_test_files<F: Fn(&fs::DirEntry)>(dir: &str, cb: F) -> io::Result<()> {
  let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let path = format!("tools/test262-parser-tests/{}", dir);
  d.push(&path);
  for entry in fs::read_dir(d.to_str().unwrap())? {
    let entry = entry?;
    let path = entry.path().clone();
    if !path.is_dir() {
      cb(&entry);
    }
  }
  Ok(())
}

const SHOULD_RUN_UNDER_STRICT_MODE_CASES: [&'static str; 2] = ["1aff49273f3e3a98.js", "12a74c60f52a60de.js"];

#[test]
fn tc39_parser_test_early() {
  if let Err(e) = get_test_files("early", |entry| {
    let path = entry.path();
    let content = fs::read_to_string(entry.path()).unwrap();
    let path_str = path.to_str().unwrap();
    let is_module = path_str.ends_with("module.js");
    let mut should_run_under_strict_mode = false;
    for file in SHOULD_RUN_UNDER_STRICT_MODE_CASES.iter() {
      if path_str.ends_with(file) {
        should_run_under_strict_mode = true;
        break;
      }
    }
    parse(
      path_str,
      &content,
      if should_run_under_strict_mode {
        ParserOptionBuilder {
          is_strict_mode: true,
          disable_skip_parser: true,
          ..Default::default()
        }
        .build()
      } else {
        ParserOptionBuilder {
          disable_skip_parser: true,
          ..Default::default()
        }
        .build()
      },
      if is_module { ParserType::Module } else { ParserType::Script },
      true,
    );
  }) {
    panic!("{:?}", e);
  }
}

use super::parser::*;
use super::source::*;
use crate::context::LuxContext;
use std::fs;
use std::io;
use std::path::PathBuf;

fn parse(filename: &str, content: &str, parser_option: ParserOption, should_fail: bool) {
  let context = LuxContext::new();
  let source = Source::new(context, filename, content);
  let mut parser = Parser::new(context, source.clone(), parser_option.clone());
  match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parser.parse())) {
    Ok(r) => match r {
      Ok(ast) => {
        if should_fail {
          println!(" ... failed");
          parser.print_stack_trace();
          println!("code: \n{}", content);
          panic!(
            "{}",
            format!("Parsing {} succeeded, but failure expected\ncode is\n{}", filename, content)
          );
        }
        println!(" ... ok");
      }
      Err(err) => {
        if !should_fail {
          println!(" ... failed");
          parser.print_stack_trace();
          println!("code: \n{}", content);
          panic!("{}", err.error_message());
        }
        println!(" ... ok");
      }
    },
    Err(err) => {
      println!(" ... failed");
      parser.print_stack_trace();
      panic!("{:?}", err);
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

const SHOULD_RUN_UNDER_STRICT_MODE_CASES: [&'static str; 9] = [
  "early/1aff49273f3e3a98.js",
  "early/12a74c60f52a60de.js",
  "early/be7329119eaa3d47.js",
  "early/e262ea7682c36f92.js",
  "early/ec31fa5e521c5df4.js",
  "early/a610a46980d6cc37.js",
  "fail/3990bb94b19b1071.module.js",
  "fail/79f882da06f88c9f.js",
  "fail/92b6af54adef3624.js",
];
const EXCLUDES_CASES: [&'static str; 2] = ["early/0f5f47108da5c34e.js", "early/4de83a7417cd30dd.js"];

fn run_tc39_parser_test(dir: &str, should_fail: bool) {
  if let Err(e) = get_test_files(dir, |entry| {
    let path = entry.path();
    let content = fs::read_to_string(entry.path()).unwrap();
    let path_str = path.to_str().unwrap();
    let is_module = path_str.ends_with("module.js");
    let mut should_run_under_strict_mode = false;
    for file in EXCLUDES_CASES.iter() {
      if path_str.ends_with(file) {
        println!("skip tc39_parer_test {}", path_str);
        return;
      }
    }
    for file in SHOULD_RUN_UNDER_STRICT_MODE_CASES.iter() {
      if path_str.ends_with(file) {
        should_run_under_strict_mode = true;
        break;
      }
    }
    print!("test parser::tc39_parer_tests::{}", path_str);

    parse(
      path_str,
      &content,
      if should_run_under_strict_mode {
        ParserOptionBuilder {
          is_strict_mode: true,
          disable_skip_parser: true,
          is_module,
          ..Default::default()
        }
        .build()
      } else {
        ParserOptionBuilder {
          disable_skip_parser: true,
          is_module,
          ..Default::default()
        }
        .build()
      },
      should_fail,
    );
  }) {
    panic!("{:?}", e);
  }
}

#[test]
fn extract_test() {
  parse("anonymous", "'use strict';'\\001'", ParserOptionBuilder::default().build(), true);
}

#[test]
fn tc39_parser_test_early() {
  run_tc39_parser_test("early", true);
}

#[test]
fn tc39_parser_test_pass() {
  run_tc39_parser_test("pass", false);
}

#[test]
fn tc39_parser_test_fail() {
  run_tc39_parser_test("fail", true);
}

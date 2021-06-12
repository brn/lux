use super::source_position::SourcePosition;
use property::Property;
use std::boxed::Box;
use std::vec::Vec;

#[derive(Clone, Property)]
pub struct ErrorDescriptor {
  ignore: bool,
  #[property(get(type = "copy"))]
  error_message: String,
  #[property(get(type = "copy"))]
  source_position: SourcePosition,
}

impl ErrorDescriptor {
  pub fn new(source_position: &SourcePosition) -> Box<ErrorDescriptor> {
    return Box::new(Self {
      ignore: false,
      error_message: String::from(""),
      source_position: source_position.clone(),
    });
  }
}

impl std::ops::Shl<&str> for Box<ErrorDescriptor> {
  type Output = Box<ErrorDescriptor>;
  fn shl(self, message: &str) -> Self::Output {
    self.error_message.push_str(message);
    return self;
  }
}

pub struct ErrorReporter {
  pending_errors: Vec<Box<ErrorDescriptor>>,
}

impl ErrorReporter {
  pub fn new() -> ErrorReporter {
    return ErrorReporter {
      pending_errors: Vec::new(),
    };
  }

  pub fn report_syntax_error(&mut self, ed: Box<ErrorDescriptor>) -> Box<ErrorDescriptor> {
    ed << "SyntaxError: ";
    self.pending_errors.push(ed);
    return ed;
  }

  pub fn print_errors(&self) {
    if self.has_pending_error() {
      for e in &self.pending_errors {
        println!("{}", e.error_message());
      }
    }
  }

  pub fn has_pending_error(&self) -> bool {
    return self.pending_errors.len() > 0;
  }

  pub fn last_error(&self) -> Option<String> {
    if self.has_pending_error() {
      return Some(self.pending_errors.last().unwrap().error_message());
    }
    return None;
  }
}

pub trait ReportSyntaxError {
  fn error_reporter(&mut self) -> &mut ErrorReporter;

  fn report_syntax_error(&mut self, ed: Box<ErrorDescriptor>) -> Box<ErrorDescriptor> {
    return self.error_reporter().report_syntax_error(ed);
  }
}

macro_rules! _base_report_syntax_error {
  ($parser:tt) => {{
    let e = ErrorDescriptor::new($parser.position());
    parser.report_syntax_error(e)
  }};
}

#[cfg(debug_assertions)]
macro_rules! report_syntax_error {
  (noreturn $parser:tt, $message:expr) => {
    _base_report_syntax_error!($parser) << "[Debug] line: " << line!() << "\n" << $message
  };
  ($parser:tt, $message:expr, $return_value:expr) => {
    report_scanner_error!(noreturn $parser, $message);
    return $return_value;
  };
}

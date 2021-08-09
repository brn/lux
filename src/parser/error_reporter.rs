use super::source_position::SourcePosition;
use property::Property;
use std::boxed::Box;
use std::vec::Vec;

#[derive(Clone, Property)]
pub struct ErrorDescriptor {
  ignore: bool,
  #[property(get(type = "ref"))]
  error_message: String,
  #[property(get(type = "ref"))]
  source_position: SourcePosition,
}

pub struct ErrorDescriptorMessageContainer<'a>(&'a mut String);

impl ErrorDescriptor {
  pub fn new(source_position: &SourcePosition) -> Box<ErrorDescriptor> {
    return Box::new(Self {
      ignore: false,
      error_message: String::from(""),
      source_position: source_position.clone(),
    });
  }
}

impl std::fmt::Debug for ErrorDescriptor {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(f, "Error: {} at: {:?}", self.error_message, self.source_position);
  }
}

impl<'a> std::ops::Shl<&str> for &'a mut Box<ErrorDescriptor> {
  type Output = ErrorDescriptorMessageContainer<'a>;
  fn shl(self, message: &str) -> Self::Output {
    self.error_message.push_str(message);
    return ErrorDescriptorMessageContainer::<'a>(&mut self.error_message);
  }
}

impl<'a> std::ops::Shl<String> for &'a mut Box<ErrorDescriptor> {
  type Output = ErrorDescriptorMessageContainer<'a>;
  fn shl(self, message: String) -> Self::Output {
    self.error_message.push_str(&message);
    return ErrorDescriptorMessageContainer::<'a>(&mut self.error_message);
  }
}

impl<'a> std::ops::Shl<&str> for ErrorDescriptorMessageContainer<'a> {
  type Output = ErrorDescriptorMessageContainer<'a>;
  fn shl(self, message: &str) -> Self::Output {
    self.0.push_str(message);
    return ErrorDescriptorMessageContainer::<'a>(self.0);
  }
}

impl<'a> std::ops::Shl<String> for ErrorDescriptorMessageContainer<'a> {
  type Output = ErrorDescriptorMessageContainer<'a>;
  fn shl(self, message: String) -> Self::Output {
    self.0.push_str(&message);
    return ErrorDescriptorMessageContainer::<'a>(self.0);
  }
}

#[derive(Property)]
pub struct ErrorReporter {
  #[property(get(type = "ref"))]
  pending_errors: Vec<Box<ErrorDescriptor>>,
}

impl ErrorReporter {
  pub fn new() -> ErrorReporter {
    return ErrorReporter {
      pending_errors: Vec::new(),
    };
  }

  pub fn report_syntax_error(&mut self, ed: Box<ErrorDescriptor>) -> &mut Box<ErrorDescriptor> {
    self.pending_errors.push(ed);
    return self.pending_errors.last_mut().unwrap();
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

  pub fn last_error(&self) -> Option<&String> {
    if self.has_pending_error() {
      return Some(self.pending_errors.last().unwrap().error_message());
    }
    return None;
  }
}

pub trait ReportSyntaxError {
  fn error_reporter(&mut self) -> &mut ErrorReporter;

  fn source_position(&self) -> &SourcePosition;

  fn report_syntax_error(&mut self, ed: Box<ErrorDescriptor>) -> &mut Box<ErrorDescriptor> {
    return self.error_reporter().report_syntax_error(ed);
  }
}

macro_rules! _base_report_syntax_error {
  ($parser:tt) => {{
    let e = ErrorDescriptor::new($parser.source_position());
    $parser.report_syntax_error(e)
  }};
}

#[cfg(debug_assertions)]
macro_rules! report_syntax_error {
  (noreturn $parser:tt, $message:expr) => {{
    debug_log!("===SYNTAX ERROR FOUND===");
    _base_report_syntax_error!($parser) << "[Debug] line: " << line!().to_string() << "\n" << $message
  }};
  ($parser:tt, $message:expr, $return_value:expr) => {{
    debug_log!("===SYNTAX ERROR FOUND===");
    report_syntax_error!(noreturn $parser, $message);
    return $return_value;
  }};
}

#[cfg(not(debug_assertions))]
macro_rules! report_syntax_error {
  (noreturn $parser:tt, $message:expr) => {
    _base_report_syntax_error!($parser) << $message
  };
  ($parser:tt, $message:expr, $return_value:expr) => {{
    report_syntax_error!(noreturn $parser, $message);
    return $return_value;
  }};
}

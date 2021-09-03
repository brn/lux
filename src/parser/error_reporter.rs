use super::error_formatter::*;
use super::source::Source;
use super::source_position::SourcePosition;
use crate::utility::Exotic;
use property::Property;
use std::boxed::Box;
use std::rc::Rc;
use std::vec::Vec;

#[derive(Clone, Property)]
pub struct ErrorDescriptor {
  #[property(get(type = "copy"))]
  is_registered: bool,
  #[property(skip)]
  error_message: String,
  #[property(get(type = "ref"))]
  source_position: SourcePosition,
  #[property(get(type = "ref"), set(type = "ref"))]
  source: Option<Rc<Source>>,
}

impl ErrorDescriptor {
  pub fn new(source_position: &SourcePosition) -> ErrorDescriptor {
    return Self {
      is_registered: false,
      error_message: String::from(""),
      source_position: source_position.clone(),
      source: None,
    };
  }

  pub fn mark_as_registered(&mut self) {
    self.is_registered = true;
  }

  pub fn append_message(&mut self, message: &str) {
    self.error_message.push_str(message);
  }

  pub fn error_message(&self) -> String {
    if let Some(ref source) = self.source {
      return format_error(
        &source.filename(),
        source.source_code(),
        &self.error_message,
        self.source_position(),
      );
    }
    return format!(
      "{} at anonymous {}:{}",
      self.error_message,
      self.source_position.start_col(),
      self.source_position.start_line_number()
    );
  }
}

impl std::fmt::Debug for ErrorDescriptor {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(f, "{}", self.error_message());
  }
}

impl std::fmt::Display for ErrorDescriptor {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(f, "{}", self.error_message());
  }
}

#[derive(Property)]
pub struct ErrorReporter {
  #[property(get(type = "ref"))]
  pending_errors: Vec<Exotic<ErrorDescriptor>>,

  source: Rc<Source>,
}

impl ErrorReporter {
  pub fn new(source: Rc<Source>) -> ErrorReporter {
    return ErrorReporter {
      pending_errors: Vec::new(),
      source,
    };
  }

  pub fn report_syntax_error(&mut self, mut ed: Exotic<ErrorDescriptor>) {
    if ed.is_registered() {
      return;
    }
    ed.mark_as_registered();
    ed.set_source(self.source.clone());
    self.pending_errors.push(ed);
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

  pub fn last_error(&self) -> Option<Exotic<ErrorDescriptor>> {
    if self.has_pending_error() {
      return Some(*self.pending_errors.last().unwrap());
    }
    return None;
  }
}

pub trait ReportSyntaxError {
  fn error_reporter(&mut self) -> &mut ErrorReporter;

  fn source_position(&self) -> &SourcePosition;

  fn report_syntax_error(&mut self, ed: Exotic<ErrorDescriptor>) {
    self.error_reporter().report_syntax_error(ed);
  }
}

macro_rules! _base_report_error {
  ($self:expr, $pos:expr, $message:expr) => {{
    let mut e = $self.region.alloc(ErrorDescriptor::new($pos));
    e.append_message($message);
    $self.report_syntax_error(e);
  }};
}

#[cfg(debug_assertions)]
macro_rules! report_error {
  (noreturn $self:expr, $message:expr, $pos:expr) => {{
    debug_log!("===SYNTAX ERROR FOUND===");
    let pos = $pos;
    let message = $message;
    _base_report_error!($self, pos, &format!("[Debug] line: {}\n{}", line!(), message));
  }};
  ($self:expr, $message:expr, $pos:expr, $return_value:expr) => {{
    debug_log!("===SYNTAX ERROR FOUND===");
    report_error!(noreturn $self, $message, $pos);
    return $return_value;
  }};
}

#[cfg(debug_assertions)]
macro_rules! parse_error {
  ($region:expr, $message:expr, $pos:expr) => {{
    debug_log!("===SYNTAX ERROR FOUND===");
    let pos = $pos;
    let message = $message;
    let mut e = $region.alloc(ErrorDescriptor::new($pos));
    e.append_message(&format!("[Debug] line: {}\n{}", line!(), message));
    Err(e)
  }};
}

#[cfg(not(debug_assertions))]
macro_rules! report_error {
  (noreturn $self:expr, $message:expr, $pos:expr) => {
    let pos = $pos;
    let message = $message;
    _base_report_error!($self, pos, message);
  };
  ($self, $message:expr, $pos:expr, $return_value:expr) => {{
    report_error!(noreturn $self, $message, $pos);
    return $return_value;
  }};
}

#[cfg(not(debug_assertions))]
macro_rules! parse_error {
  ($region:expr, $message:expr, $pos:expr) => {{
    let pos = $pos;
    let message = $message;
    let mut e = $region.alloc(ErrorDescriptor::new($pos));
    e.append_message(message);
    Err(e)
  }};
}

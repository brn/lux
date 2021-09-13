use super::ast::*;
use super::error_reporter::ErrorDescriptor;
use super::source_position::{RuntimeSourcePosition, SourcePosition};
use crate::utility::{Exotic, Region};

pub type ParseResult<T> = Result<T, Exotic<ErrorDescriptor>>;

bitflags! {
  pub struct ParserConstraints: u8 {
    const None = 0;
    const Template = 1;
    const Call = 2;
    const BindingPattern = 4;
    const Initializer = 8;
    const KeywordIdentifier = 16;
  }
}

#[derive(PartialEq, PartialOrd, Copy, Clone)]
pub enum OperatorPriority {
  None,
  NullCoalescing,
  LogicalOr,
  LogicalAnd,
  BitwiseOr,
  BitwiseXor,
  BitwiseAnd,
  Equality,
  Relational,
  Shift,
  Additive,
  Multiplicative,
  Exponentiation,
  Paren,
}

impl ParserConstraints {
  pub fn allow_template(&mut self) {
    *self |= ParserConstraints::Template;
  }

  pub fn is_template_allowed(&self) -> bool {
    return self.contains(ParserConstraints::Template);
  }

  pub fn allow_call(&mut self) {
    *self |= ParserConstraints::Call;
  }

  pub fn is_call_allowed(&self) -> bool {
    return self.contains(ParserConstraints::Call);
  }

  pub fn allow_binding_pattern(&mut self) {
    *self |= ParserConstraints::BindingPattern;
  }

  pub fn is_binding_pattern_allowed(&self) -> bool {
    return self.contains(ParserConstraints::BindingPattern);
  }

  pub fn allow_initializer(&mut self) {
    *self |= ParserConstraints::Initializer;
  }

  pub fn is_initializer_allowed(&self) -> bool {
    return self.contains(ParserConstraints::Initializer);
  }

  pub fn is_keyword_identifier_allowed(&self) -> bool {
    return self.contains(ParserConstraints::KeywordIdentifier);
  }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AssignmentTargetType {
  Simple,
  Invalid,
}

#[derive(Property, Clone)]
pub struct ExpressionContext {
  #[property(skip)]
  first_pattern_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_value_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_strict_mode_error: Option<Exotic<ErrorDescriptor>>,

  #[property(get(type = "copy"), set(type = "ref"))]
  assignment_target_type: AssignmentTargetType,

  #[property(get(type = "copy"), set(type = "ref"))]
  is_in_allowed: bool,

  #[property(get(type = "copy"), set(type = "ref"))]
  is_maybe_immediate_function: bool,
}

macro_rules! _get_set {
  ($name:ident) => {
    paste! {
      pub fn [<set_ $name>](&mut self, e: Exotic<ErrorDescriptor>) {
        if self.[<$name>].is_none() {
          self.[<$name>] = Some(e);
        }
      }

      pub fn [<$name>](&self) -> Option<Exotic<ErrorDescriptor>> {
        return self.[<$name>];
      }
    }
  };
}

impl ExpressionContext {
  pub fn new() -> Self {
    ExpressionContext {
      first_pattern_error: None,
      first_value_error: None,
      first_strict_mode_error: None,
      is_maybe_immediate_function: false,
      is_in_allowed: false,
      assignment_target_type: AssignmentTargetType::Simple,
    }
  }

  pub fn create_child_context(&self) -> ExpressionContext {
    let mut child = ExpressionContext::new();
    child.set_is_in_allowed(self.is_in_allowed);
    child.set_is_maybe_immediate_function(self.is_maybe_immediate_function);
    return child;
  }

  pub fn create_child_arrow_context(&self) -> ArrowFunctionContext {
    let mut child = ArrowFunctionContext::new();
    child.expression_context.set_is_in_allowed(self.is_in_allowed);
    child
      .expression_context
      .set_is_maybe_immediate_function(self.is_maybe_immediate_function);
    return child;
  }

  pub fn propagate(&self, ec: &mut ExprCtx) {
    if self.first_pattern_error.is_some() {
      ec.first_pattern_error = self.first_pattern_error.clone();
    }
    if self.first_value_error.is_some() {
      ec.first_value_error = self.first_value_error.clone();
    }
    if self.first_strict_mode_error.is_some() {
      ec.first_strict_mode_error = self.first_strict_mode_error.clone();
    }
    ec.assignment_target_type = self.assignment_target_type;
  }

  pub fn reset(&mut self) -> ExpressionContext {
    return std::mem::replace(self, ExpressionContext::new());
  }

  _get_set!(first_pattern_error);
  _get_set!(first_value_error);
  _get_set!(first_strict_mode_error);
}
impl std::fmt::Debug for ExpressionContext {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "pattern_error = {} value_errr = {} strict_mode_error = {}, imm_fn = {}",
      self.first_pattern_error.is_some(),
      self.first_value_error.is_some(),
      self.first_strict_mode_error.is_some(),
      self.is_maybe_immediate_function
    );
  }
}

#[derive(Property, Clone)]
pub struct ArrowFunctionContext {
  #[property(mut(public, suffix = "_mut"))]
  expression_context: ExpressionContext,

  #[property(skip)]
  first_arrow_parameter_error: Option<Exotic<ErrorDescriptor>>,

  #[property(get(type = "copy"), set(type = "ref"))]
  is_simple_parameter: bool,

  #[property(mut(public, suffix = "_mut"), set(disable))]
  var_list: Vec<(Vec<u16>, SourcePosition)>,
}

impl std::fmt::Debug for ArrowFunctionContext {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "{:?} arrow_param_error = {} var_list = {:?}",
      self.expression_context,
      self.first_arrow_parameter_error.is_some(),
      self.var_list
    );
  }
}

impl ArrowFunctionContext {
  pub fn new() -> Self {
    ArrowFunctionContext {
      expression_context: ExpressionContext::new(),
      first_arrow_parameter_error: None,
      is_simple_parameter: true,
      var_list: Vec::new(),
    }
  }

  pub fn create_child_context(&self) -> ExpressionContext {
    let mut child = ExpressionContext::new();
    child.set_is_in_allowed(self.expression_context.is_in_allowed);
    child.set_is_maybe_immediate_function(self.expression_context.is_maybe_immediate_function);
    return child;
  }

  pub fn create_child_arrow_context(&self) -> ArrowFunctionContext {
    let mut child = ArrowFunctionContext::new();
    child.expression_context = self.expression_context.create_child_context();
    return child;
  }

  _get_set!(first_arrow_parameter_error);

  pub fn propagate(&self, ec: &mut ExprCtx) {
    self.expression_context.propagate(ec);
    if let Ok(ctx) = ec.to_arrow_ctx_mut() {
      if self.first_arrow_parameter_error.is_some() {
        ctx.first_arrow_parameter_error = self.first_arrow_parameter_error;
      }
      ctx.is_simple_parameter = self.is_simple_parameter;
      ctx.var_list.append(&mut self.var_list.clone());
    }
  }

  pub fn propagate_arrow(&self, ec: &mut ExprCtx) {
    if let Ok(ctx) = ec.to_arrow_ctx_mut() {
      ctx.first_arrow_parameter_error = self.first_arrow_parameter_error;
      ctx.var_list.append(&mut self.var_list.clone());
    }
  }

  pub fn reset(&mut self) -> ArrowFunctionContext {
    return std::mem::replace(self, ArrowFunctionContext::new());
  }

  pub fn new_var(&mut self, v: Vec<u16>, pos: SourcePosition) {
    self.var_list.push((v, pos));
  }

  pub fn set_var_list(&mut self, v: &Vec<(Vec<u16>, SourcePosition)>) {
    self.var_list = v.clone();
  }
}

macro_rules! context_enum {
  ($name:ident { $($item:ident($type:ty),)* }) => {
    #[derive(Clone)]
    pub enum $name {
      $(
        $item($type),
      )*
    }

    impl std::fmt::Debug for $name {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}", match self {
          $(
            &$name::$item(ref n) => format!("{:?}", n),
          )*
        });
      }
    }

    impl $name {
      pub fn to_expr_ctx_mut(&mut self) -> &mut ExpressionContext {
        match self {
          &mut $name::Expr(ref mut ctx) => ctx,
          &mut $name::Arrow(ref mut ctx) => ctx.expression_context_mut()
        }
      }

      pub fn to_expr_ctx(&self) -> &ExpressionContext {
        match self {
          &$name::Expr(ref ctx) => ctx,
          &$name::Arrow(ref ctx) => ctx.expression_context()
        }
      }

      pub fn to_arrow_ctx_mut(&mut self) -> Result<&mut ArrowFunctionContext, ()> {
        match self {
          &mut $name::Expr(_) => Err(()),
          &mut $name::Arrow(ref mut ctx) => Ok(ctx)
        }
      }

      pub fn to_arrow_ctx_mut_unchecked(&mut self) -> &mut ArrowFunctionContext {
        return self.to_arrow_ctx_mut().unwrap();
      }

      pub fn to_arrow_ctx(&self) -> Result<&ArrowFunctionContext, ()> {
        match self {
          &$name::Expr(_) => Err(()),
          &$name::Arrow(ref ctx) => Ok(ctx)
        }
      }

      pub fn to_arrow_ctx_unchecked(&self) -> &ArrowFunctionContext {
        return self.to_arrow_ctx().unwrap();
      }

      pub fn propagate(&mut self, ec: &mut ExprCtx) {
        match self {
          $(
            &mut $name::$item(ref mut e) => e.propagate(ec),
          )*
        }
      }

      pub fn create_child_context(&self) -> ExpressionContext {
        match self {
          $(
            &$name::$item(ref e) => e.create_child_context(),
          )*
        }
      }

      pub fn create_child_arrow_context(&self) -> ArrowFunctionContext {
        match self {
          $(
            &$name::$item(ref e) => e.create_child_arrow_context(),
          )*
        }
      }

      pub fn reset(&mut self) {
        self.reset();
        if let Ok(a) = self.to_arrow_ctx_mut() {
          a.reset();
        }
      }
    }

    impl std::ops::Deref for $name {
      type Target = ExpressionContext;
      fn deref(&self) -> &Self::Target {
        return self.to_expr_ctx();
      }
    }

    impl std::ops::DerefMut for $name {
      fn deref_mut(&mut self) -> &mut Self::Target {
        return self.to_expr_ctx_mut();
      }
    }
  }
}

context_enum!(
  ExprCtx {
    Expr(ExpressionContext),
    Arrow(ArrowFunctionContext),
  }
);
impl From<ExpressionContext> for ExprCtx {
  fn from(e: ExpressionContext) -> ExprCtx {
    return ExprCtx::Expr(e);
  }
}
impl From<ArrowFunctionContext> for ExprCtx {
  fn from(e: ArrowFunctionContext) -> ExprCtx {
    return ExprCtx::Arrow(e);
  }
}

pub struct SyntaxErrorFactory {
  region: Region,
}

macro_rules! error_def {
  ($name:ident, $text:expr) => {
    pub fn $name<T>(&mut self, pos: SourcePosition) -> ParseResult<T> {
      return parse_error!(self.region, $text, &pos);
    }

    paste! {
      pub fn [<$name _raw>](&mut self, pos: SourcePosition) -> Exotic<ErrorDescriptor> {
        return parse_error!(@raw, self.region, $text, &pos);
      }
    }
  };
}
impl SyntaxErrorFactory {
  pub fn new(region: Region) -> Self {
    SyntaxErrorFactory { region: region.clone() }
  }
  error_def!(invalid_token_found, "Invalid token found");
  error_def!(
    template_literal_not_allowed_after_op_chain,
    "Template literal not allowed after optional chain"
  );
  error_def!(
    duplicate_parameter_not_allowed_here,
    "Duplicate parameter not allowed here"
  );
  error_def!(
    use_strict_after_non_simple_param,
    "Declaring 'use strict' is not allowed if function has non-simple parameter list"
  );
  error_def!(unexpected_token_found, "Unexpected token found");
  error_def!(
    invalid_left_hand_side_expression_found,
    "Invalid left hand side expression found"
  );
  error_def!(
    eval_or_arguments_in_strict_mode_code,
    "In strict mode code, eval or arguments not allowed here"
  );
  error_def!(invalid_update_expression, "Invalid update expression");
}

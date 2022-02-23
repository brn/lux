use super::ast::*;
use super::error_reporter::ErrorDescriptor;
use super::source_position::{RuntimeSourcePosition, SourcePosition};
use crate::utility::{Exotic, WeakRegion};

pub type ParseResult<T> = Result<T, Exotic<ErrorDescriptor>>;

bitflags! {
  pub struct ParserConstraints: u8 {
    const None = 0;
    const Template = 1;
    const Call = 2;
    const BindingPattern = 4;
    const Initializer = 8;
    const KeywordIdentifier = 16;
    const Let = 32;
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

  pub fn is_let_allowed(&self) -> bool {
    return self.contains(ParserConstraints::Let);
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
  first_pattern_in_decl_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_value_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_strict_mode_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_global_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_async_context_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_generator_context_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_generator_context_origin_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_for_in_of_lhs_error: Option<Exotic<ErrorDescriptor>>,

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

      pub fn [<clear_ $name>](&mut self) {
        self.[<$name>] = None;
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
      first_pattern_in_decl_error: None,
      first_value_error: None,
      first_strict_mode_error: None,
      first_async_context_error: None,
      first_generator_context_error: None,
      first_generator_context_origin_error: None,
      first_global_error: None,
      first_for_in_of_lhs_error: None,
      is_maybe_immediate_function: false,
      is_in_allowed: true,
      assignment_target_type: AssignmentTargetType::Simple,
    }
  }

  pub fn create_child_context(&self) -> ExpressionContext {
    let mut child = ExpressionContext::new();
    child.set_is_in_allowed(self.is_in_allowed);
    child.set_is_maybe_immediate_function(self.is_maybe_immediate_function());
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
    if self.first_pattern_in_decl_error.is_some() {
      ec.first_pattern_in_decl_error = self.first_pattern_in_decl_error.clone();
    }
    if self.first_value_error.is_some() {
      ec.first_value_error = self.first_value_error.clone();
    }
    if self.first_strict_mode_error.is_some() {
      ec.first_strict_mode_error = self.first_strict_mode_error.clone();
    }
    if self.first_async_context_error.is_some() {
      ec.first_async_context_error = self.first_async_context_error.clone();
    }
    if self.first_generator_context_error.is_some() {
      ec.first_generator_context_error = self.first_generator_context_error.clone();
    }
    if self.first_generator_context_origin_error.is_some() {
      ec.first_generator_context_origin_error = self.first_generator_context_origin_error.clone();
    }
    if self.first_global_error.is_some() {
      ec.first_global_error = self.first_global_error.clone();
    }
    if self.first_for_in_of_lhs_error.is_some() {
      ec.first_for_in_of_lhs_error = self.first_for_in_of_lhs_error.clone();
    }
    ec.assignment_target_type = self.assignment_target_type;
  }

  pub fn reset(&mut self) -> ExpressionContext {
    return std::mem::replace(self, ExpressionContext::new());
  }

  _get_set!(first_pattern_error);
  _get_set!(first_pattern_in_decl_error);
  _get_set!(first_value_error);
  _get_set!(first_strict_mode_error);
  _get_set!(first_async_context_error);
  _get_set!(first_global_error);
  _get_set!(first_generator_context_error);
  _get_set!(first_generator_context_origin_error);
  _get_set!(first_for_in_of_lhs_error);
}
impl std::fmt::Debug for ExpressionContext {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(
      f,
      "pattern_error = {} pattern_in_decl_error = {} value_errr = {} strict_mode_error = {}, imm_fn = {} in = {} assignment_target_type = {:?}",
      self.first_pattern_error.is_some(),
      self.first_pattern_in_decl_error.is_some(),
      self.first_value_error.is_some(),
      self.first_strict_mode_error.is_some(),
      self.is_maybe_immediate_function,
      self.is_in_allowed,
      self.assignment_target_type
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

  #[property(get(type = "ref"), set(disable))]
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
    return self.expression_context.create_child_context();
  }

  pub fn create_child_arrow_context(&self) -> ArrowFunctionContext {
    let mut child = ArrowFunctionContext::new();
    child.expression_context = self.create_child_context();
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

  pub fn propagate_arrow_error(&self, ec: &mut ExprCtx) {
    if let Ok(ctx) = ec.to_arrow_ctx_mut() {
      ctx.first_arrow_parameter_error = self.first_arrow_parameter_error;
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

  pub fn var_list_mut(&mut self) -> &mut Vec<(Vec<u16>, SourcePosition)> {
    return &mut self.var_list;
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
          &mut $name::Arrow(ref mut ctx) => ctx.expression_context_mut(),
        }
      }

      pub fn to_expr_ctx(&self) -> &ExpressionContext {
        match self {
          &$name::Expr(ref ctx) => ctx,
          &$name::Arrow(ref ctx) => ctx.expression_context(),
        }
      }

      pub fn to_arrow_ctx_mut(&mut self) -> Result<&mut ArrowFunctionContext, ()> {
        match self {
          &mut $name::Expr(_) => Err(()),
          &mut $name::Arrow(ref mut ctx) => Ok(ctx),
        }
      }

      pub fn to_arrow_ctx_mut_unchecked(&mut self) -> &mut ArrowFunctionContext {
        return self.to_arrow_ctx_mut().unwrap();
      }

      pub fn to_arrow_ctx(&self) -> Result<&ArrowFunctionContext, ()> {
        match self {
          &$name::Expr(_) => Err(()),
          &$name::Arrow(ref ctx) => Ok(ctx),
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
        if let Ok(a) = self.to_arrow_ctx_mut() {
          a.reset();
        } else {
          self.reset();
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

#[derive(Property)]
pub struct StatementBlock {
  #[property(get(type = "copy"), set(type = "ref"))]
  is_continuable: bool,
  #[property(get(type = "copy"), set(type = "ref"))]
  is_breakable: bool,
  #[property(get(type = "copy"), set(type = "ref"))]
  is_returnable: bool,
  #[property(get(type = "copy"), set(type = "ref"))]
  first_const_initializer_error: Option<Exotic<ErrorDescriptor>>,
  #[property(get(type = "copy"), set(type = "ref"))]
  first_labelled_function_error: Option<Exotic<ErrorDescriptor>>,
  #[property(get(type = "copy"), set(type = "ref"))]
  is_export: bool,
}
impl StatementBlock {
  pub fn new() -> Self {
    return StatementBlock {
      is_continuable: false,
      is_breakable: false,
      is_returnable: false,
      is_export: false,
      first_const_initializer_error: None,
      first_labelled_function_error: None,
    };
  }

  pub fn propagate(&self, b: &mut StatementBlock) {
    b.set_first_const_initializer_error(self.first_const_initializer_error);
    b.set_first_labelled_function_error(self.first_labelled_function_error);
  }

  pub fn with(is_continuable: bool, is_breakable: bool, is_returnable: bool, is_export: bool) -> Self {
    let mut i = Self::new();
    i.set_is_continuable(is_continuable);
    i.set_is_breakable(is_breakable);
    i.set_is_returnable(is_returnable);
    i.set_is_export(is_export);
    return i;
  }

  pub fn next_block(&self) -> Self {
    return Self::with(self.is_continuable, self.is_breakable, self.is_returnable, self.is_export);
  }
}

pub struct SyntaxErrorFactory {
  region: WeakRegion,
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
  pub fn new(region: WeakRegion) -> Self {
    SyntaxErrorFactory { region: region.clone() }
  }
  error_def!(invalid_token_found, "Invalid token found");
  error_def!(
    template_literal_not_allowed_after_op_chain,
    "Template literal not allowed after optional chain"
  );
  error_def!(duplicate_parameter_not_allowed_here, "Duplicate parameter not allowed here");
  error_def!(
    use_strict_after_non_simple_param,
    "Declaring 'use strict' is not allowed if function has non-simple parameter list"
  );
  error_def!(unexpected_token_found, "Unexpected token found");
  error_def!(invalid_left_hand_side_expression, "Invalid left hand side expression found");
  error_def!(
    eval_or_arguments_in_strict_mode_code,
    "In strict mode code, eval or arguments not allowed here"
  );
  error_def!(invalid_update_expression, "Invalid update expression");
}

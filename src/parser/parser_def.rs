use super::ast::*;
use super::error_reporter::ErrorDescriptor;
use super::scope::*;
use super::source_position::{RuntimeSourcePosition, SourcePosition};
use super::token::*;
use crate::utility::Exotic;

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

#[derive(Property, Clone)]
pub struct ExpressionContext {
  #[property(skip)]
  first_pattern_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_value_error: Option<Exotic<ErrorDescriptor>>,

  #[property(skip)]
  first_strict_mode_error: Option<Exotic<ErrorDescriptor>>,

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
    }
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

      pub fn to_arrow_ctx_unchecked(&mut self) -> &ArrowFunctionContext {
        return self.to_arrow_ctx().unwrap();
      }

      pub fn propagate(&mut self, ec: &mut ExprCtx) {
        match self {
          $(
            &mut $name::$item(ref mut e) => e.propagate(ec),
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

// pub trait ParserDef {
//   fn parse_directive_prologue(&mut self, scope: Exotic<Scope>);
//   fn parse_program(&mut self);
//   fn parse_terminator<T>(&mut self, expr: T) -> ParseResult<T>;
//   fn parse_identifier(&mut self, constraints: ParserConstraints) -> ParseResult<Expr>;
//   fn parse_identifier_reference(&mut self, constraints: ParserConstraints) -> ParseResult<Expr>;
//   fn parse_primary_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_literal(&mut self) -> ParseResult<Expr>;
//   fn parse_regular_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_array_literal(&mut self, constraints: ParserConstraints) -> ParseResult<Expr>;
//   fn parse_element_list(&mut self) -> ParseResult<Expr>;
//   fn parse_spread_element(&mut self) -> ParseResult<Expr>;
//   fn parse_object_literal(&mut self, constraints: ParserConstraints) -> ParseResult<Expr>;
//   fn parse_object_literal_property(&mut self, constraints: ParserConstraints) -> ParseResult<Expr>;
//   fn parse_property_name(&mut self) -> ParseResult<Expr>;
//   fn parse_template_literal(&mut self) -> ParseResult<Expr>;
//   fn parse_cover_parenthesized_expression_and_arrow_parameter_list(&mut self) -> ParseResult<Expr>;
//   fn parse_member_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_post_member_expression(
//     &mut self,
//     source_position: &RuntimeSourcePosition,
//     expr: Expr,
//     receiver_type: CallReceiverType,
//     constraints: ParserConstraints,
//     error_if_default: bool,
//   ) -> ParseResult<Expr>;
//   fn parse_new_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_super_call(&mut self) -> ParseResult<Expr>;
//   fn parse_import_call(&mut self) -> ParseResult<Expr>;
//   fn parse_arguments(&mut self) -> ParseResult<Expr>;
//   fn parse_arguments_list(&mut self) -> ParseResult<Expr>;
//   fn parse_left_hand_side_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_update_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_unary_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_binary_operator_by_priority(
//     &mut self,
//     prev_ast: Expr,
//     priority: OperatorPriority,
//     prev_priority: OperatorPriority,
//   ) -> ParseResult<Expr>;
//   fn parse_binary_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_conditional_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_assignment_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_assignment_expression_lhs(&mut self) -> ParseResult<Expr>;
//   fn parse_assignment_pattern(&mut self) -> ParseResult<Expr>;
//   fn parse_object_assignment_pattern(&mut self) -> ParseResult<Expr>;
//   fn parse_array_assignment_pattern(&mut self) -> ParseResult<Expr>;
//   fn parse_assignment_property_list(&mut self) -> ParseResult<Expr>;
//   fn parse_assignment_element_list(&mut self) -> ParseResult<Expr>;
//   fn parse_assignment_elision_element(&mut self) -> ParseResult<Expr>;
//   fn parse_assignment_property(&mut self) -> ParseResult<Expr>;
//   fn parse_assignment_element(&mut self) -> ParseResult<Expr>;
//   fn parse_assignment_rest_element(&mut self) -> ParseResult<Expr>;
//   fn parse_destructuring_assignment_target(&mut self) -> ParseResult<Expr>;
//   fn parse_expression(&mut self, should_return_exprs: bool) -> ParseResult<Expr>;
//   fn parse_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_declaration(&mut self) -> ParseResult<Stmt>;
//   fn parse_hoistable_declaration(&mut self) -> ParseResult<Stmt>;
//   fn parse_breakable_statement(&mut self) -> ParseResult<Expr>;
//   fn parse_block_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_block(&mut self) -> ParseResult<Stmt>;
//   fn parse_statement_list<T: Fn(Token) -> bool>(&mut self, a: T) -> ParseResult<Stmt>;
//   fn parse_statement_list_item(&mut self) -> ParseResult<Stmt>;
//   fn parse_lexical_declaration(&mut self) -> ParseResult<Stmt>;
//   fn parse_lexical_binding(&mut self) -> ParseResult<Expr>;
//   fn parse_variable_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_variable_declaration_list(&mut self) -> ParseResult<Stmt>;
//   fn parse_variable_declaration(&mut self) -> ParseResult<Stmt>;
//   fn parse_binding_pattern(&mut self) -> ParseResult<Expr>;
//   fn parse_binding_element(&mut self) -> ParseResult<Expr>;
//   fn parse_single_name_binding(&mut self, constraints: ParserConstraints) -> ParseResult<Expr>;
//   fn parse_binding_rest_element(&mut self) -> ParseResult<Expr>;
//   fn parse_expression_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_if_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_iteration_statement(&mut self) -> ParseResult<Expr>;
//   fn parse_for_declaration(&mut self) -> ParseResult<Stmt>;
//   fn parse_for_binding(&mut self) -> ParseResult<Expr>;
//   fn parse_continue_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_break_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_return_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_with_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_switch_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_case_block(&mut self) -> ParseResult<Expr>;
//   fn parse_case_clauses(&mut self) -> ParseResult<Expr>;
//   fn parse_case_clause(&mut self) -> ParseResult<Expr>;
//   fn parse_default_caluse(&mut self) -> ParseResult<Expr>;
//   fn parse_labelled_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_labelled_item(&mut self) -> ParseResult<Expr>;
//   fn parse_throw_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_try_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_catch(&mut self) -> ParseResult<Expr>;
//   fn parse_finally(&mut self) -> ParseResult<Expr>;
//   fn parse_catch_parameter(&mut self) -> ParseResult<Expr>;
//   fn parse_debugger_statement(&mut self) -> ParseResult<Stmt>;
//   fn parse_function_declaration(&mut self, is_async: bool, is_default: bool) -> ParseResult<Stmt>;
//   fn parse_function_expression(&mut self, is_async: bool, is_default: bool) -> ParseResult<Expr>;
//   fn parse_formal_parameters(&mut self) -> ParseResult<Expr>;
//   fn parse_formal_parameter_list(&mut self) -> ParseResult<Expr>;
//   fn parse_function_rest_parameter(&mut self) -> ParseResult<Expr>;
//   fn parse_function_body(&mut self) -> ParseResult<Stmt>;
//   fn parse_arrow_function(&mut self, is_async: bool) -> ParseResult<Expr>;
//   fn parse_arrow_parameter(&mut self) -> ParseResult<Expr>;
//   fn parse_concise_body(&mut self, is_async: bool, args: Expr) -> ParseResult<Expr>;
//   fn parse_method_definition(&mut self) -> ParseResult<Expr>;
//   fn parse_property_set_parameter_list(&mut self) -> ParseResult<Expr>;
//   fn parse_generator_method(&mut self) -> ParseResult<Expr>;
//   fn parse_generator_declaration(&mut self) -> ParseResult<Stmt>;
//   fn parse_generator_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_generator_body(&mut self) -> ParseResult<Expr>;
//   fn parse_yield_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_await_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_class_declaration(&mut self) -> ParseResult<Stmt>;
//   fn parse_class_expression(&mut self) -> ParseResult<Expr>;
//   fn parse_class_tail(&mut self) -> ParseResult<Expr>;
//   fn parse_class_heritage(&mut self) -> ParseResult<Expr>;
//   fn parse_class_body(&mut self) -> ParseResult<Expr>;
//   fn parse_class_element_list(&mut self) -> ParseResult<Expr>;
//   fn parse_class_element(&mut self) -> ParseResult<Expr>;
//   fn parse_script(&mut self) -> ParseResult<Ast>;
//   fn parse_module(&mut self) -> ParseResult<Ast>;
//   fn parse_module_body(&mut self) -> ParseResult<Stmt>;
//   fn parse_module_item(&mut self) -> ParseResult<Stmt>;
//   fn parse_import_declaration(&mut self) -> ParseResult<Stmt>;
//   fn parse_name_space_import(&mut self) -> ParseResult<Expr>;
//   fn parse_named_import(&mut self) -> ParseResult<Expr>;
//   fn parse_export_declaration(&mut self) -> ParseResult<Stmt>;
//   fn parse_export_clause(&mut self) -> ParseResult<Expr>;
//   fn parse_named_list(&mut self) -> ParseResult<Expr>;
// }

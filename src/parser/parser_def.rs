use super::ast::*;
use super::source_position::SourcePosition;

type ParseResult<T> = Result<T, &'static str>;

bitflags! {
  pub struct ParserConstraintsType: u8 {
    const Template = 1;
    const Call = 2;
    const BindingPattern = 4;
    const Initializer = 8;
  }
}

pub struct ParserConstraints {
  constraints_type: ParserConstraintsType,
}

#[derive(PartialEq, PartialOrd, Copy, Clone)]
pub enum OperatorPriority {
  None,
  LogicalOR,
  LogicalAND,
  BitwiseOR,
  BitwiseXOR,
  BitwiseAND,
  Equality,
  Relational,
  Shift,
  Additive,
  Multiplicative,
  Exponentiation,
}

impl ParserConstraints {
  pub fn new(constraints_type: ParserConstraintsType) -> Self {
    ParserConstraints { constraints_type }
  }

  pub fn allow_template(&mut self) {
    self.constraints_type |= ParserConstraintsType::Template;
  }

  pub fn is_template_allowed(&self) -> bool {
    return self.constraints_type.contains(ParserConstraintsType::Template);
  }

  pub fn allow_call(&mut self) {
    self.constraints_type |= ParserConstraintsType::Call;
  }

  pub fn is_call_allowed(&self) -> bool {
    return self.constraints_type.contains(ParserConstraintsType::Call);
  }

  pub fn allow_binding_pattern(&mut self) {
    self.constraints_type |= ParserConstraintsType::BindingPattern;
  }

  pub fn is_binding_pattern_allowed(&self) -> bool {
    return self.constraints_type.contains(ParserConstraintsType::BindingPattern);
  }

  pub fn allow_initializer(&mut self) {
    self.constraints_type |= ParserConstraintsType::Initializer;
  }

  pub fn is_initializer_allowed(&self) -> bool {
    return self.constraints_type.contains(ParserConstraintsType::Initializer);
  }
}

pub trait ParserDef {
  fn parser_directive_prologue(&mut self);
  fn parse_identifier(&mut self) -> ParseResult<Expr>;
  fn parse_primary_expression(&mut self) -> ParseResult<Expr>;
  fn parse_literal(&mut self) -> ParseResult<Expr>;
  fn parse_regular_expression(&mut self) -> ParseResult<Expr>;
  fn parse_array_literal(&mut self, constraints: ParserConstraints) -> ParseResult<Expr>;
  fn parse_element_list(&mut self) -> ParseResult<Expr>;
  fn parse_spread_element(&mut self) -> ParseResult<Expr>;
  fn parse_object_literal(&mut self, constraints: ParserConstraints) -> ParseResult<Expr>;
  fn parse_object_literal_property(&mut self) -> ParseResult<Expr>;
  fn parse_property_definition_list(&mut self) -> ParseResult<Expr>;
  fn parse_property_definition(&mut self) -> ParseResult<Expr>;
  fn parse_property_name(&mut self) -> ParseResult<Expr>;
  fn parse_cover_initialized_name(&mut self) -> ParseResult<Expr>;
  fn parse_initializer(&mut self) -> ParseResult<Expr>;
  fn parse_template_literal(&mut self) -> ParseResult<Expr>;
  fn parse_template_spans(&mut self) -> ParseResult<Expr>;
  fn parse_template_middle_list(&mut self) -> ParseResult<Expr>;
  fn parse_member_expression(&mut self) -> ParseResult<Expr>;
  fn parse_post_member_expression(
    &mut self,
    source_position: &SourcePosition,
    expr: Expr,
    receiver_type: CallReceiverType,
    constraints: ParserConstraints,
    error_if_default: bool,
  ) -> ParseResult<Expr>;
  fn parse_super_property(&mut self) -> ParseResult<Expr>;
  fn parse_new_target(&mut self) -> ParseResult<Expr>;
  fn parse_new_expression(&mut self) -> ParseResult<Expr>;
  fn parse_call_expression(&mut self) -> ParseResult<Expr>;
  fn parse_cover_call_expression_and_async_arrow_head(&mut self) -> ParseResult<Expr>;
  fn parse_call_member_expression(&mut self) -> ParseResult<Expr>;
  fn parse_super_call(&mut self) -> ParseResult<Expr>;
  fn parse_arguments(&mut self) -> ParseResult<Expr>;
  fn parse_arguments_list(&mut self) -> ParseResult<Expr>;
  fn parse_left_hand_side_expression(&mut self) -> ParseResult<Expr>;
  fn parse_update_expression(&mut self) -> ParseResult<Expr>;
  fn parse_unary_expression(&mut self) -> ParseResult<Expr>;
  fn parse_binary_operator_by_priority(&mut self, priority: OperatorPriority) -> ParseResult<Expr>;
  fn parse_binary_expression(&mut self) -> ParseResult<Expr>;
  fn parse_conditional_expression(&mut self) -> ParseResult<Expr>;
  fn parse_assignment_expression(&mut self) -> ParseResult<Expr>;
  fn parse_assignment_expression_lhs(&mut self) -> ParseResult<Expr>;
  fn parse_assignment_pattern(&mut self) -> ParseResult<Expr>;
  fn parse_object_assignment_pattern(&mut self) -> ParseResult<Expr>;
  fn parse_array_assignment_pattern(&mut self) -> ParseResult<Expr>;
  fn parse_assignment_property_list(&mut self) -> ParseResult<Expr>;
  fn parse_assignment_element_list(&mut self) -> ParseResult<Expr>;
  fn parse_assignment_elision_element(&mut self) -> ParseResult<Expr>;
  fn parse_assignment_property(&mut self) -> ParseResult<Expr>;
  fn parse_assignment_element(&mut self) -> ParseResult<Expr>;
  fn parse_assignment_rest_element(&mut self) -> ParseResult<Expr>;
  fn parse_destructuring_assignment_target(&mut self) -> ParseResult<Expr>;
  fn parse_expression(&mut self) -> ParseResult<Expr>;
  fn parse_statement(&mut self) -> ParseResult<Expr>;
  fn parse_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_hoistable_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_breakable_statement(&mut self) -> ParseResult<Expr>;
  fn parse_block_statement(&mut self) -> ParseResult<Expr>;
  fn parse_block(&mut self) -> ParseResult<Expr>;
  fn parse_statement_list(&mut self) -> ParseResult<Expr>;
  fn parse_statement_list_item(&mut self) -> ParseResult<Expr>;
  fn parse_lexical_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_lexical_binding(&mut self) -> ParseResult<Expr>;
  fn parse_variable_statement(&mut self) -> ParseResult<Expr>;
  fn parse_variable_declaration_list(&mut self) -> ParseResult<Expr>;
  fn parse_variable_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_binding_pattern(&mut self) -> ParseResult<Expr>;
  fn parse_binding_element(&mut self) -> ParseResult<Expr>;
  fn parse_single_name_binding(&mut self, constraints: ParserConstraints) -> ParseResult<Expr>;
  fn parse_binding_rest_element(&mut self) -> ParseResult<Expr>;
  fn parse_expression_statement(&mut self) -> ParseResult<Expr>;
  fn parse_if_statement(&mut self) -> ParseResult<Expr>;
  fn parse_iteration_statement(&mut self) -> ParseResult<Expr>;
  fn parse_for_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_for_binding(&mut self) -> ParseResult<Expr>;
  fn parse_continue_statement(&mut self) -> ParseResult<Expr>;
  fn parse_break_statement(&mut self) -> ParseResult<Expr>;
  fn parse_return_statement(&mut self) -> ParseResult<Expr>;
  fn parse_with_statement(&mut self) -> ParseResult<Expr>;
  fn parse_switch_statement(&mut self) -> ParseResult<Expr>;
  fn parse_case_block(&mut self) -> ParseResult<Expr>;
  fn parse_case_clauses(&mut self) -> ParseResult<Expr>;
  fn parse_case_clause(&mut self) -> ParseResult<Expr>;
  fn parse_default_caluse(&mut self) -> ParseResult<Expr>;
  fn parse_labelled_statement(&mut self) -> ParseResult<Expr>;
  fn parse_labelled_item(&mut self) -> ParseResult<Expr>;
  fn parse_throw_statement(&mut self) -> ParseResult<Expr>;
  fn parse_try_statement(&mut self) -> ParseResult<Expr>;
  fn parse_catch(&mut self) -> ParseResult<Expr>;
  fn parse_finally(&mut self) -> ParseResult<Expr>;
  fn parse_catch_parameter(&mut self) -> ParseResult<Expr>;
  fn parse_debugger_statement(&mut self) -> ParseResult<Expr>;
  fn parse_function_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_function_expression(&mut self) -> ParseResult<Expr>;
  fn parse_formal_parameters(&mut self) -> ParseResult<Expr>;
  fn parse_formal_parameter_list(&mut self) -> ParseResult<Expr>;
  fn parse_function_reste_parameter(&mut self) -> ParseResult<Expr>;
  fn parse_function_body(&mut self) -> ParseResult<Expr>;
  fn parse_arrow_function(&mut self) -> ParseResult<Expr>;
  fn parse_arrow_parameter(&mut self) -> ParseResult<Expr>;
  fn parse_async_concise_body(&mut self) -> ParseResult<Expr>;
  fn parse_async_arrow_head(&mut self) -> ParseResult<Expr>;
  fn parse_method_definition(&mut self) -> ParseResult<Expr>;
  fn parse_property_set_parameter_list(&mut self) -> ParseResult<Expr>;
  fn parse_generator_method(&mut self) -> ParseResult<Expr>;
  fn parse_generator_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_generator_expression(&mut self) -> ParseResult<Expr>;
  fn parse_generator_body(&mut self) -> ParseResult<Expr>;
  fn parse_yield_expression(&mut self) -> ParseResult<Expr>;
  fn parse_async_method(&mut self) -> ParseResult<Expr>;
  fn parse_async_function_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_async_function_expression(&mut self) -> ParseResult<Expr>;
  fn parse_async_function_body(&mut self) -> ParseResult<Expr>;
  fn parse_await_expression(&mut self) -> ParseResult<Expr>;
  fn parse_class_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_class_expression(&mut self) -> ParseResult<Expr>;
  fn parse_class_tail(&mut self) -> ParseResult<Expr>;
  fn parse_class_heritage(&mut self) -> ParseResult<Expr>;
  fn parse_class_body(&mut self) -> ParseResult<Expr>;
  fn parse_class_element_list(&mut self) -> ParseResult<Expr>;
  fn parse_class_element(&mut self) -> ParseResult<Expr>;
  fn parse_script(&mut self) -> ParseResult<Expr>;
  fn parse_module(&mut self) -> ParseResult<Expr>;
  fn parse_module_body(&mut self) -> ParseResult<Expr>;
  fn parse_module_item(&mut self) -> ParseResult<Expr>;
  fn parse_import_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_name_space_import(&mut self) -> ParseResult<Expr>;
  fn parse_named_import(&mut self) -> ParseResult<Expr>;
  fn parse_export_declaration(&mut self) -> ParseResult<Expr>;
  fn parse_export_clause(&mut self) -> ParseResult<Expr>;
  fn parse_named_list(&mut self) -> ParseResult<Expr>;
}

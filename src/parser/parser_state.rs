use std::vec::Vec;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum ParserState {
  InTemplateLiteral = 0,
  InTemplateInterpolation,
  RegexpExpected,
  InFunction,
  InAsyncFunction,
  InGeneratorFunction,
  InAsyncGeneratorFunction,
  _None,
  _Sentinel,
}

pub struct ParserStateStack {
  pub val: Vec<u32>,
  stack: Vec<ParserState>,
  state_count: Vec<u32>,
}

impl ParserStateStack {
  pub fn new() -> ParserStateStack {
    let mut state_count = vec![0; ParserState::_Sentinel as usize];
    return ParserStateStack {
      val: vec![0; 10],
      stack: Vec::<ParserState>::new(),
      state_count,
    };
  }

  pub fn push_state(&mut self, state: ParserState) {
    self.stack.push(state);
    self.state_count[state as usize] += 1;
  }

  pub fn pop_state(&mut self, state: ParserState) -> bool {
    if let Some(s) = self.stack.pop() {
      if s == state {
        self.state_count[state as usize] -= 1;
        return true;
      }
    }
    return false;
  }

  pub fn is_in_state(&self, state: ParserState) -> bool {
    return self.state_count[state as usize] > 0;
  }

  pub fn is_in_states(&self, state: &[ParserState]) -> bool {
    for s in state.iter() {
      if self.state_count[(*s) as usize] > 0 {
        return true;
      }
    }
    return false;
  }

  pub fn match_state(&self, state: ParserState) -> bool {
    return if let Some(s) = self.stack.last() {
      *s == state
    } else {
      false
    };
  }

  pub fn match_states(&self, state: &[ParserState]) -> bool {
    for s in state.iter() {
      if self.state_count[(*s) as usize] > 0 {
        return true;
      }
    }
    return false;
  }

  pub fn cur_state(&self) -> Option<&ParserState> {
    return self.stack.last();
  }
}

impl std::fmt::Debug for ParserStateStack {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    return write!(f, "{:?} {:?}", self.stack, self.state_count);
  }
}

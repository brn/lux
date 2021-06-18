use std::vec::Vec;

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum ParserState {
  InTemplateLiteral,
  RegexpExpected,
  _Sentinel,
}

pub struct ParserStateStack {
  stack: Vec<ParserState>,
  state_count: Vec<u32>,
}

impl ParserStateStack {
  pub fn new() -> ParserStateStack {
    let mut state_count = vec![0; ParserState::_Sentinel as usize];
    return ParserStateStack {
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
}

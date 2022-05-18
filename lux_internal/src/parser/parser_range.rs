#[derive(Default, Clone, Copy, Debug, Property)]
pub struct ParserRange {
  #[property(get(type = "copy"), set(disable))]
  begin: u32,

  #[property(get(type = "copy"), set(disable))]
  end: u32,
}
impl ParserRange {
  pub fn new(begin: u32, end: u32) -> Self {
    return ParserRange { begin, end };
  }
}

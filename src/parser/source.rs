use crate::context::ObjectRecordsInitializedContext;
use crate::structs::FixedU16CodePointArray;
use property::Property;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Property)]
pub struct SourceLayout {
  #[property(get(type = "copy"))]
  source_code: FixedU16CodePointArray,

  #[property(get(type = "ref"))]
  filename: String,
}

#[derive(Clone)]
pub struct Source(Rc<SourceLayout>);

impl Source {
  pub fn new(context: impl ObjectRecordsInitializedContext, filename: &str, source_code: &str) -> Self {
    return Source(Rc::new(SourceLayout {
      source_code: FixedU16CodePointArray::from_utf8(context, source_code),
      filename: filename.to_string(),
    }));
  }

  pub fn len(&self) -> usize {
    return self.source_code.len();
  }
}

impl Deref for Source {
  type Target = SourceLayout;
  fn deref(&self) -> &Self::Target {
    return &self.0;
  }
}

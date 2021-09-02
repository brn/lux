use crate::context::ObjectRecordsInitializedContext;
use crate::structs::FixedU16CodePointArray;
use property::Property;

#[derive(Property, Clone)]
pub struct Source {
  #[property(get(type = "copy"))]
  source_code: FixedU16CodePointArray,

  #[property(get(type = "ref"))]
  filename: String,
}

impl Source {
  pub fn new(context: impl ObjectRecordsInitializedContext, filename: &str, source_code: &str) -> Self {
    return Source {
      source_code: FixedU16CodePointArray::from_utf8(context, source_code),
      filename: filename.to_string(),
    };
  }

  pub fn len(&self) -> usize {
    return self.source_code.len();
  }
}

use crate::parser::SourcePosition;

#[cfg(test)]
#[inline]
pub fn join(pos: u32, v: &Vec<&str>) -> String {
  let mut a = String::new();
  let mut i = 0_u32;

  for str in v.iter() {
    a.push_str(str);
    a.push_str("\n");
    if i == pos {
      let start = (*str).chars().enumerate().skip_while(|(i, ch)| *ch == ' ').take(1).next();
      let last = (*str).chars().rev().enumerate().skip_while(|(_, ch)| *ch == ' ').next();
      if start.is_some() && last.is_some() {
        let start_index = start.unwrap().0;
        let end_index = (*str).len() - last.unwrap().0;
        a.push_str(&(" ".repeat(start_index)));
        a.push_str(&("^".repeat(end_index - start_index)));
        a.push_str("\n");
      }
    }
    i += 1_u32;
  }
  if i == pos {
    let s = format!("\n {}", "^".repeat(v.last().unwrap().len()));
    a.push_str(&s);
  }
  return a.get(0..a.len() - 1).unwrap().to_string();
}

#[cfg(test)]
#[inline]
pub fn compare_node(code: &str, value: &str, expected: &str) -> Result<(), String> {
  let mut v = value.split('\n').collect::<Vec<_>>();
  let mut e = expected.split('\n').collect::<Vec<_>>();
  let line_number = e.len();
  let mut v_it = v.iter().cloned();
  let mut e_it = e.iter().cloned();
  let mut index = 0_u32;

  loop {
    let vn = v_it.next();
    let en = e_it.next();
    if let Some(a) = vn {
      if let Some(b) = en {
        if a != b {
          let em = format!(
            "Expectation is not match to the result\nat line {} \nvalue: \n{}\nexpected: \n{}\nCode: {}\n\n",
            index + 1,
            join(index, &mut v),
            join(index, &mut e),
            code
          );
          return Err(em);
        }
      }
    }
    index += 1_u32;

    if vn.is_none() {
      if en.is_some() {
        let em = format!(
          "Expectation is longer than result\nat line {} \nvalue: \n{}\nexpected: \n{}\nCode: {}\n\n",
          index + 1,
          join(index, &mut v),
          join(index, &mut e),
          code
        );
        return Err(em);
      }
      break;
    } else if en.is_none() {
      if vn.is_some() {
        let em = format!(
          "Expectation is shorter than result\nat line {} \nvalue: \n{}\nexpected: \n{}\nCode: {}\n\n",
          index + 1,
          join(index, &mut v),
          join(index, &mut e),
          code
        );
        return Err(em);
      }
      break;
    }
  }

  return Ok(());
}

#[cfg(test)]
pub fn compare_position(actual: &SourcePosition, expected: &SourcePosition) -> Result<(), String> {
  if actual.start_col() != expected.start_col() {
    return Err(format!(
      "start_col => actual: {} expected: {}\nactual: {:?}\nexpected: {:?}",
      actual.start_col(),
      expected.start_col(),
      actual,
      expected
    ));
  }

  if actual.end_col() != expected.end_col() {
    return Err(format!(
      "end_col => actual: {} expected: {}\nactual: {:?}\nexpected: {:?}",
      actual.end_col(),
      expected.end_col(),
      actual,
      expected
    ));
  }

  if actual.start_line_number() != expected.start_line_number() {
    return Err(format!(
      "start_line_number => actual: {} expected: {}\nactual: {:?}\nexpected: {:?}",
      actual.start_line_number(),
      expected.start_line_number(),
      actual,
      expected
    ));
  }

  if actual.end_line_number() != expected.end_line_number() {
    return Err(format!(
      "end_line_number => actual: {} expected: {}\nactual: {:?}\nexpected: {:?}",
      actual.end_line_number(),
      expected.end_line_number(),
      actual,
      expected
    ));
  }

  return Ok(());
}

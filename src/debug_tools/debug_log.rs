#[cfg(feature = "debug")]
macro_rules! debug_log {
  ($format:expr, $($values:expr),+$(,)?) => { {
    let bt = backtrace::Backtrace::new();
    println!($format, $($values),+);
    println!("{:?}", bt);
  } };
  ($format:expr) => { {
    let bt = backtrace::Backtrace::new();
    println!($format);
    println!("{:?}", bt);
  } };
}

#[cfg(not(feature = "debug"))]
macro_rules! debug_log {
  ($format:expr, $($values:expr),+$(,)?) => {{}};
  ($format:expr) => {{}};
}

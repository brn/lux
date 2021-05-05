macro_rules! kb {
  ($val:expr) => {
    $val * 1024
  };
}

macro_rules! mb {
  ($val:expr) => {
    kb!($val) * 1024
  };
}

macro_rules! gb {
  ($val:expr) => {
    mb!($val) * 1024
  };
}

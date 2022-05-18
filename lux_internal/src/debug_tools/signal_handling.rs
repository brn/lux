use signal_hook::{consts::SIGSEGV, iterator::Signals};
use std::{error::Error, thread, time::Duration};

pub fn handle_sigsegv() -> Result<(), Box<dyn Error>> {
  let mut signals = Signals::new(&[SIGSEGV])?;

  thread::spawn(move || {
    for sig in signals.forever() {
      debug_log!("Sigsegv catched");
    }
  });

  // Following code does the actual work, and can be interrupted by pressing
  // Ctrl-C. As an example: Let's wait a few seconds.
  thread::sleep(Duration::from_secs(2));

  Ok(())
}

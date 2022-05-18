use vm_gen::builtins::ReprGen;
use vm_gen::vm_gen_context::VMGenContext;

pub fn main() {
  let c = VMGenContext::new("lux_vm");
  ReprGen::generate(c);
  c.flush();
  c.dispose();
}

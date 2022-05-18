use super::vm_gen_context::VMGenContext;
use inkwell::module::Linkage;
use inkwell::values::{BasicValueEnum, CallableValue, FunctionValue};
use inkwell::{FloatPredicate, IntPredicate};
use lux_internal::structs::{HEAP_OBJECT_MASK, NAN_BIT};
use std::collections::HashMap;

pub struct ReprGen<'ctx> {
  context: VMGenContext<'ctx>,
  function_map: HashMap<&'static str, FunctionValue<'ctx>>,
}

impl<'ctx> ReprGen<'ctx> {
  pub fn generate(context: VMGenContext<'ctx>) {
    let mut rg = ReprGen {
      context,
      function_map: HashMap::<&'static str, FunctionValue<'ctx>>::new(),
    };
    rg.generate_internal();
  }

  fn generate_internal(&mut self) {
    self.is_invalid_gen();
    self.is_heap_object_gen();
  }

  fn is_heap_object_gen(&mut self) {
    let module = self.context.module();
    let builder = self.context.builder();
    let fn_type = self
      .context
      .types()
      .bool()
      .fn_type(&[self.context.ptr_types().i8_generic_ptr().into()], false);
    let is_heap_object = module.add_function("repr__is_heap_object", fn_type, Some(Linkage::Private));
    let ebv = self.context.context().append_basic_block(is_heap_object, "entry");
    builder.position_at_end(ebv);
    let fn_value = self.function_map.get("repr__is_invalid").cloned().unwrap();
    let return_value = builder.build_call(
      Into::<CallableValue<'ctx>>::into(fn_value),
      &[is_heap_object.get_nth_param(0).unwrap().into()],
      "call",
    );
    let i64_val = builder.build_ptr_to_int(
      is_heap_object.get_nth_param(0).unwrap().into_pointer_value(),
      self.context.types().i64(),
      "[is_ptr] to f64",
    );
    let constant_mask = self.context.types().i64().const_int(HEAP_OBJECT_MASK, false);
    let heap_object_mask = builder.build_and(i64_val, constant_mask, "[is_heap_object] and_heap_object_mask");
    let has_heap_object_bit = builder.build_int_compare(
      IntPredicate::EQ,
      heap_object_mask,
      constant_mask,
      "[is_heap_object] compare heap_object_mask and values",
    );
    let result = builder.build_and(
      return_value.try_as_basic_value().left().unwrap().into_int_value(),
      has_heap_object_bit,
      "[is_heap_object] final comparison",
    );
    builder.build_return(Some(&result));
  }

  fn is_invalid_gen(&mut self) {
    let module = self.context.module();
    let builder = self.context.builder();
    let fn_type = self
      .context
      .types()
      .bool()
      .fn_type(&[self.context.ptr_types().i8_generic_ptr().into()], false);
    let is_invalid_fn = module.add_function("repr__is_invalid", fn_type, Some(Linkage::Private));
    let ebv = self.context.context().append_basic_block(is_invalid_fn, "entry");
    builder.position_at_end(ebv);
    let i64_val = builder.build_ptr_to_int(
      is_invalid_fn.get_nth_param(0).unwrap().into_pointer_value(),
      self.context.types().i64(),
      "[is_ptr] to f64",
    );
    self.function_map.insert("repr__is_invalid", is_invalid_fn);
    let float_val = builder
      .build_bitcast(i64_val, self.context.types().f64(), "[is_ptr] u64tof64")
      .into_float_value();
    let is_nan = builder.build_float_compare(
      FloatPredicate::OEQ,
      float_val,
      self
        .context
        .types()
        .f64()
        .const_float(unsafe { std::mem::transmute::<u64, f64>(NAN_BIT) }),
      "[is_ptr] compare to NAN_BIT",
    );
    builder.build_return(Some(&is_nan));
  }
}

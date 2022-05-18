use core::arch::global_asm;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{FloatType, IntType, PointerType, VoidType};
use inkwell::OptimizationLevel;
use lux_internal::utility::Exotic;
use property::Property;
use std::path::{Path, PathBuf};

#[derive(Property)]
pub struct VMGenTypes<'ctx> {
  #[property(get(type = "copy"), set(type = "ref"))]
  i32: IntType<'ctx>,
  #[property(get(type = "copy"), set(type = "ref"))]
  i16: IntType<'ctx>,
  #[property(get(type = "copy"), set(type = "ref"))]
  i8: IntType<'ctx>,
  #[property(get(type = "copy"), set(type = "ref"))]
  i64: IntType<'ctx>,
  #[property(get(type = "copy"), set(type = "ref"))]
  i128: IntType<'ctx>,
  #[property(get(type = "copy"), set(type = "ref"))]
  f32: FloatType<'ctx>,
  #[property(get(type = "copy"), set(type = "ref"))]
  f64: FloatType<'ctx>,
  #[property(get(type = "copy"), set(type = "ref"))]
  f128: FloatType<'ctx>,
  #[property(get(type = "copy"), set(type = "ref"))]
  void: VoidType<'ctx>,
  #[property(get(type = "copy"), set(type = "ref"))]
  bool: IntType<'ctx>,
}

#[derive(Property)]
pub struct VMGenPointerTypes<'ctx> {
  #[property(get(type = "copy"), set(type = "ref"))]
  i8_generic_ptr: PointerType<'ctx>,
}

#[derive(Property)]
pub struct VMGenContextLayout<'ctx> {
  #[property(get(type = "copy"), set(type = "ref"))]
  module: Exotic<Module<'ctx>>,
  #[property(get(type = "copy"), set(type = "ref"))]
  builder: Exotic<Builder<'ctx>>,
  #[property(get(disable), set(disable))]
  context: *mut Context,
  #[property(get(type = "copy"), set(type = "ref"))]
  types: Exotic<VMGenTypes<'ctx>>,
  #[property(get(type = "copy"), set(type = "ref"))]
  ptr_types: Exotic<VMGenPointerTypes<'ctx>>,
}
impl<'ctx> VMGenContextLayout<'ctx> {
  pub fn new(name_of_module: &str) -> Exotic<Self> {
    let context = Box::leak(Box::new(Context::create()));
    let mut layout = Exotic::from(Box::new(VMGenContextLayout {
      context,
      types: Exotic::new(std::ptr::null_mut()),
      module: Exotic::new(std::ptr::null_mut()),
      builder: Exotic::new(std::ptr::null_mut()),
      ptr_types: Exotic::new(std::ptr::null_mut()),
    }));

    let context_ref = unsafe { &*context };
    layout.module = Exotic::from(Box::new(context_ref.create_module(name_of_module)));
    layout.builder = Exotic::from(Box::new(context_ref.create_builder()));
    layout.types = Exotic::from(Box::new(VMGenTypes {
      i32: context_ref.i32_type(),
      i16: context_ref.i16_type(),
      i8: context_ref.i8_type(),
      i64: context_ref.i64_type(),
      i128: context_ref.i128_type(),
      f32: context_ref.f32_type(),
      f64: context_ref.f64_type(),
      f128: context_ref.f128_type(),
      void: context_ref.void_type(),
      bool: context_ref.bool_type(),
    }));
    layout.ptr_types = Exotic::from(Box::new(VMGenPointerTypes {
      i8_generic_ptr: layout.types().i8().ptr_type(inkwell::AddressSpace::Generic),
    }));

    return layout;
  }

  pub fn context(&self) -> &Context {
    return unsafe { &*self.context };
  }

  pub fn context_mut(&mut self) -> &mut Context {
    return unsafe { &mut *self.context };
  }
}
impl<'ctx> Drop for VMGenContextLayout<'ctx> {
  fn drop(&mut self) {
    let _ = self.builder.to_box();
    let _ = self.module.to_box();
    let _ = self.types.to_box();
    let _ = self.ptr_types.to_box();
  }
}

#[derive(Clone, Copy)]
pub struct VMGenContext<'ctx>(Exotic<VMGenContextLayout<'ctx>>);

impl<'ctx> VMGenContext<'ctx> {
  pub fn new(name_of_module: &str) -> Self {
    VMGenContext(VMGenContextLayout::new(name_of_module))
  }

  pub fn flush(&self) -> Result<(), String> {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("bitcode");
    println!("{:?}", d);
    self.module.write_bitcode_to_path(&d);
    match Target::initialize_native(&InitializationConfig::default()) {
      Ok(_) => {
        if let Some(target) = Target::get_first() {
          let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
          d.push("vm_asm");
          if let Some(target_machine) = target.create_target_machine(
            &TargetMachine::get_default_triple(),
            &TargetMachine::get_host_cpu_name().to_string(),
            &TargetMachine::get_host_cpu_features().to_string(),
            OptimizationLevel::Aggressive,
            RelocMode::Default,
            CodeModel::JITDefault,
          ) {
            target_machine.write_to_file(&*self.module(), FileType::Assembly, &d);
            return Ok(());
          }
          return Err("Failed to create target machine".to_string());
        }
        return Err("Target not initialized".to_string());
      }
      Err(e) => {
        return Err(e);
      }
    };
  }

  pub fn dispose(&self) {
    let _ = self.0.to_box();
  }

  pub fn run(&mut self) {
    // let bytes = include_bytes!("../../bitcode");
    // let memory_buf = MemoryBuffer::create_from_memory_range(bytes, "bitcode");
    // if let Ok(module) = Module::parse_bitcode_from_buffer(&memory_buf, &self.context) {
    //   // JIT実行エンジンを作成し、main関数を実行
    //   let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Aggressive).unwrap();
    //   unsafe {
    //     execution_engine.get_function::<unsafe extern "C" fn()>("main").unwrap().call();
    //   }
    //   return;
    // }

    // let module = self.context.create_module("main");
    // // builderを作成
    // let builder = self.context.create_builder();

    // // 型関係の変数
    // let i32_type = self.context.i32_type();
    // let i8_type = self.context.i8_type();
    // let void_type = self.context.void_type();
    // let i8_ptr_type = i8_type.ptr_type(inkwell::AddressSpace::Generic);

    // // printf関数を宣言
    // let printf_fn_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
    // let printf_function = module.add_function("printf", printf_fn_type, None);

    // let rust_fn_type = void_type.fn_type(&[i8_ptr_type.into()], false);
    // let rust_function = module.add_function("rust_function", rust_fn_type, None);

    // // main関数を宣言
    // let main_fn_type = i32_type.fn_type(&[], false);
    // let main_function = module.add_function("test_print", main_fn_type, None);

    // // main関数にBasic Blockを追加
    // let entry_basic_block = self.context.append_basic_block(main_function, "entry");
    // // builderのpositionをentry Basic Blockに設定
    // builder.position_at_end(entry_basic_block);

    // // ここからmain関数に命令をビルドしていく
    // // globalに文字列を宣言
    // let hw_string_ptr = builder.build_global_string_ptr("Hello, world!", "hw");
    // // printfをcall
    // builder.build_call(rust_function, &[hw_string_ptr.as_pointer_value().into()], "call");
    // // main関数は0を返す
    // builder.build_return(Some(&i32_type.const_int(0, false)));

    // JIT実行エンジンを作成し、main関数を実行
    //    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Aggressive).unwrap();
    // unsafe {
    //   execution_engine.get_function::<unsafe extern "C" fn()>("main").unwrap().call();
    // }
  }
}

impl<'ctx> std::ops::Deref for VMGenContext<'ctx> {
  type Target = VMGenContextLayout<'ctx>;
  fn deref(&self) -> &Self::Target {
    return &*self.0;
  }
}

impl<'ctx> std::ops::DerefMut for VMGenContext<'ctx> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    return &mut *self.0;
  }
}

use core::arch::global_asm;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::OptimizationLevel;
use std::path::{Path, PathBuf};

pub struct VM {
  context: Context,
}

impl VM {
  pub fn new() -> Self {
    VM {
      context: Context::create(),
    }
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

    let module = self.context.create_module("main");
    // builderを作成
    let builder = self.context.create_builder();

    // 型関係の変数
    let i32_type = self.context.i32_type();
    let i8_type = self.context.i8_type();
    let i8_ptr_type = i8_type.ptr_type(inkwell::AddressSpace::Generic);

    // printf関数を宣言
    let printf_fn_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
    let printf_function = module.add_function("printf", printf_fn_type, None);

    // main関数を宣言
    let main_fn_type = i32_type.fn_type(&[], false);
    let main_function = module.add_function("test_print", main_fn_type, None);

    // main関数にBasic Blockを追加
    let entry_basic_block = self.context.append_basic_block(main_function, "entry");
    // builderのpositionをentry Basic Blockに設定
    builder.position_at_end(entry_basic_block);

    // ここからmain関数に命令をビルドしていく
    // globalに文字列を宣言
    let hw_string_ptr = builder.build_global_string_ptr("Hello, world!", "hw");
    // printfをcall
    builder.build_call(printf_function, &[hw_string_ptr.as_pointer_value().into()], "call");
    // main関数は0を返す
    builder.build_return(Some(&i32_type.const_int(0, false)));

    if let Ok(_) = Target::initialize_native(&InitializationConfig::default()) {
      if let Some(target) = Target::get_first() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("bitcode");
        if let Some(target_machine) = target.create_target_machine(
          &TargetMachine::get_default_triple(),
          &TargetMachine::get_host_cpu_name().to_string(),
          &TargetMachine::get_host_cpu_features().to_string(),
          OptimizationLevel::Aggressive,
          RelocMode::Default,
          CodeModel::JITDefault,
        ) {
          println!("{:?}", target);
          target_machine.write_to_file(&module, FileType::Assembly, &d);
        }
      }
    }

    // JIT実行エンジンを作成し、main関数を実行
    //    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Aggressive).unwrap();
    // unsafe {
    //   execution_engine.get_function::<unsafe extern "C" fn()>("main").unwrap().call();
    // }
  }
}

global_asm!(include_str!("../../bitcode"));
extern "C" {
  fn test_print();
}

#[test]
fn test_run_inkwell() {
  let mut vm = VM::new();
  vm.run();
  unsafe { test_print() };
}

mod abi;
mod binary;
mod codegen;
mod conditional;
mod func;
mod literal;
mod panic;
mod runtime_check;
mod ty;
mod unary;
mod util;

use chili_ast::{ast, workspace::Workspace};
use chili_infer::ty_ctx::TyCtx;
use codegen::Codegen;
use common::{
    build_options::BuildOptions,
    target::{Arch, TargetMetrics},
    time,
};
use execute::Execute;
use inkwell::{
    context::Context,
    module::Module,
    passes::PassManager,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    values::FunctionValue,
    OptimizationLevel,
};
use std::{
    collections::{HashMap, HashSet},
    path::Path,
    process::Command,
};
use ustr::UstrMap;

pub fn codegen<'w>(workspace: &Workspace, tycx: &TyCtx, ast: &ast::TypedAst) {
    let context = Context::create();
    let module = context.create_module(
        workspace
            .build_options
            .source_path()
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap(),
    );
    let builder = context.create_builder();

    let target_metrics = workspace.build_options.target_platform.metrics();

    let target_machine = get_target_machine(&target_metrics);
    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module.set_triple(&target_machine.get_triple());

    let fpm = PassManager::create(&module);
    init_pass_manager(&fpm);

    let mut cg = Codegen {
        workspace,
        tycx,
        ast,
        target_metrics,
        context: &context,
        module: &module,
        fpm: &fpm,
        builder: &builder,
        ptr_sized_int_type: context.ptr_sized_int_type(&target_machine.get_target_data(), None),
        global_decls: HashMap::default(),
        types: HashMap::default(),
        static_strs: UstrMap::default(),
        fn_types: HashMap::new(),
    };

    time! { workspace.build_options.verbose, "llvm",
        cg.start()
    };

    if workspace.build_options.emit_llvm_ir {
        dump_ir(&module, workspace.build_options.source_path());
    }

    let executable_path = build_executable(
        &workspace.build_options,
        &target_machine,
        &module,
        &workspace.foreign_libraries,
    );

    if workspace.build_options.run {
        Command::new(executable_path).spawn().unwrap();
    }
}

fn init_pass_manager<'a>(fpm: &PassManager<FunctionValue<'a>>) {
    // fpm.add_instruction_combining_pass();
    // fpm.add_reassociate_pass();
    // fpm.add_cfg_simplification_pass();
    // fpm.add_basic_alias_analysis_pass();
    // fpm.add_promote_memory_to_register_pass();
    // fpm.add_scalar_repl_aggregates_pass_ssa();
    // fpm.add_reassociate_pass();
    // fpm.add_tail_call_elimination_pass();
    // fpm.add_gvn_pass();
    // fpm.add_loop_deletion_pass();
    // fpm.add_loop_rotate_pass();
    // fpm.add_loop_vectorize_pass();

    // fpm.add_promote_memory_to_register_pass();
    // fpm.add_global_dce_pass();
    // fpm.add_tail_call_elimination_pass();
    // fpm.add_ipsccp_pass();
    // fpm.add_correlated_value_propagation_pass();
    // fpm.add_global_optimizer_pass();
    // fpm.add_cfg_simplification_pass();
    // fpm.add_licm_pass();
    // fpm.add_loop_unswitch_pass();
    // fpm.add_loop_idiom_pass();
    // fpm.add_loop_deletion_pass();
    // fpm.add_memcpy_optimize_pass();
    // fpm.add_sccp_pass();
    // fpm.add_aggressive_dce_pass();
    // fpm.add_function_inlining_pass();
    // fpm.add_loop_rotate_pass();
    // fpm.add_loop_vectorize_pass();
    // fpm.add_instruction_combining_pass();
    // fpm.add_jump_threading_pass();
    // fpm.add_reassociate_pass();
    // fpm.add_gvn_pass();

    fpm.initialize();
}

fn get_target_machine(target_metrics: &TargetMetrics) -> TargetMachine {
    match &target_metrics.arch {
        Arch::Amd64 | Arch::_386 => Target::initialize_x86(&InitializationConfig::default()),
        Arch::Arm64 => Target::initialize_aarch64(&InitializationConfig::default()),
        Arch::Wasm32 | Arch::Wasm64 => {
            Target::initialize_webassembly(&InitializationConfig::default())
        }
    }

    let triple = TargetTriple::create(target_metrics.target_triplet);
    let target = Target::from_triple(&triple).unwrap();
    let host_cpu = TargetMachine::get_host_cpu_name();
    let features = TargetMachine::get_host_cpu_features();

    target
        .create_target_machine(
            &triple,
            host_cpu.to_str().unwrap(),
            features.to_str().unwrap(),
            OptimizationLevel::Default, // TODO: get optimization level from build options
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap()
}

fn dump_ir(module: &Module, path: &Path) {
    module.print_to_file(path.with_extension("ll")).unwrap();
}

fn build_executable(
    build_options: &BuildOptions,
    target_machine: &TargetMachine,
    module: &Module,
    foreign_libraries: &HashSet<ast::ForeignLibrary>,
) -> String {
    let mut lib_paths = vec![];
    let mut libs = vec![];

    for lib in foreign_libraries.iter() {
        match lib {
            ast::ForeignLibrary::System(lib_name) => {
                if lib_name != "c.lib" {
                    libs.push(lib_name)
                }
            }
            ast::ForeignLibrary::Path {
                lib_dir,
                lib_path: _,
                lib_name,
            } => {
                lib_paths.push(lib_dir);
                libs.push(lib_name);
            }
        }
    }

    let source_path = build_options.source_path();

    let object_file = source_path.with_extension("obj");
    let executable_file = source_path.with_extension("exe");

    time! { build_options.verbose, "write obj",
        target_machine
            .write_to_file(&module, FileType::Object, &object_file)
            .unwrap()
    };

    time! { build_options.verbose, "link",
        Command::new("lld-link")
            .arg(format!("/out:{}", executable_file.to_str().unwrap()))
            .arg("/entry:mainCRTStartup")
            .arg("/defaultlib:libcmt")
            // .arg("/defaultlib:oldnames")
            // .arg("/nodefaultlib")
            .arg("/nologo")
            .arg("/incremental:no")
            .arg("/opt:ref")
            .arg("/threads:8")
            .arg("/subsystem:CONSOLE")
            .arg("/machine:x64")
            .arg("/libpath:C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\Llvm\\x64\\lib\\clang\\12.0.0\\lib\\windows")
            .arg("/libpath:C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.29.30133\\lib\\x64")
            .arg("/libpath:C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.19041.0\\um\\x64")
            .arg("/libpath:C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.19041.0\\ucrt\\x64")
            .args(lib_paths.iter().map(|path| format!("/libpath:{}", path)))
            .arg(object_file.to_str().unwrap())
            .args(libs)
            .execute_output()
            .unwrap()
    };

    executable_file.to_str().unwrap().to_string()
}

#[allow(dead_code)]
#[repr(u32)]
pub enum CallingConv {
    C = 0,
    Fast = 8,
    Cold = 9,
    GHC = 10,
    HiPE = 11,
    WebKitJs = 12,
    AnyReg = 13,
    PreserveMost = 14,
    PreserveAll = 15,
    Swift = 16,
    CxxFastTls = 17,
    Tail = 18,
    CfguardCheck = 19,
    SwiftTail = 20,
    X86StdCall = 64,
    X86FastCall = 65,
    ArmApcs = 66,
    ArmAapcs = 67,
    ArmAapcsVfp = 68,
    Msp430Intr = 69,
    X86ThisCall = 70,
    PtxKernel = 71,
    PtxDevice = 72,
    SpirFunc = 75,
    SpirKernel = 76,
    IntelOclBi = 77,
    X86_64SysV = 78,
    Win64 = 79,
    X86VectorCall = 80,
    HHVM = 81,
    HhvmC = 82,
    X86Intr = 83,
    AvrIntr = 84,
    AvrSignal = 85,
    AvrBuiltin = 86,
    AmdgpuVs = 87,
    AmdgpuGs = 88,
    AmdgpuPs = 89,
    AmdgpuCs = 90,
    AmdgpuKernel = 91,
    X86RegCall = 92,
    AmdgpuHs = 93,
    Msp430Builtin = 94,
    AmdgpuLs = 95,
    AmdgpuEs = 96,
    Aarch64VectorCall = 97,
    Aarch64SveVectorCall = 98,
    WasmEmscriptenInvoke = 99,
    AmdgpuGfx = 100,
    M68kIntr = 101,
    MaxID = 1023,
}

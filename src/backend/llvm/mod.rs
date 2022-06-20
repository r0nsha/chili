mod abi;
mod binary;
mod codegen;
mod conditional;
mod function;
mod intrinsics;
mod literal;
mod panic;
mod runtime_check;
mod startup;
mod traits;
mod ty;
mod unary;
mod util;

use crate::ast::{ast, workspace::Workspace};
use crate::infer::ty_ctx::TyCtx;
use crate::{
    common::{
        build_options::{BuildOptions, EnabledCodegenOptions},
        target::{Arch, Os, TargetMetrics},
    },
    time,
};
use codegen::Codegen;
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
use path_absolutize::Absolutize;
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    process::Command,
};
use ustr::UstrMap;

pub fn codegen<'w>(
    workspace: &Workspace,
    tycx: &TyCtx,
    ast: &ast::TypedAst,
    codegen_options: &EnabledCodegenOptions,
) -> String {
    let context = Context::create();
    let module = context.create_module(
        workspace
            .build_options
            .source_file
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
    init_pass_manager(&fpm, workspace.build_options.opt_level.is_release());

    let mut cg = Codegen {
        workspace,
        tycx,
        typed_ast: ast,
        target_metrics: target_metrics.clone(),
        context: &context,
        module: &module,
        fpm: &fpm,
        builder: &builder,
        ptr_sized_int_type: context.ptr_sized_int_type(&target_machine.get_target_data(), None),
        global_decls: HashMap::default(),
        types: HashMap::default(),
        static_strs: UstrMap::default(),
        functions: HashMap::default(),
        extern_functions: UstrMap::default(),
        intrinsics: HashMap::default(),
    };

    time! { workspace.build_options.verbose, "llvm",
        cg.start()
    };

    if codegen_options.emit_llvm_ir {
        dump_ir(&module, &workspace.build_options.source_file);
    }

    let executable_path = build_executable(
        &workspace.build_options,
        &target_machine,
        &target_metrics,
        &module,
        &workspace.extern_libraries,
    );

    executable_path
}

fn init_pass_manager<'a>(fpm: &PassManager<FunctionValue<'a>>, is_release: bool) {
    if is_release {
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_scalar_repl_aggregates_pass_ssa();
        fpm.add_reassociate_pass();
        fpm.add_tail_call_elimination_pass();
        fpm.add_gvn_pass();
        fpm.add_loop_deletion_pass();
        fpm.add_loop_rotate_pass();
        fpm.add_loop_vectorize_pass();
    }
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
    target_metrics: &TargetMetrics,
    module: &Module,
    extern_libraries: &HashSet<ast::ExternLibrary>,
) -> String {
    let output_path = build_options
        .output_file
        .as_ref()
        .unwrap_or_else(|| &build_options.source_file);

    let object_file = if target_metrics.os == Os::Windows {
        output_path.with_extension("obj")
    } else {
        output_path.with_extension("o")
    };

    let executable_file = if target_metrics.os == Os::Windows {
        output_path.with_extension("exe")
    } else {
        output_path.with_extension("")
    };

    let _ = std::fs::create_dir_all(output_path.parent().unwrap());

    time! { build_options.verbose, "write obj",
        target_machine
            .write_to_file(&module, FileType::Object, &object_file)
            .unwrap()
    };

    time! { build_options.verbose, "link",
        link(target_metrics, &executable_file, &object_file,&extern_libraries,)
    };

    let _ = std::fs::remove_file(object_file);

    executable_file
        .absolutize()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string()
}

fn link(
    target_metrics: &TargetMetrics,
    executable_file: &PathBuf,
    object_file: &PathBuf,
    extern_libraries: &HashSet<ast::ExternLibrary>,
) {
    let link_flags = match target_metrics.arch {
        Arch::Amd64 => match target_metrics.os {
            Os::Windows => vec!["/machine:x64"],
            Os::Linux | Os::FreeBSD => vec!["-arch x86-64"],
            _ => vec![],
        },
        Arch::_386 => match target_metrics.os {
            Os::Windows => vec!["/machine:x86"],
            Os::Darwin => panic!("unsupported architecture"), // TODO: this needs to be a proper diagnostic
            Os::Linux | Os::FreeBSD => vec!["-arch x86"],
            _ => vec![],
        },
        Arch::Arm64 => match target_metrics.os {
            Os::Darwin => vec!["-arch arm64"],
            Os::Linux => vec!["-arch aarch64"],
            _ => vec![],
        },
        Arch::Wasm32 | Arch::Wasm64 => {
            let mut link_flags = vec!["--allow-undefined"];

            if matches!(target_metrics.arch, Arch::Wasm64) {
                link_flags.push("-mwas64");
            }

            if matches!(target_metrics.os, Os::Freestanding) {
                link_flags.push("--no-entry");
            }

            link_flags
        }
    };

    if cfg!(windows) {
        let mut lib_paths = vec![];
        let mut libs = vec![];

        for lib in extern_libraries.iter() {
            match lib {
                ast::ExternLibrary::System(lib_name) => {
                    if !is_libc(lib_name) {
                        libs.push(lib_name.clone())
                    }
                }
                ast::ExternLibrary::Path(path) => {
                    lib_paths.push(path.lib_dir().to_str().unwrap().to_string());
                    libs.push(path.lib_name().to_str().unwrap().to_string());
                }
            }
        }

        Command::new("lld-link")
            .arg(format!("/out:{}", executable_file.to_str().unwrap()))
            .arg("/entry:mainCRTStartup")
            .arg("/defaultlib:libcmt")
            .arg("/nologo")
            .arg("/incremental:no")
            .arg("/opt:ref")
            .arg("/threads:8")
            .arg("/subsystem:CONSOLE")
            .args(lib_paths.iter().map(|path| format!("/libpath:{}", path)))
            .arg(object_file.to_str().unwrap())
            .args(libs)
            .args(link_flags)
            .execute_output()
            .unwrap();
    } else {
        let libs: Vec<String> = extern_libraries
            .iter()
            .map(|lib| match lib {
                ast::ExternLibrary::System(lib_name) => {
                    if !is_libc(lib_name) {
                        Some(format!("-l{}", lib_name))
                    } else {
                        None
                    }
                }
                ast::ExternLibrary::Path(path) => Some(format!("-l:{}", path.to_string())),
            })
            .flatten()
            .collect();

        Command::new("clang")
            .arg("-Wno-unused-command-line-argument")
            .arg(object_file.to_str().unwrap())
            .arg(format!("-o{}", executable_file.to_str().unwrap()))
            .arg("-lc")
            .arg("-lm")
            .arg("-no-pie")
            .args(libs)
            .args(link_flags)
            .execute_output()
            .unwrap();
    }
}

fn is_libc(lib: &str) -> bool {
    lib.eq_ignore_ascii_case("c")
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

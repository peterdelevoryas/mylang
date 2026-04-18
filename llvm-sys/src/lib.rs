#![allow(bad_style)]

pub const LLVMDisassembler_VariantKind_None: u32 = 0;
pub const LLVMDisassembler_VariantKind_ARM_HI16: u32 = 1;
pub const LLVMDisassembler_VariantKind_ARM_LO16: u32 = 2;
pub const LLVMDisassembler_VariantKind_ARM64_PAGE: u32 = 1;
pub const LLVMDisassembler_VariantKind_ARM64_PAGEOFF: u32 = 2;
pub const LLVMDisassembler_VariantKind_ARM64_GOTPAGE: u32 = 3;
pub const LLVMDisassembler_VariantKind_ARM64_GOTPAGEOFF: u32 = 4;
pub const LLVMDisassembler_VariantKind_ARM64_TLVP: u32 = 5;
pub const LLVMDisassembler_VariantKind_ARM64_TLVOFF: u32 = 6;
pub const LLVMDisassembler_ReferenceType_InOut_None: u32 = 0;
pub const LLVMDisassembler_ReferenceType_In_Branch: u32 = 1;
pub const LLVMDisassembler_ReferenceType_In_PCrel_Load: u32 = 2;
pub const LLVMDisassembler_ReferenceType_In_ARM64_ADRP: u64 = 4294967297;
pub const LLVMDisassembler_ReferenceType_In_ARM64_ADDXri: u64 = 4294967298;
pub const LLVMDisassembler_ReferenceType_In_ARM64_LDRXui: u64 = 4294967299;
pub const LLVMDisassembler_ReferenceType_In_ARM64_LDRXl: u64 = 4294967300;
pub const LLVMDisassembler_ReferenceType_In_ARM64_ADR: u64 = 4294967301;
pub const LLVMDisassembler_ReferenceType_Out_SymbolStub: u32 = 1;
pub const LLVMDisassembler_ReferenceType_Out_LitPool_SymAddr: u32 = 2;
pub const LLVMDisassembler_ReferenceType_Out_LitPool_CstrAddr: u32 = 3;
pub const LLVMDisassembler_ReferenceType_Out_Objc_CFString_Ref: u32 = 4;
pub const LLVMDisassembler_ReferenceType_Out_Objc_Message: u32 = 5;
pub const LLVMDisassembler_ReferenceType_Out_Objc_Message_Ref: u32 = 6;
pub const LLVMDisassembler_ReferenceType_Out_Objc_Selector_Ref: u32 = 7;
pub const LLVMDisassembler_ReferenceType_Out_Objc_Class_Ref: u32 = 8;
pub const LLVMDisassembler_ReferenceType_DeMangled_Name: u32 = 9;
pub const LLVMDisassembler_Option_UseMarkup: u32 = 1;
pub const LLVMDisassembler_Option_PrintImmHex: u32 = 2;
pub const LLVMDisassembler_Option_AsmPrinterVariant: u32 = 4;
pub const LLVMDisassembler_Option_SetInstrComments: u32 = 8;
pub const LLVMDisassembler_Option_PrintLatency: u32 = 16;
pub const LLVMErrorSuccess: u32 = 0;
pub const LLVM_DEFAULT_TARGET_TRIPLE: &'static [u8; 20usize] = b"x86_64-pc-linux-gnu\0";
pub const LLVM_ENABLE_THREADS: u32 = 1;
pub const LLVM_HAS_ATOMICS: u32 = 1;
pub const LLVM_HOST_TRIPLE: &'static [u8; 20usize] = b"x86_64-pc-linux-gnu\0";
pub const LLVM_ON_UNIX: u32 = 1;
pub const LLVM_USE_INTEL_JITEVENTS: u32 = 0;
pub const LLVM_USE_OPROFILE: u32 = 0;
pub const LLVM_USE_PERF: u32 = 0;
pub const LLVM_VERSION_MAJOR: u32 = 10;
pub const LLVM_VERSION_MINOR: u32 = 0;
pub const LLVM_VERSION_PATCH: u32 = 0;
pub const LLVM_VERSION_STRING: &'static [u8; 7usize] = b"10.0.0\0";
pub const LLVM_FORCE_ENABLE_STATS: u32 = 0;
pub type __uint8_t = ::std::os::raw::c_uchar;
pub type __uint32_t = ::std::os::raw::c_uint;
pub type __int64_t = ::std::os::raw::c_long;
pub type __uint64_t = ::std::os::raw::c_ulong;
pub type size_t = ::std::os::raw::c_ulong;
pub type LLVMBool = ::std::os::raw::c_int;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueMemoryBuffer {
    _unused: [u8; 0],
}
pub type LLVMMemoryBufferRef = *mut LLVMOpaqueMemoryBuffer;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueContext {
    _unused: [u8; 0],
}
pub type LLVMContextRef = *mut LLVMOpaqueContext;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueModule {
    _unused: [u8; 0],
}
pub type LLVMModuleRef = *mut LLVMOpaqueModule;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueType {
    _unused: [u8; 0],
}
pub type LLVMTypeRef = *mut LLVMOpaqueType;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueValue {
    _unused: [u8; 0],
}
pub type LLVMValueRef = *mut LLVMOpaqueValue;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueBasicBlock {
    _unused: [u8; 0],
}
pub type LLVMBasicBlockRef = *mut LLVMOpaqueBasicBlock;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueMetadata {
    _unused: [u8; 0],
}
pub type LLVMMetadataRef = *mut LLVMOpaqueMetadata;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueNamedMDNode {
    _unused: [u8; 0],
}
pub type LLVMNamedMDNodeRef = *mut LLVMOpaqueNamedMDNode;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueValueMetadataEntry {
    _unused: [u8; 0],
}
pub type LLVMValueMetadataEntry = LLVMOpaqueValueMetadataEntry;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueBuilder {
    _unused: [u8; 0],
}
pub type LLVMBuilderRef = *mut LLVMOpaqueBuilder;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueDIBuilder {
    _unused: [u8; 0],
}
pub type LLVMDIBuilderRef = *mut LLVMOpaqueDIBuilder;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueModuleProvider {
    _unused: [u8; 0],
}
pub type LLVMModuleProviderRef = *mut LLVMOpaqueModuleProvider;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaquePassManager {
    _unused: [u8; 0],
}
pub type LLVMPassManagerRef = *mut LLVMOpaquePassManager;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaquePassRegistry {
    _unused: [u8; 0],
}
pub type LLVMPassRegistryRef = *mut LLVMOpaquePassRegistry;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueUse {
    _unused: [u8; 0],
}
pub type LLVMUseRef = *mut LLVMOpaqueUse;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueAttributeRef {
    _unused: [u8; 0],
}
pub type LLVMAttributeRef = *mut LLVMOpaqueAttributeRef;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueDiagnosticInfo {
    _unused: [u8; 0],
}
pub type LLVMDiagnosticInfoRef = *mut LLVMOpaqueDiagnosticInfo;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMComdat {
    _unused: [u8; 0],
}
pub type LLVMComdatRef = *mut LLVMComdat;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueModuleFlagEntry {
    _unused: [u8; 0],
}
pub type LLVMModuleFlagEntry = LLVMOpaqueModuleFlagEntry;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueJITEventListener {
    _unused: [u8; 0],
}
pub type LLVMJITEventListenerRef = *mut LLVMOpaqueJITEventListener;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueBinary {
    _unused: [u8; 0],
}
pub type LLVMBinaryRef = *mut LLVMOpaqueBinary;
pub const LLVMVerifierFailureAction_LLVMAbortProcessAction: LLVMVerifierFailureAction = 0;
pub const LLVMVerifierFailureAction_LLVMPrintMessageAction: LLVMVerifierFailureAction = 1;
pub const LLVMVerifierFailureAction_LLVMReturnStatusAction: LLVMVerifierFailureAction = 2;
pub type LLVMVerifierFailureAction = u32;
extern "C" {
    pub fn LLVMVerifyModule(
        M: LLVMModuleRef,
        Action: LLVMVerifierFailureAction,
        OutMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMVerifyFunction(Fn: LLVMValueRef, Action: LLVMVerifierFailureAction) -> LLVMBool;
}
extern "C" {
    pub fn LLVMViewFunctionCFG(Fn: LLVMValueRef);
}
extern "C" {
    pub fn LLVMViewFunctionCFGOnly(Fn: LLVMValueRef);
}
extern "C" {
    pub fn LLVMParseBitcode(
        MemBuf: LLVMMemoryBufferRef,
        OutModule: *mut LLVMModuleRef,
        OutMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMParseBitcode2(
        MemBuf: LLVMMemoryBufferRef,
        OutModule: *mut LLVMModuleRef,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMParseBitcodeInContext(
        ContextRef: LLVMContextRef,
        MemBuf: LLVMMemoryBufferRef,
        OutModule: *mut LLVMModuleRef,
        OutMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMParseBitcodeInContext2(
        ContextRef: LLVMContextRef,
        MemBuf: LLVMMemoryBufferRef,
        OutModule: *mut LLVMModuleRef,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetBitcodeModuleInContext(
        ContextRef: LLVMContextRef,
        MemBuf: LLVMMemoryBufferRef,
        OutM: *mut LLVMModuleRef,
        OutMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetBitcodeModuleInContext2(
        ContextRef: LLVMContextRef,
        MemBuf: LLVMMemoryBufferRef,
        OutM: *mut LLVMModuleRef,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetBitcodeModule(
        MemBuf: LLVMMemoryBufferRef,
        OutM: *mut LLVMModuleRef,
        OutMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetBitcodeModule2(MemBuf: LLVMMemoryBufferRef, OutM: *mut LLVMModuleRef)
        -> LLVMBool;
}
extern "C" {
    pub fn LLVMWriteBitcodeToFile(
        M: LLVMModuleRef,
        Path: *const ::std::os::raw::c_char,
    ) -> ::std::os::raw::c_int;
}
extern "C" {
    pub fn LLVMWriteBitcodeToFD(
        M: LLVMModuleRef,
        FD: ::std::os::raw::c_int,
        ShouldClose: ::std::os::raw::c_int,
        Unbuffered: ::std::os::raw::c_int,
    ) -> ::std::os::raw::c_int;
}
extern "C" {
    pub fn LLVMWriteBitcodeToFileHandle(
        M: LLVMModuleRef,
        Handle: ::std::os::raw::c_int,
    ) -> ::std::os::raw::c_int;
}
extern "C" {
    pub fn LLVMWriteBitcodeToMemoryBuffer(M: LLVMModuleRef) -> LLVMMemoryBufferRef;
}
pub const LLVMComdatSelectionKind_LLVMAnyComdatSelectionKind: LLVMComdatSelectionKind = 0;
pub const LLVMComdatSelectionKind_LLVMExactMatchComdatSelectionKind: LLVMComdatSelectionKind = 1;
pub const LLVMComdatSelectionKind_LLVMLargestComdatSelectionKind: LLVMComdatSelectionKind = 2;
pub const LLVMComdatSelectionKind_LLVMNoDuplicatesComdatSelectionKind: LLVMComdatSelectionKind = 3;
pub const LLVMComdatSelectionKind_LLVMSameSizeComdatSelectionKind: LLVMComdatSelectionKind = 4;
pub type LLVMComdatSelectionKind = u32;
extern "C" {
    pub fn LLVMGetOrInsertComdat(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMComdatRef;
}
extern "C" {
    pub fn LLVMGetComdat(V: LLVMValueRef) -> LLVMComdatRef;
}
extern "C" {
    pub fn LLVMSetComdat(V: LLVMValueRef, C: LLVMComdatRef);
}
extern "C" {
    pub fn LLVMGetComdatSelectionKind(C: LLVMComdatRef) -> LLVMComdatSelectionKind;
}
extern "C" {
    pub fn LLVMSetComdatSelectionKind(C: LLVMComdatRef, Kind: LLVMComdatSelectionKind);
}
pub type LLVMFatalErrorHandler =
    ::std::option::Option<unsafe extern "C" fn(Reason: *const ::std::os::raw::c_char)>;
extern "C" {
    pub fn LLVMInstallFatalErrorHandler(Handler: LLVMFatalErrorHandler);
}
extern "C" {
    pub fn LLVMResetFatalErrorHandler();
}
extern "C" {
    pub fn LLVMEnablePrettyStackTrace();
}
pub const LLVMOpcode_LLVMRet: LLVMOpcode = 1;
pub const LLVMOpcode_LLVMBr: LLVMOpcode = 2;
pub const LLVMOpcode_LLVMSwitch: LLVMOpcode = 3;
pub const LLVMOpcode_LLVMIndirectBr: LLVMOpcode = 4;
pub const LLVMOpcode_LLVMInvoke: LLVMOpcode = 5;
pub const LLVMOpcode_LLVMUnreachable: LLVMOpcode = 7;
pub const LLVMOpcode_LLVMCallBr: LLVMOpcode = 67;
pub const LLVMOpcode_LLVMFNeg: LLVMOpcode = 66;
pub const LLVMOpcode_LLVMAdd: LLVMOpcode = 8;
pub const LLVMOpcode_LLVMFAdd: LLVMOpcode = 9;
pub const LLVMOpcode_LLVMSub: LLVMOpcode = 10;
pub const LLVMOpcode_LLVMFSub: LLVMOpcode = 11;
pub const LLVMOpcode_LLVMMul: LLVMOpcode = 12;
pub const LLVMOpcode_LLVMFMul: LLVMOpcode = 13;
pub const LLVMOpcode_LLVMUDiv: LLVMOpcode = 14;
pub const LLVMOpcode_LLVMSDiv: LLVMOpcode = 15;
pub const LLVMOpcode_LLVMFDiv: LLVMOpcode = 16;
pub const LLVMOpcode_LLVMURem: LLVMOpcode = 17;
pub const LLVMOpcode_LLVMSRem: LLVMOpcode = 18;
pub const LLVMOpcode_LLVMFRem: LLVMOpcode = 19;
pub const LLVMOpcode_LLVMShl: LLVMOpcode = 20;
pub const LLVMOpcode_LLVMLShr: LLVMOpcode = 21;
pub const LLVMOpcode_LLVMAShr: LLVMOpcode = 22;
pub const LLVMOpcode_LLVMAnd: LLVMOpcode = 23;
pub const LLVMOpcode_LLVMOr: LLVMOpcode = 24;
pub const LLVMOpcode_LLVMXor: LLVMOpcode = 25;
pub const LLVMOpcode_LLVMAlloca: LLVMOpcode = 26;
pub const LLVMOpcode_LLVMLoad: LLVMOpcode = 27;
pub const LLVMOpcode_LLVMStore: LLVMOpcode = 28;
pub const LLVMOpcode_LLVMGetElementPtr: LLVMOpcode = 29;
pub const LLVMOpcode_LLVMTrunc: LLVMOpcode = 30;
pub const LLVMOpcode_LLVMZExt: LLVMOpcode = 31;
pub const LLVMOpcode_LLVMSExt: LLVMOpcode = 32;
pub const LLVMOpcode_LLVMFPToUI: LLVMOpcode = 33;
pub const LLVMOpcode_LLVMFPToSI: LLVMOpcode = 34;
pub const LLVMOpcode_LLVMUIToFP: LLVMOpcode = 35;
pub const LLVMOpcode_LLVMSIToFP: LLVMOpcode = 36;
pub const LLVMOpcode_LLVMFPTrunc: LLVMOpcode = 37;
pub const LLVMOpcode_LLVMFPExt: LLVMOpcode = 38;
pub const LLVMOpcode_LLVMPtrToInt: LLVMOpcode = 39;
pub const LLVMOpcode_LLVMIntToPtr: LLVMOpcode = 40;
pub const LLVMOpcode_LLVMBitCast: LLVMOpcode = 41;
pub const LLVMOpcode_LLVMAddrSpaceCast: LLVMOpcode = 60;
pub const LLVMOpcode_LLVMICmp: LLVMOpcode = 42;
pub const LLVMOpcode_LLVMFCmp: LLVMOpcode = 43;
pub const LLVMOpcode_LLVMPHI: LLVMOpcode = 44;
pub const LLVMOpcode_LLVMCall: LLVMOpcode = 45;
pub const LLVMOpcode_LLVMSelect: LLVMOpcode = 46;
pub const LLVMOpcode_LLVMUserOp1: LLVMOpcode = 47;
pub const LLVMOpcode_LLVMUserOp2: LLVMOpcode = 48;
pub const LLVMOpcode_LLVMVAArg: LLVMOpcode = 49;
pub const LLVMOpcode_LLVMExtractElement: LLVMOpcode = 50;
pub const LLVMOpcode_LLVMInsertElement: LLVMOpcode = 51;
pub const LLVMOpcode_LLVMShuffleVector: LLVMOpcode = 52;
pub const LLVMOpcode_LLVMExtractValue: LLVMOpcode = 53;
pub const LLVMOpcode_LLVMInsertValue: LLVMOpcode = 54;
pub const LLVMOpcode_LLVMFreeze: LLVMOpcode = 68;
pub const LLVMOpcode_LLVMFence: LLVMOpcode = 55;
pub const LLVMOpcode_LLVMAtomicCmpXchg: LLVMOpcode = 56;
pub const LLVMOpcode_LLVMAtomicRMW: LLVMOpcode = 57;
pub const LLVMOpcode_LLVMResume: LLVMOpcode = 58;
pub const LLVMOpcode_LLVMLandingPad: LLVMOpcode = 59;
pub const LLVMOpcode_LLVMCleanupRet: LLVMOpcode = 61;
pub const LLVMOpcode_LLVMCatchRet: LLVMOpcode = 62;
pub const LLVMOpcode_LLVMCatchPad: LLVMOpcode = 63;
pub const LLVMOpcode_LLVMCleanupPad: LLVMOpcode = 64;
pub const LLVMOpcode_LLVMCatchSwitch: LLVMOpcode = 65;
pub type LLVMOpcode = u32;
pub const LLVMTypeKind_LLVMVoidTypeKind: LLVMTypeKind = 0;
pub const LLVMTypeKind_LLVMHalfTypeKind: LLVMTypeKind = 1;
pub const LLVMTypeKind_LLVMFloatTypeKind: LLVMTypeKind = 2;
pub const LLVMTypeKind_LLVMDoubleTypeKind: LLVMTypeKind = 3;
pub const LLVMTypeKind_LLVMX86_FP80TypeKind: LLVMTypeKind = 4;
pub const LLVMTypeKind_LLVMFP128TypeKind: LLVMTypeKind = 5;
pub const LLVMTypeKind_LLVMPPC_FP128TypeKind: LLVMTypeKind = 6;
pub const LLVMTypeKind_LLVMLabelTypeKind: LLVMTypeKind = 7;
pub const LLVMTypeKind_LLVMIntegerTypeKind: LLVMTypeKind = 8;
pub const LLVMTypeKind_LLVMFunctionTypeKind: LLVMTypeKind = 9;
pub const LLVMTypeKind_LLVMStructTypeKind: LLVMTypeKind = 10;
pub const LLVMTypeKind_LLVMArrayTypeKind: LLVMTypeKind = 11;
pub const LLVMTypeKind_LLVMPointerTypeKind: LLVMTypeKind = 12;
pub const LLVMTypeKind_LLVMVectorTypeKind: LLVMTypeKind = 13;
pub const LLVMTypeKind_LLVMMetadataTypeKind: LLVMTypeKind = 14;
pub const LLVMTypeKind_LLVMX86_MMXTypeKind: LLVMTypeKind = 15;
pub const LLVMTypeKind_LLVMTokenTypeKind: LLVMTypeKind = 16;
pub type LLVMTypeKind = u32;
pub const LLVMLinkage_LLVMExternalLinkage: LLVMLinkage = 0;
pub const LLVMLinkage_LLVMAvailableExternallyLinkage: LLVMLinkage = 1;
pub const LLVMLinkage_LLVMLinkOnceAnyLinkage: LLVMLinkage = 2;
pub const LLVMLinkage_LLVMLinkOnceODRLinkage: LLVMLinkage = 3;
pub const LLVMLinkage_LLVMLinkOnceODRAutoHideLinkage: LLVMLinkage = 4;
pub const LLVMLinkage_LLVMWeakAnyLinkage: LLVMLinkage = 5;
pub const LLVMLinkage_LLVMWeakODRLinkage: LLVMLinkage = 6;
pub const LLVMLinkage_LLVMAppendingLinkage: LLVMLinkage = 7;
pub const LLVMLinkage_LLVMInternalLinkage: LLVMLinkage = 8;
pub const LLVMLinkage_LLVMPrivateLinkage: LLVMLinkage = 9;
pub const LLVMLinkage_LLVMDLLImportLinkage: LLVMLinkage = 10;
pub const LLVMLinkage_LLVMDLLExportLinkage: LLVMLinkage = 11;
pub const LLVMLinkage_LLVMExternalWeakLinkage: LLVMLinkage = 12;
pub const LLVMLinkage_LLVMGhostLinkage: LLVMLinkage = 13;
pub const LLVMLinkage_LLVMCommonLinkage: LLVMLinkage = 14;
pub const LLVMLinkage_LLVMLinkerPrivateLinkage: LLVMLinkage = 15;
pub const LLVMLinkage_LLVMLinkerPrivateWeakLinkage: LLVMLinkage = 16;
pub type LLVMLinkage = u32;
pub const LLVMVisibility_LLVMDefaultVisibility: LLVMVisibility = 0;
pub const LLVMVisibility_LLVMHiddenVisibility: LLVMVisibility = 1;
pub const LLVMVisibility_LLVMProtectedVisibility: LLVMVisibility = 2;
pub type LLVMVisibility = u32;
pub const LLVMUnnamedAddr_LLVMNoUnnamedAddr: LLVMUnnamedAddr = 0;
pub const LLVMUnnamedAddr_LLVMLocalUnnamedAddr: LLVMUnnamedAddr = 1;
pub const LLVMUnnamedAddr_LLVMGlobalUnnamedAddr: LLVMUnnamedAddr = 2;
pub type LLVMUnnamedAddr = u32;
pub const LLVMDLLStorageClass_LLVMDefaultStorageClass: LLVMDLLStorageClass = 0;
pub const LLVMDLLStorageClass_LLVMDLLImportStorageClass: LLVMDLLStorageClass = 1;
pub const LLVMDLLStorageClass_LLVMDLLExportStorageClass: LLVMDLLStorageClass = 2;
pub type LLVMDLLStorageClass = u32;
pub const LLVMCallConv_LLVMCCallConv: LLVMCallConv = 0;
pub const LLVMCallConv_LLVMFastCallConv: LLVMCallConv = 8;
pub const LLVMCallConv_LLVMColdCallConv: LLVMCallConv = 9;
pub const LLVMCallConv_LLVMGHCCallConv: LLVMCallConv = 10;
pub const LLVMCallConv_LLVMHiPECallConv: LLVMCallConv = 11;
pub const LLVMCallConv_LLVMWebKitJSCallConv: LLVMCallConv = 12;
pub const LLVMCallConv_LLVMAnyRegCallConv: LLVMCallConv = 13;
pub const LLVMCallConv_LLVMPreserveMostCallConv: LLVMCallConv = 14;
pub const LLVMCallConv_LLVMPreserveAllCallConv: LLVMCallConv = 15;
pub const LLVMCallConv_LLVMSwiftCallConv: LLVMCallConv = 16;
pub const LLVMCallConv_LLVMCXXFASTTLSCallConv: LLVMCallConv = 17;
pub const LLVMCallConv_LLVMX86StdcallCallConv: LLVMCallConv = 64;
pub const LLVMCallConv_LLVMX86FastcallCallConv: LLVMCallConv = 65;
pub const LLVMCallConv_LLVMARMAPCSCallConv: LLVMCallConv = 66;
pub const LLVMCallConv_LLVMARMAAPCSCallConv: LLVMCallConv = 67;
pub const LLVMCallConv_LLVMARMAAPCSVFPCallConv: LLVMCallConv = 68;
pub const LLVMCallConv_LLVMMSP430INTRCallConv: LLVMCallConv = 69;
pub const LLVMCallConv_LLVMX86ThisCallCallConv: LLVMCallConv = 70;
pub const LLVMCallConv_LLVMPTXKernelCallConv: LLVMCallConv = 71;
pub const LLVMCallConv_LLVMPTXDeviceCallConv: LLVMCallConv = 72;
pub const LLVMCallConv_LLVMSPIRFUNCCallConv: LLVMCallConv = 75;
pub const LLVMCallConv_LLVMSPIRKERNELCallConv: LLVMCallConv = 76;
pub const LLVMCallConv_LLVMIntelOCLBICallConv: LLVMCallConv = 77;
pub const LLVMCallConv_LLVMX8664SysVCallConv: LLVMCallConv = 78;
pub const LLVMCallConv_LLVMWin64CallConv: LLVMCallConv = 79;
pub const LLVMCallConv_LLVMX86VectorCallCallConv: LLVMCallConv = 80;
pub const LLVMCallConv_LLVMHHVMCallConv: LLVMCallConv = 81;
pub const LLVMCallConv_LLVMHHVMCCallConv: LLVMCallConv = 82;
pub const LLVMCallConv_LLVMX86INTRCallConv: LLVMCallConv = 83;
pub const LLVMCallConv_LLVMAVRINTRCallConv: LLVMCallConv = 84;
pub const LLVMCallConv_LLVMAVRSIGNALCallConv: LLVMCallConv = 85;
pub const LLVMCallConv_LLVMAVRBUILTINCallConv: LLVMCallConv = 86;
pub const LLVMCallConv_LLVMAMDGPUVSCallConv: LLVMCallConv = 87;
pub const LLVMCallConv_LLVMAMDGPUGSCallConv: LLVMCallConv = 88;
pub const LLVMCallConv_LLVMAMDGPUPSCallConv: LLVMCallConv = 89;
pub const LLVMCallConv_LLVMAMDGPUCSCallConv: LLVMCallConv = 90;
pub const LLVMCallConv_LLVMAMDGPUKERNELCallConv: LLVMCallConv = 91;
pub const LLVMCallConv_LLVMX86RegCallCallConv: LLVMCallConv = 92;
pub const LLVMCallConv_LLVMAMDGPUHSCallConv: LLVMCallConv = 93;
pub const LLVMCallConv_LLVMMSP430BUILTINCallConv: LLVMCallConv = 94;
pub const LLVMCallConv_LLVMAMDGPULSCallConv: LLVMCallConv = 95;
pub const LLVMCallConv_LLVMAMDGPUESCallConv: LLVMCallConv = 96;
pub type LLVMCallConv = u32;
pub const LLVMValueKind_LLVMArgumentValueKind: LLVMValueKind = 0;
pub const LLVMValueKind_LLVMBasicBlockValueKind: LLVMValueKind = 1;
pub const LLVMValueKind_LLVMMemoryUseValueKind: LLVMValueKind = 2;
pub const LLVMValueKind_LLVMMemoryDefValueKind: LLVMValueKind = 3;
pub const LLVMValueKind_LLVMMemoryPhiValueKind: LLVMValueKind = 4;
pub const LLVMValueKind_LLVMFunctionValueKind: LLVMValueKind = 5;
pub const LLVMValueKind_LLVMGlobalAliasValueKind: LLVMValueKind = 6;
pub const LLVMValueKind_LLVMGlobalIFuncValueKind: LLVMValueKind = 7;
pub const LLVMValueKind_LLVMGlobalVariableValueKind: LLVMValueKind = 8;
pub const LLVMValueKind_LLVMBlockAddressValueKind: LLVMValueKind = 9;
pub const LLVMValueKind_LLVMConstantExprValueKind: LLVMValueKind = 10;
pub const LLVMValueKind_LLVMConstantArrayValueKind: LLVMValueKind = 11;
pub const LLVMValueKind_LLVMConstantStructValueKind: LLVMValueKind = 12;
pub const LLVMValueKind_LLVMConstantVectorValueKind: LLVMValueKind = 13;
pub const LLVMValueKind_LLVMUndefValueValueKind: LLVMValueKind = 14;
pub const LLVMValueKind_LLVMConstantAggregateZeroValueKind: LLVMValueKind = 15;
pub const LLVMValueKind_LLVMConstantDataArrayValueKind: LLVMValueKind = 16;
pub const LLVMValueKind_LLVMConstantDataVectorValueKind: LLVMValueKind = 17;
pub const LLVMValueKind_LLVMConstantIntValueKind: LLVMValueKind = 18;
pub const LLVMValueKind_LLVMConstantFPValueKind: LLVMValueKind = 19;
pub const LLVMValueKind_LLVMConstantPointerNullValueKind: LLVMValueKind = 20;
pub const LLVMValueKind_LLVMConstantTokenNoneValueKind: LLVMValueKind = 21;
pub const LLVMValueKind_LLVMMetadataAsValueValueKind: LLVMValueKind = 22;
pub const LLVMValueKind_LLVMInlineAsmValueKind: LLVMValueKind = 23;
pub const LLVMValueKind_LLVMInstructionValueKind: LLVMValueKind = 24;
pub type LLVMValueKind = u32;
pub const LLVMIntPredicate_LLVMIntEQ: LLVMIntPredicate = 32;
pub const LLVMIntPredicate_LLVMIntNE: LLVMIntPredicate = 33;
pub const LLVMIntPredicate_LLVMIntUGT: LLVMIntPredicate = 34;
pub const LLVMIntPredicate_LLVMIntUGE: LLVMIntPredicate = 35;
pub const LLVMIntPredicate_LLVMIntULT: LLVMIntPredicate = 36;
pub const LLVMIntPredicate_LLVMIntULE: LLVMIntPredicate = 37;
pub const LLVMIntPredicate_LLVMIntSGT: LLVMIntPredicate = 38;
pub const LLVMIntPredicate_LLVMIntSGE: LLVMIntPredicate = 39;
pub const LLVMIntPredicate_LLVMIntSLT: LLVMIntPredicate = 40;
pub const LLVMIntPredicate_LLVMIntSLE: LLVMIntPredicate = 41;
pub type LLVMIntPredicate = u32;
pub const LLVMRealPredicate_LLVMRealPredicateFalse: LLVMRealPredicate = 0;
pub const LLVMRealPredicate_LLVMRealOEQ: LLVMRealPredicate = 1;
pub const LLVMRealPredicate_LLVMRealOGT: LLVMRealPredicate = 2;
pub const LLVMRealPredicate_LLVMRealOGE: LLVMRealPredicate = 3;
pub const LLVMRealPredicate_LLVMRealOLT: LLVMRealPredicate = 4;
pub const LLVMRealPredicate_LLVMRealOLE: LLVMRealPredicate = 5;
pub const LLVMRealPredicate_LLVMRealONE: LLVMRealPredicate = 6;
pub const LLVMRealPredicate_LLVMRealORD: LLVMRealPredicate = 7;
pub const LLVMRealPredicate_LLVMRealUNO: LLVMRealPredicate = 8;
pub const LLVMRealPredicate_LLVMRealUEQ: LLVMRealPredicate = 9;
pub const LLVMRealPredicate_LLVMRealUGT: LLVMRealPredicate = 10;
pub const LLVMRealPredicate_LLVMRealUGE: LLVMRealPredicate = 11;
pub const LLVMRealPredicate_LLVMRealULT: LLVMRealPredicate = 12;
pub const LLVMRealPredicate_LLVMRealULE: LLVMRealPredicate = 13;
pub const LLVMRealPredicate_LLVMRealUNE: LLVMRealPredicate = 14;
pub const LLVMRealPredicate_LLVMRealPredicateTrue: LLVMRealPredicate = 15;
pub type LLVMRealPredicate = u32;
pub const LLVMLandingPadClauseTy_LLVMLandingPadCatch: LLVMLandingPadClauseTy = 0;
pub const LLVMLandingPadClauseTy_LLVMLandingPadFilter: LLVMLandingPadClauseTy = 1;
pub type LLVMLandingPadClauseTy = u32;
pub const LLVMThreadLocalMode_LLVMNotThreadLocal: LLVMThreadLocalMode = 0;
pub const LLVMThreadLocalMode_LLVMGeneralDynamicTLSModel: LLVMThreadLocalMode = 1;
pub const LLVMThreadLocalMode_LLVMLocalDynamicTLSModel: LLVMThreadLocalMode = 2;
pub const LLVMThreadLocalMode_LLVMInitialExecTLSModel: LLVMThreadLocalMode = 3;
pub const LLVMThreadLocalMode_LLVMLocalExecTLSModel: LLVMThreadLocalMode = 4;
pub type LLVMThreadLocalMode = u32;
pub const LLVMAtomicOrdering_LLVMAtomicOrderingNotAtomic: LLVMAtomicOrdering = 0;
pub const LLVMAtomicOrdering_LLVMAtomicOrderingUnordered: LLVMAtomicOrdering = 1;
pub const LLVMAtomicOrdering_LLVMAtomicOrderingMonotonic: LLVMAtomicOrdering = 2;
pub const LLVMAtomicOrdering_LLVMAtomicOrderingAcquire: LLVMAtomicOrdering = 4;
pub const LLVMAtomicOrdering_LLVMAtomicOrderingRelease: LLVMAtomicOrdering = 5;
pub const LLVMAtomicOrdering_LLVMAtomicOrderingAcquireRelease: LLVMAtomicOrdering = 6;
pub const LLVMAtomicOrdering_LLVMAtomicOrderingSequentiallyConsistent: LLVMAtomicOrdering = 7;
pub type LLVMAtomicOrdering = u32;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpXchg: LLVMAtomicRMWBinOp = 0;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpAdd: LLVMAtomicRMWBinOp = 1;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpSub: LLVMAtomicRMWBinOp = 2;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpAnd: LLVMAtomicRMWBinOp = 3;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpNand: LLVMAtomicRMWBinOp = 4;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpOr: LLVMAtomicRMWBinOp = 5;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpXor: LLVMAtomicRMWBinOp = 6;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpMax: LLVMAtomicRMWBinOp = 7;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpMin: LLVMAtomicRMWBinOp = 8;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpUMax: LLVMAtomicRMWBinOp = 9;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpUMin: LLVMAtomicRMWBinOp = 10;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpFAdd: LLVMAtomicRMWBinOp = 11;
pub const LLVMAtomicRMWBinOp_LLVMAtomicRMWBinOpFSub: LLVMAtomicRMWBinOp = 12;
pub type LLVMAtomicRMWBinOp = u32;
pub const LLVMDiagnosticSeverity_LLVMDSError: LLVMDiagnosticSeverity = 0;
pub const LLVMDiagnosticSeverity_LLVMDSWarning: LLVMDiagnosticSeverity = 1;
pub const LLVMDiagnosticSeverity_LLVMDSRemark: LLVMDiagnosticSeverity = 2;
pub const LLVMDiagnosticSeverity_LLVMDSNote: LLVMDiagnosticSeverity = 3;
pub type LLVMDiagnosticSeverity = u32;
pub const LLVMInlineAsmDialect_LLVMInlineAsmDialectATT: LLVMInlineAsmDialect = 0;
pub const LLVMInlineAsmDialect_LLVMInlineAsmDialectIntel: LLVMInlineAsmDialect = 1;
pub type LLVMInlineAsmDialect = u32;
pub const LLVMModuleFlagBehavior_LLVMModuleFlagBehaviorError: LLVMModuleFlagBehavior = 0;
pub const LLVMModuleFlagBehavior_LLVMModuleFlagBehaviorWarning: LLVMModuleFlagBehavior = 1;
pub const LLVMModuleFlagBehavior_LLVMModuleFlagBehaviorRequire: LLVMModuleFlagBehavior = 2;
pub const LLVMModuleFlagBehavior_LLVMModuleFlagBehaviorOverride: LLVMModuleFlagBehavior = 3;
pub const LLVMModuleFlagBehavior_LLVMModuleFlagBehaviorAppend: LLVMModuleFlagBehavior = 4;
pub const LLVMModuleFlagBehavior_LLVMModuleFlagBehaviorAppendUnique: LLVMModuleFlagBehavior = 5;
pub type LLVMModuleFlagBehavior = u32;
pub const LLVMAttributeReturnIndex: _bindgen_ty_2 = 0;
pub const LLVMAttributeFunctionIndex: _bindgen_ty_2 = -1;
pub type _bindgen_ty_2 = i32;
pub type LLVMAttributeIndex = ::std::os::raw::c_uint;
extern "C" {
    pub fn LLVMInitializeCore(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMShutdown();
}
extern "C" {
    pub fn LLVMCreateMessage(Message: *const ::std::os::raw::c_char)
        -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMDisposeMessage(Message: *mut ::std::os::raw::c_char);
}
pub type LLVMDiagnosticHandler = ::std::option::Option<
    unsafe extern "C" fn(arg1: LLVMDiagnosticInfoRef, arg2: *mut ::std::os::raw::c_void),
>;
pub type LLVMYieldCallback = ::std::option::Option<
    unsafe extern "C" fn(arg1: LLVMContextRef, arg2: *mut ::std::os::raw::c_void),
>;
extern "C" {
    pub fn LLVMContextCreate() -> LLVMContextRef;
}
extern "C" {
    pub fn LLVMGetGlobalContext() -> LLVMContextRef;
}
extern "C" {
    pub fn LLVMContextSetDiagnosticHandler(
        C: LLVMContextRef,
        Handler: LLVMDiagnosticHandler,
        DiagnosticContext: *mut ::std::os::raw::c_void,
    );
}
extern "C" {
    pub fn LLVMContextGetDiagnosticHandler(C: LLVMContextRef) -> LLVMDiagnosticHandler;
}
extern "C" {
    pub fn LLVMContextGetDiagnosticContext(C: LLVMContextRef) -> *mut ::std::os::raw::c_void;
}
extern "C" {
    pub fn LLVMContextSetYieldCallback(
        C: LLVMContextRef,
        Callback: LLVMYieldCallback,
        OpaqueHandle: *mut ::std::os::raw::c_void,
    );
}
extern "C" {
    pub fn LLVMContextShouldDiscardValueNames(C: LLVMContextRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMContextSetDiscardValueNames(C: LLVMContextRef, Discard: LLVMBool);
}
extern "C" {
    pub fn LLVMContextDispose(C: LLVMContextRef);
}
extern "C" {
    pub fn LLVMGetDiagInfoDescription(DI: LLVMDiagnosticInfoRef) -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetDiagInfoSeverity(DI: LLVMDiagnosticInfoRef) -> LLVMDiagnosticSeverity;
}
extern "C" {
    pub fn LLVMGetMDKindIDInContext(
        C: LLVMContextRef,
        Name: *const ::std::os::raw::c_char,
        SLen: ::std::os::raw::c_uint,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetMDKindID(
        Name: *const ::std::os::raw::c_char,
        SLen: ::std::os::raw::c_uint,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetEnumAttributeKindForName(
        Name: *const ::std::os::raw::c_char,
        SLen: size_t,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetLastEnumAttributeKind() -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMCreateEnumAttribute(
        C: LLVMContextRef,
        KindID: ::std::os::raw::c_uint,
        Val: u64,
    ) -> LLVMAttributeRef;
}
extern "C" {
    pub fn LLVMGetEnumAttributeKind(A: LLVMAttributeRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetEnumAttributeValue(A: LLVMAttributeRef) -> u64;
}
extern "C" {
    pub fn LLVMCreateStringAttribute(
        C: LLVMContextRef,
        K: *const ::std::os::raw::c_char,
        KLength: ::std::os::raw::c_uint,
        V: *const ::std::os::raw::c_char,
        VLength: ::std::os::raw::c_uint,
    ) -> LLVMAttributeRef;
}
extern "C" {
    pub fn LLVMGetStringAttributeKind(
        A: LLVMAttributeRef,
        Length: *mut ::std::os::raw::c_uint,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetStringAttributeValue(
        A: LLVMAttributeRef,
        Length: *mut ::std::os::raw::c_uint,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMIsEnumAttribute(A: LLVMAttributeRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMIsStringAttribute(A: LLVMAttributeRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMModuleCreateWithName(ModuleID: *const ::std::os::raw::c_char) -> LLVMModuleRef;
}
extern "C" {
    pub fn LLVMModuleCreateWithNameInContext(
        ModuleID: *const ::std::os::raw::c_char,
        C: LLVMContextRef,
    ) -> LLVMModuleRef;
}
extern "C" {
    pub fn LLVMCloneModule(M: LLVMModuleRef) -> LLVMModuleRef;
}
extern "C" {
    pub fn LLVMDisposeModule(M: LLVMModuleRef);
}
extern "C" {
    pub fn LLVMGetModuleIdentifier(
        M: LLVMModuleRef,
        Len: *mut size_t,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMSetModuleIdentifier(
        M: LLVMModuleRef,
        Ident: *const ::std::os::raw::c_char,
        Len: size_t,
    );
}
extern "C" {
    pub fn LLVMGetSourceFileName(
        M: LLVMModuleRef,
        Len: *mut size_t,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMSetSourceFileName(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
        Len: size_t,
    );
}
extern "C" {
    pub fn LLVMGetDataLayoutStr(M: LLVMModuleRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetDataLayout(M: LLVMModuleRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMSetDataLayout(M: LLVMModuleRef, DataLayoutStr: *const ::std::os::raw::c_char);
}
extern "C" {
    pub fn LLVMGetTarget(M: LLVMModuleRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMSetTarget(M: LLVMModuleRef, Triple: *const ::std::os::raw::c_char);
}
extern "C" {
    pub fn LLVMCopyModuleFlagsMetadata(
        M: LLVMModuleRef,
        Len: *mut size_t,
    ) -> *mut LLVMModuleFlagEntry;
}
extern "C" {
    pub fn LLVMDisposeModuleFlagsMetadata(Entries: *mut LLVMModuleFlagEntry);
}
extern "C" {
    pub fn LLVMModuleFlagEntriesGetFlagBehavior(
        Entries: *mut LLVMModuleFlagEntry,
        Index: ::std::os::raw::c_uint,
    ) -> LLVMModuleFlagBehavior;
}
extern "C" {
    pub fn LLVMModuleFlagEntriesGetKey(
        Entries: *mut LLVMModuleFlagEntry,
        Index: ::std::os::raw::c_uint,
        Len: *mut size_t,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMModuleFlagEntriesGetMetadata(
        Entries: *mut LLVMModuleFlagEntry,
        Index: ::std::os::raw::c_uint,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMGetModuleFlag(
        M: LLVMModuleRef,
        Key: *const ::std::os::raw::c_char,
        KeyLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMAddModuleFlag(
        M: LLVMModuleRef,
        Behavior: LLVMModuleFlagBehavior,
        Key: *const ::std::os::raw::c_char,
        KeyLen: size_t,
        Val: LLVMMetadataRef,
    );
}
extern "C" {
    pub fn LLVMDumpModule(M: LLVMModuleRef);
}
extern "C" {
    pub fn LLVMPrintModuleToFile(
        M: LLVMModuleRef,
        Filename: *const ::std::os::raw::c_char,
        ErrorMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMPrintModuleToString(M: LLVMModuleRef) -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetModuleInlineAsm(
        M: LLVMModuleRef,
        Len: *mut size_t,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMSetModuleInlineAsm2(
        M: LLVMModuleRef,
        Asm: *const ::std::os::raw::c_char,
        Len: size_t,
    );
}
extern "C" {
    pub fn LLVMAppendModuleInlineAsm(
        M: LLVMModuleRef,
        Asm: *const ::std::os::raw::c_char,
        Len: size_t,
    );
}
extern "C" {
    pub fn LLVMGetInlineAsm(
        Ty: LLVMTypeRef,
        AsmString: *mut ::std::os::raw::c_char,
        AsmStringSize: size_t,
        Constraints: *mut ::std::os::raw::c_char,
        ConstraintsSize: size_t,
        HasSideEffects: LLVMBool,
        IsAlignStack: LLVMBool,
        Dialect: LLVMInlineAsmDialect,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetModuleContext(M: LLVMModuleRef) -> LLVMContextRef;
}
extern "C" {
    pub fn LLVMGetTypeByName(M: LLVMModuleRef, Name: *const ::std::os::raw::c_char) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMGetFirstNamedMetadata(M: LLVMModuleRef) -> LLVMNamedMDNodeRef;
}
extern "C" {
    pub fn LLVMGetLastNamedMetadata(M: LLVMModuleRef) -> LLVMNamedMDNodeRef;
}
extern "C" {
    pub fn LLVMGetNextNamedMetadata(NamedMDNode: LLVMNamedMDNodeRef) -> LLVMNamedMDNodeRef;
}
extern "C" {
    pub fn LLVMGetPreviousNamedMetadata(NamedMDNode: LLVMNamedMDNodeRef) -> LLVMNamedMDNodeRef;
}
extern "C" {
    pub fn LLVMGetNamedMetadata(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
    ) -> LLVMNamedMDNodeRef;
}
extern "C" {
    pub fn LLVMGetOrInsertNamedMetadata(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
    ) -> LLVMNamedMDNodeRef;
}
extern "C" {
    pub fn LLVMGetNamedMetadataName(
        NamedMD: LLVMNamedMDNodeRef,
        NameLen: *mut size_t,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetNamedMetadataNumOperands(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetNamedMetadataOperands(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
        Dest: *mut LLVMValueRef,
    );
}
extern "C" {
    pub fn LLVMAddNamedMetadataOperand(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
        Val: LLVMValueRef,
    );
}
extern "C" {
    pub fn LLVMGetDebugLocDirectory(
        Val: LLVMValueRef,
        Length: *mut ::std::os::raw::c_uint,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetDebugLocFilename(
        Val: LLVMValueRef,
        Length: *mut ::std::os::raw::c_uint,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetDebugLocLine(Val: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetDebugLocColumn(Val: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMAddFunction(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
        FunctionTy: LLVMTypeRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetNamedFunction(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetFirstFunction(M: LLVMModuleRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetLastFunction(M: LLVMModuleRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetNextFunction(Fn: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetPreviousFunction(Fn: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMSetModuleInlineAsm(M: LLVMModuleRef, Asm: *const ::std::os::raw::c_char);
}
extern "C" {
    pub fn LLVMGetTypeKind(Ty: LLVMTypeRef) -> LLVMTypeKind;
}
extern "C" {
    pub fn LLVMTypeIsSized(Ty: LLVMTypeRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetTypeContext(Ty: LLVMTypeRef) -> LLVMContextRef;
}
extern "C" {
    pub fn LLVMDumpType(Val: LLVMTypeRef);
}
extern "C" {
    pub fn LLVMPrintTypeToString(Val: LLVMTypeRef) -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMInt1TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt8TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt16TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt32TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt64TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt128TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMIntTypeInContext(C: LLVMContextRef, NumBits: ::std::os::raw::c_uint) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt1Type() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt8Type() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt16Type() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt32Type() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt64Type() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMInt128Type() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMIntType(NumBits: ::std::os::raw::c_uint) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMGetIntTypeWidth(IntegerTy: LLVMTypeRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMHalfTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMFloatTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMDoubleTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMX86FP80TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMFP128TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMPPCFP128TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMHalfType() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMFloatType() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMDoubleType() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMX86FP80Type() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMFP128Type() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMPPCFP128Type() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMFunctionType(
        ReturnType: LLVMTypeRef,
        ParamTypes: *mut LLVMTypeRef,
        ParamCount: ::std::os::raw::c_uint,
        IsVarArg: LLVMBool,
    ) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMIsFunctionVarArg(FunctionTy: LLVMTypeRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetReturnType(FunctionTy: LLVMTypeRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMCountParamTypes(FunctionTy: LLVMTypeRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetParamTypes(FunctionTy: LLVMTypeRef, Dest: *mut LLVMTypeRef);
}
extern "C" {
    pub fn LLVMStructTypeInContext(
        C: LLVMContextRef,
        ElementTypes: *mut LLVMTypeRef,
        ElementCount: ::std::os::raw::c_uint,
        Packed: LLVMBool,
    ) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMStructType(
        ElementTypes: *mut LLVMTypeRef,
        ElementCount: ::std::os::raw::c_uint,
        Packed: LLVMBool,
    ) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMStructCreateNamed(
        C: LLVMContextRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMGetStructName(Ty: LLVMTypeRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMStructSetBody(
        StructTy: LLVMTypeRef,
        ElementTypes: *mut LLVMTypeRef,
        ElementCount: ::std::os::raw::c_uint,
        Packed: LLVMBool,
    );
}
extern "C" {
    pub fn LLVMCountStructElementTypes(StructTy: LLVMTypeRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetStructElementTypes(StructTy: LLVMTypeRef, Dest: *mut LLVMTypeRef);
}
extern "C" {
    pub fn LLVMStructGetTypeAtIndex(
        StructTy: LLVMTypeRef,
        i: ::std::os::raw::c_uint,
    ) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMIsPackedStruct(StructTy: LLVMTypeRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMIsOpaqueStruct(StructTy: LLVMTypeRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMIsLiteralStruct(StructTy: LLVMTypeRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetElementType(Ty: LLVMTypeRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMGetSubtypes(Tp: LLVMTypeRef, Arr: *mut LLVMTypeRef);
}
extern "C" {
    pub fn LLVMGetNumContainedTypes(Tp: LLVMTypeRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMArrayType(
        ElementType: LLVMTypeRef,
        ElementCount: ::std::os::raw::c_uint,
    ) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMGetArrayLength(ArrayTy: LLVMTypeRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMPointerType(
        ElementType: LLVMTypeRef,
        AddressSpace: ::std::os::raw::c_uint,
    ) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMGetPointerAddressSpace(PointerTy: LLVMTypeRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMVectorType(
        ElementType: LLVMTypeRef,
        ElementCount: ::std::os::raw::c_uint,
    ) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMGetVectorSize(VectorTy: LLVMTypeRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMVoidTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMLabelTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMX86MMXTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMTokenTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMMetadataTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMVoidType() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMLabelType() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMX86MMXType() -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMTypeOf(Val: LLVMValueRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMGetValueKind(Val: LLVMValueRef) -> LLVMValueKind;
}
extern "C" {
    pub fn LLVMGetValueName2(
        Val: LLVMValueRef,
        Length: *mut size_t,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMSetValueName2(
        Val: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
    );
}
extern "C" {
    pub fn LLVMDumpValue(Val: LLVMValueRef);
}
extern "C" {
    pub fn LLVMPrintValueToString(Val: LLVMValueRef) -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMReplaceAllUsesWith(OldVal: LLVMValueRef, NewVal: LLVMValueRef);
}
extern "C" {
    pub fn LLVMIsConstant(Val: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMIsUndef(Val: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMIsAArgument(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsABasicBlock(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAInlineAsm(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAUser(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstant(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsABlockAddress(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantAggregateZero(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantArray(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantDataSequential(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantDataArray(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantDataVector(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantExpr(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantFP(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantInt(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantPointerNull(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantStruct(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantTokenNone(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAConstantVector(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAGlobalValue(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAGlobalAlias(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAGlobalIFunc(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAGlobalObject(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAFunction(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAGlobalVariable(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAUndefValue(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAInstruction(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAUnaryOperator(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsABinaryOperator(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsACallInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAIntrinsicInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsADbgInfoIntrinsic(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsADbgVariableIntrinsic(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsADbgDeclareInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsADbgLabelInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAMemIntrinsic(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAMemCpyInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAMemMoveInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAMemSetInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsACmpInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAFCmpInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAICmpInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAExtractElementInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAGetElementPtrInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAInsertElementInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAInsertValueInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsALandingPadInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAPHINode(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsASelectInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAShuffleVectorInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAStoreInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsABranchInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAIndirectBrInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAInvokeInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAReturnInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsASwitchInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAUnreachableInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAResumeInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsACleanupReturnInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsACatchReturnInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsACatchSwitchInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsACallBrInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAFuncletPadInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsACatchPadInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsACleanupPadInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAUnaryInstruction(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAAllocaInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsACastInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAAddrSpaceCastInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsABitCastInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAFPExtInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAFPToSIInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAFPToUIInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAFPTruncInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAIntToPtrInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAPtrToIntInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsASExtInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsASIToFPInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsATruncInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAUIToFPInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAZExtInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAExtractValueInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsALoadInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAVAArgInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAFreezeInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAAtomicCmpXchgInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAAtomicRMWInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAFenceInst(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAMDNode(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAMDString(Val: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetValueName(Val: LLVMValueRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMSetValueName(Val: LLVMValueRef, Name: *const ::std::os::raw::c_char);
}
extern "C" {
    pub fn LLVMGetFirstUse(Val: LLVMValueRef) -> LLVMUseRef;
}
extern "C" {
    pub fn LLVMGetNextUse(U: LLVMUseRef) -> LLVMUseRef;
}
extern "C" {
    pub fn LLVMGetUser(U: LLVMUseRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetUsedValue(U: LLVMUseRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetOperand(Val: LLVMValueRef, Index: ::std::os::raw::c_uint) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetOperandUse(Val: LLVMValueRef, Index: ::std::os::raw::c_uint) -> LLVMUseRef;
}
extern "C" {
    pub fn LLVMSetOperand(User: LLVMValueRef, Index: ::std::os::raw::c_uint, Val: LLVMValueRef);
}
extern "C" {
    pub fn LLVMGetNumOperands(Val: LLVMValueRef) -> ::std::os::raw::c_int;
}
extern "C" {
    pub fn LLVMConstNull(Ty: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstAllOnes(Ty: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetUndef(Ty: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsNull(Val: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMConstPointerNull(Ty: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstInt(
        IntTy: LLVMTypeRef,
        N: ::std::os::raw::c_ulonglong,
        SignExtend: LLVMBool,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstIntOfArbitraryPrecision(
        IntTy: LLVMTypeRef,
        NumWords: ::std::os::raw::c_uint,
        Words: *const u64,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstIntOfString(
        IntTy: LLVMTypeRef,
        Text: *const ::std::os::raw::c_char,
        Radix: u8,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstIntOfStringAndSize(
        IntTy: LLVMTypeRef,
        Text: *const ::std::os::raw::c_char,
        SLen: ::std::os::raw::c_uint,
        Radix: u8,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstReal(RealTy: LLVMTypeRef, N: f64) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstRealOfString(
        RealTy: LLVMTypeRef,
        Text: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstRealOfStringAndSize(
        RealTy: LLVMTypeRef,
        Text: *const ::std::os::raw::c_char,
        SLen: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstIntGetZExtValue(ConstantVal: LLVMValueRef) -> ::std::os::raw::c_ulonglong;
}
extern "C" {
    pub fn LLVMConstIntGetSExtValue(ConstantVal: LLVMValueRef) -> ::std::os::raw::c_longlong;
}
extern "C" {
    pub fn LLVMConstRealGetDouble(ConstantVal: LLVMValueRef, losesInfo: *mut LLVMBool) -> f64;
}
extern "C" {
    pub fn LLVMConstStringInContext(
        C: LLVMContextRef,
        Str: *const ::std::os::raw::c_char,
        Length: ::std::os::raw::c_uint,
        DontNullTerminate: LLVMBool,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstString(
        Str: *const ::std::os::raw::c_char,
        Length: ::std::os::raw::c_uint,
        DontNullTerminate: LLVMBool,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsConstantString(c: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetAsString(c: LLVMValueRef, Length: *mut size_t) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMConstStructInContext(
        C: LLVMContextRef,
        ConstantVals: *mut LLVMValueRef,
        Count: ::std::os::raw::c_uint,
        Packed: LLVMBool,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstStruct(
        ConstantVals: *mut LLVMValueRef,
        Count: ::std::os::raw::c_uint,
        Packed: LLVMBool,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstArray(
        ElementTy: LLVMTypeRef,
        ConstantVals: *mut LLVMValueRef,
        Length: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNamedStruct(
        StructTy: LLVMTypeRef,
        ConstantVals: *mut LLVMValueRef,
        Count: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetElementAsConstant(C: LLVMValueRef, idx: ::std::os::raw::c_uint) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstVector(
        ScalarConstantVals: *mut LLVMValueRef,
        Size: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetConstOpcode(ConstantVal: LLVMValueRef) -> LLVMOpcode;
}
extern "C" {
    pub fn LLVMAlignOf(Ty: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMSizeOf(Ty: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNeg(ConstantVal: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNSWNeg(ConstantVal: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNUWNeg(ConstantVal: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFNeg(ConstantVal: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNot(ConstantVal: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstAdd(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNSWAdd(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNUWAdd(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFAdd(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstSub(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNSWSub(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNUWSub(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFSub(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstMul(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNSWMul(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstNUWMul(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFMul(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstUDiv(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstExactUDiv(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef)
        -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstSDiv(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstExactSDiv(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef)
        -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFDiv(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstURem(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstSRem(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFRem(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstAnd(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstOr(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstXor(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstICmp(
        Predicate: LLVMIntPredicate,
        LHSConstant: LLVMValueRef,
        RHSConstant: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFCmp(
        Predicate: LLVMRealPredicate,
        LHSConstant: LLVMValueRef,
        RHSConstant: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstShl(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstLShr(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstAShr(LHSConstant: LLVMValueRef, RHSConstant: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstGEP(
        ConstantVal: LLVMValueRef,
        ConstantIndices: *mut LLVMValueRef,
        NumIndices: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstGEP2(
        Ty: LLVMTypeRef,
        ConstantVal: LLVMValueRef,
        ConstantIndices: *mut LLVMValueRef,
        NumIndices: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstInBoundsGEP(
        ConstantVal: LLVMValueRef,
        ConstantIndices: *mut LLVMValueRef,
        NumIndices: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstInBoundsGEP2(
        Ty: LLVMTypeRef,
        ConstantVal: LLVMValueRef,
        ConstantIndices: *mut LLVMValueRef,
        NumIndices: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstTrunc(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstSExt(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstZExt(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFPTrunc(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFPExt(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstUIToFP(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstSIToFP(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFPToUI(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFPToSI(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstPtrToInt(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstIntToPtr(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstBitCast(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstAddrSpaceCast(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstZExtOrBitCast(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstSExtOrBitCast(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstTruncOrBitCast(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstPointerCast(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstIntCast(
        ConstantVal: LLVMValueRef,
        ToType: LLVMTypeRef,
        isSigned: LLVMBool,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstFPCast(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstSelect(
        ConstantCondition: LLVMValueRef,
        ConstantIfTrue: LLVMValueRef,
        ConstantIfFalse: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstExtractElement(
        VectorConstant: LLVMValueRef,
        IndexConstant: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstInsertElement(
        VectorConstant: LLVMValueRef,
        ElementValueConstant: LLVMValueRef,
        IndexConstant: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstShuffleVector(
        VectorAConstant: LLVMValueRef,
        VectorBConstant: LLVMValueRef,
        MaskConstant: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstExtractValue(
        AggConstant: LLVMValueRef,
        IdxList: *mut ::std::os::raw::c_uint,
        NumIdx: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstInsertValue(
        AggConstant: LLVMValueRef,
        ElementValueConstant: LLVMValueRef,
        IdxList: *mut ::std::os::raw::c_uint,
        NumIdx: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBlockAddress(F: LLVMValueRef, BB: LLVMBasicBlockRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMConstInlineAsm(
        Ty: LLVMTypeRef,
        AsmString: *const ::std::os::raw::c_char,
        Constraints: *const ::std::os::raw::c_char,
        HasSideEffects: LLVMBool,
        IsAlignStack: LLVMBool,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetGlobalParent(Global: LLVMValueRef) -> LLVMModuleRef;
}
extern "C" {
    pub fn LLVMIsDeclaration(Global: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetLinkage(Global: LLVMValueRef) -> LLVMLinkage;
}
extern "C" {
    pub fn LLVMSetLinkage(Global: LLVMValueRef, Linkage: LLVMLinkage);
}
extern "C" {
    pub fn LLVMGetSection(Global: LLVMValueRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMSetSection(Global: LLVMValueRef, Section: *const ::std::os::raw::c_char);
}
extern "C" {
    pub fn LLVMGetVisibility(Global: LLVMValueRef) -> LLVMVisibility;
}
extern "C" {
    pub fn LLVMSetVisibility(Global: LLVMValueRef, Viz: LLVMVisibility);
}
extern "C" {
    pub fn LLVMGetDLLStorageClass(Global: LLVMValueRef) -> LLVMDLLStorageClass;
}
extern "C" {
    pub fn LLVMSetDLLStorageClass(Global: LLVMValueRef, Class: LLVMDLLStorageClass);
}
extern "C" {
    pub fn LLVMGetUnnamedAddress(Global: LLVMValueRef) -> LLVMUnnamedAddr;
}
extern "C" {
    pub fn LLVMSetUnnamedAddress(Global: LLVMValueRef, UnnamedAddr: LLVMUnnamedAddr);
}
extern "C" {
    pub fn LLVMGlobalGetValueType(Global: LLVMValueRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMHasUnnamedAddr(Global: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMSetUnnamedAddr(Global: LLVMValueRef, HasUnnamedAddr: LLVMBool);
}
extern "C" {
    pub fn LLVMGetAlignment(V: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMSetAlignment(V: LLVMValueRef, Bytes: ::std::os::raw::c_uint);
}
extern "C" {
    pub fn LLVMGlobalSetMetadata(
        Global: LLVMValueRef,
        Kind: ::std::os::raw::c_uint,
        MD: LLVMMetadataRef,
    );
}
extern "C" {
    pub fn LLVMGlobalEraseMetadata(Global: LLVMValueRef, Kind: ::std::os::raw::c_uint);
}
extern "C" {
    pub fn LLVMGlobalClearMetadata(Global: LLVMValueRef);
}
extern "C" {
    pub fn LLVMGlobalCopyAllMetadata(
        Value: LLVMValueRef,
        NumEntries: *mut size_t,
    ) -> *mut LLVMValueMetadataEntry;
}
extern "C" {
    pub fn LLVMDisposeValueMetadataEntries(Entries: *mut LLVMValueMetadataEntry);
}
extern "C" {
    pub fn LLVMValueMetadataEntriesGetKind(
        Entries: *mut LLVMValueMetadataEntry,
        Index: ::std::os::raw::c_uint,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMValueMetadataEntriesGetMetadata(
        Entries: *mut LLVMValueMetadataEntry,
        Index: ::std::os::raw::c_uint,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMAddGlobal(
        M: LLVMModuleRef,
        Ty: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMAddGlobalInAddressSpace(
        M: LLVMModuleRef,
        Ty: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
        AddressSpace: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetNamedGlobal(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetFirstGlobal(M: LLVMModuleRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetLastGlobal(M: LLVMModuleRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetNextGlobal(GlobalVar: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetPreviousGlobal(GlobalVar: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMDeleteGlobal(GlobalVar: LLVMValueRef);
}
extern "C" {
    pub fn LLVMGetInitializer(GlobalVar: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMSetInitializer(GlobalVar: LLVMValueRef, ConstantVal: LLVMValueRef);
}
extern "C" {
    pub fn LLVMIsThreadLocal(GlobalVar: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMSetThreadLocal(GlobalVar: LLVMValueRef, IsThreadLocal: LLVMBool);
}
extern "C" {
    pub fn LLVMIsGlobalConstant(GlobalVar: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMSetGlobalConstant(GlobalVar: LLVMValueRef, IsConstant: LLVMBool);
}
extern "C" {
    pub fn LLVMGetThreadLocalMode(GlobalVar: LLVMValueRef) -> LLVMThreadLocalMode;
}
extern "C" {
    pub fn LLVMSetThreadLocalMode(GlobalVar: LLVMValueRef, Mode: LLVMThreadLocalMode);
}
extern "C" {
    pub fn LLVMIsExternallyInitialized(GlobalVar: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMSetExternallyInitialized(GlobalVar: LLVMValueRef, IsExtInit: LLVMBool);
}
extern "C" {
    pub fn LLVMAddAlias(
        M: LLVMModuleRef,
        Ty: LLVMTypeRef,
        Aliasee: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetNamedGlobalAlias(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetFirstGlobalAlias(M: LLVMModuleRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetLastGlobalAlias(M: LLVMModuleRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetNextGlobalAlias(GA: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetPreviousGlobalAlias(GA: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMAliasGetAliasee(Alias: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMAliasSetAliasee(Alias: LLVMValueRef, Aliasee: LLVMValueRef);
}
extern "C" {
    pub fn LLVMDeleteFunction(Fn: LLVMValueRef);
}
extern "C" {
    pub fn LLVMHasPersonalityFn(Fn: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetPersonalityFn(Fn: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMSetPersonalityFn(Fn: LLVMValueRef, PersonalityFn: LLVMValueRef);
}
extern "C" {
    pub fn LLVMLookupIntrinsicID(
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetIntrinsicID(Fn: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetIntrinsicDeclaration(
        Mod: LLVMModuleRef,
        ID: ::std::os::raw::c_uint,
        ParamTypes: *mut LLVMTypeRef,
        ParamCount: size_t,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIntrinsicGetType(
        Ctx: LLVMContextRef,
        ID: ::std::os::raw::c_uint,
        ParamTypes: *mut LLVMTypeRef,
        ParamCount: size_t,
    ) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMIntrinsicGetName(
        ID: ::std::os::raw::c_uint,
        NameLength: *mut size_t,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMIntrinsicCopyOverloadedName(
        ID: ::std::os::raw::c_uint,
        ParamTypes: *mut LLVMTypeRef,
        ParamCount: size_t,
        NameLength: *mut size_t,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMIntrinsicIsOverloaded(ID: ::std::os::raw::c_uint) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetFunctionCallConv(Fn: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMSetFunctionCallConv(Fn: LLVMValueRef, CC: ::std::os::raw::c_uint);
}
extern "C" {
    pub fn LLVMGetGC(Fn: LLVMValueRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMSetGC(Fn: LLVMValueRef, Name: *const ::std::os::raw::c_char);
}
extern "C" {
    pub fn LLVMAddAttributeAtIndex(F: LLVMValueRef, Idx: LLVMAttributeIndex, A: LLVMAttributeRef);
}
extern "C" {
    pub fn LLVMGetAttributeCountAtIndex(
        F: LLVMValueRef,
        Idx: LLVMAttributeIndex,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetAttributesAtIndex(
        F: LLVMValueRef,
        Idx: LLVMAttributeIndex,
        Attrs: *mut LLVMAttributeRef,
    );
}
extern "C" {
    pub fn LLVMGetEnumAttributeAtIndex(
        F: LLVMValueRef,
        Idx: LLVMAttributeIndex,
        KindID: ::std::os::raw::c_uint,
    ) -> LLVMAttributeRef;
}
extern "C" {
    pub fn LLVMGetStringAttributeAtIndex(
        F: LLVMValueRef,
        Idx: LLVMAttributeIndex,
        K: *const ::std::os::raw::c_char,
        KLen: ::std::os::raw::c_uint,
    ) -> LLVMAttributeRef;
}
extern "C" {
    pub fn LLVMRemoveEnumAttributeAtIndex(
        F: LLVMValueRef,
        Idx: LLVMAttributeIndex,
        KindID: ::std::os::raw::c_uint,
    );
}
extern "C" {
    pub fn LLVMRemoveStringAttributeAtIndex(
        F: LLVMValueRef,
        Idx: LLVMAttributeIndex,
        K: *const ::std::os::raw::c_char,
        KLen: ::std::os::raw::c_uint,
    );
}
extern "C" {
    pub fn LLVMAddTargetDependentFunctionAttr(
        Fn: LLVMValueRef,
        A: *const ::std::os::raw::c_char,
        V: *const ::std::os::raw::c_char,
    );
}
extern "C" {
    pub fn LLVMCountParams(Fn: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetParams(Fn: LLVMValueRef, Params: *mut LLVMValueRef);
}
extern "C" {
    pub fn LLVMGetParam(Fn: LLVMValueRef, Index: ::std::os::raw::c_uint) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetParamParent(Inst: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetFirstParam(Fn: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetLastParam(Fn: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetNextParam(Arg: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetPreviousParam(Arg: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMSetParamAlignment(Arg: LLVMValueRef, Align: ::std::os::raw::c_uint);
}
extern "C" {
    pub fn LLVMAddGlobalIFunc(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        Ty: LLVMTypeRef,
        AddrSpace: ::std::os::raw::c_uint,
        Resolver: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetNamedGlobalIFunc(
        M: LLVMModuleRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetFirstGlobalIFunc(M: LLVMModuleRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetLastGlobalIFunc(M: LLVMModuleRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetNextGlobalIFunc(IFunc: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetPreviousGlobalIFunc(IFunc: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetGlobalIFuncResolver(IFunc: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMSetGlobalIFuncResolver(IFunc: LLVMValueRef, Resolver: LLVMValueRef);
}
extern "C" {
    pub fn LLVMEraseGlobalIFunc(IFunc: LLVMValueRef);
}
extern "C" {
    pub fn LLVMRemoveGlobalIFunc(IFunc: LLVMValueRef);
}
extern "C" {
    pub fn LLVMMDStringInContext2(
        C: LLVMContextRef,
        Str: *const ::std::os::raw::c_char,
        SLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMMDNodeInContext2(
        C: LLVMContextRef,
        MDs: *mut LLVMMetadataRef,
        Count: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMMetadataAsValue(C: LLVMContextRef, MD: LLVMMetadataRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMValueAsMetadata(Val: LLVMValueRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMGetMDString(
        V: LLVMValueRef,
        Length: *mut ::std::os::raw::c_uint,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetMDNodeNumOperands(V: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetMDNodeOperands(V: LLVMValueRef, Dest: *mut LLVMValueRef);
}
extern "C" {
    pub fn LLVMMDStringInContext(
        C: LLVMContextRef,
        Str: *const ::std::os::raw::c_char,
        SLen: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMMDString(
        Str: *const ::std::os::raw::c_char,
        SLen: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMMDNodeInContext(
        C: LLVMContextRef,
        Vals: *mut LLVMValueRef,
        Count: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMMDNode(Vals: *mut LLVMValueRef, Count: ::std::os::raw::c_uint) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBasicBlockAsValue(BB: LLVMBasicBlockRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMValueIsBasicBlock(Val: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMValueAsBasicBlock(Val: LLVMValueRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMGetBasicBlockName(BB: LLVMBasicBlockRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetBasicBlockParent(BB: LLVMBasicBlockRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetBasicBlockTerminator(BB: LLVMBasicBlockRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMCountBasicBlocks(Fn: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetBasicBlocks(Fn: LLVMValueRef, BasicBlocks: *mut LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMGetFirstBasicBlock(Fn: LLVMValueRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMGetLastBasicBlock(Fn: LLVMValueRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMGetNextBasicBlock(BB: LLVMBasicBlockRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMGetPreviousBasicBlock(BB: LLVMBasicBlockRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMGetEntryBasicBlock(Fn: LLVMValueRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMInsertExistingBasicBlockAfterInsertBlock(
        Builder: LLVMBuilderRef,
        BB: LLVMBasicBlockRef,
    );
}
extern "C" {
    pub fn LLVMAppendExistingBasicBlock(Fn: LLVMValueRef, BB: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMCreateBasicBlockInContext(
        C: LLVMContextRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMAppendBasicBlockInContext(
        C: LLVMContextRef,
        Fn: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMAppendBasicBlock(
        Fn: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMInsertBasicBlockInContext(
        C: LLVMContextRef,
        BB: LLVMBasicBlockRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMInsertBasicBlock(
        InsertBeforeBB: LLVMBasicBlockRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMDeleteBasicBlock(BB: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMRemoveBasicBlockFromParent(BB: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMMoveBasicBlockBefore(BB: LLVMBasicBlockRef, MovePos: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMMoveBasicBlockAfter(BB: LLVMBasicBlockRef, MovePos: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMGetFirstInstruction(BB: LLVMBasicBlockRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetLastInstruction(BB: LLVMBasicBlockRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMHasMetadata(Val: LLVMValueRef) -> ::std::os::raw::c_int;
}
extern "C" {
    pub fn LLVMGetMetadata(Val: LLVMValueRef, KindID: ::std::os::raw::c_uint) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMSetMetadata(Val: LLVMValueRef, KindID: ::std::os::raw::c_uint, Node: LLVMValueRef);
}
extern "C" {
    pub fn LLVMInstructionGetAllMetadataOtherThanDebugLoc(
        Instr: LLVMValueRef,
        NumEntries: *mut size_t,
    ) -> *mut LLVMValueMetadataEntry;
}
extern "C" {
    pub fn LLVMGetInstructionParent(Inst: LLVMValueRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMGetNextInstruction(Inst: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetPreviousInstruction(Inst: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMInstructionRemoveFromParent(Inst: LLVMValueRef);
}
extern "C" {
    pub fn LLVMInstructionEraseFromParent(Inst: LLVMValueRef);
}
extern "C" {
    pub fn LLVMGetInstructionOpcode(Inst: LLVMValueRef) -> LLVMOpcode;
}
extern "C" {
    pub fn LLVMGetICmpPredicate(Inst: LLVMValueRef) -> LLVMIntPredicate;
}
extern "C" {
    pub fn LLVMGetFCmpPredicate(Inst: LLVMValueRef) -> LLVMRealPredicate;
}
extern "C" {
    pub fn LLVMInstructionClone(Inst: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsATerminatorInst(Inst: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetNumArgOperands(Instr: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMSetInstructionCallConv(Instr: LLVMValueRef, CC: ::std::os::raw::c_uint);
}
extern "C" {
    pub fn LLVMGetInstructionCallConv(Instr: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMSetInstrParamAlignment(
        Instr: LLVMValueRef,
        index: ::std::os::raw::c_uint,
        Align: ::std::os::raw::c_uint,
    );
}
extern "C" {
    pub fn LLVMAddCallSiteAttribute(C: LLVMValueRef, Idx: LLVMAttributeIndex, A: LLVMAttributeRef);
}
extern "C" {
    pub fn LLVMGetCallSiteAttributeCount(
        C: LLVMValueRef,
        Idx: LLVMAttributeIndex,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetCallSiteAttributes(
        C: LLVMValueRef,
        Idx: LLVMAttributeIndex,
        Attrs: *mut LLVMAttributeRef,
    );
}
extern "C" {
    pub fn LLVMGetCallSiteEnumAttribute(
        C: LLVMValueRef,
        Idx: LLVMAttributeIndex,
        KindID: ::std::os::raw::c_uint,
    ) -> LLVMAttributeRef;
}
extern "C" {
    pub fn LLVMGetCallSiteStringAttribute(
        C: LLVMValueRef,
        Idx: LLVMAttributeIndex,
        K: *const ::std::os::raw::c_char,
        KLen: ::std::os::raw::c_uint,
    ) -> LLVMAttributeRef;
}
extern "C" {
    pub fn LLVMRemoveCallSiteEnumAttribute(
        C: LLVMValueRef,
        Idx: LLVMAttributeIndex,
        KindID: ::std::os::raw::c_uint,
    );
}
extern "C" {
    pub fn LLVMRemoveCallSiteStringAttribute(
        C: LLVMValueRef,
        Idx: LLVMAttributeIndex,
        K: *const ::std::os::raw::c_char,
        KLen: ::std::os::raw::c_uint,
    );
}
extern "C" {
    pub fn LLVMGetCalledFunctionType(C: LLVMValueRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMGetCalledValue(Instr: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsTailCall(CallInst: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMSetTailCall(CallInst: LLVMValueRef, IsTailCall: LLVMBool);
}
extern "C" {
    pub fn LLVMGetNormalDest(InvokeInst: LLVMValueRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMGetUnwindDest(InvokeInst: LLVMValueRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMSetNormalDest(InvokeInst: LLVMValueRef, B: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMSetUnwindDest(InvokeInst: LLVMValueRef, B: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMGetNumSuccessors(Term: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetSuccessor(Term: LLVMValueRef, i: ::std::os::raw::c_uint) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMSetSuccessor(
        Term: LLVMValueRef,
        i: ::std::os::raw::c_uint,
        block: LLVMBasicBlockRef,
    );
}
extern "C" {
    pub fn LLVMIsConditional(Branch: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetCondition(Branch: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMSetCondition(Branch: LLVMValueRef, Cond: LLVMValueRef);
}
extern "C" {
    pub fn LLVMGetSwitchDefaultDest(SwitchInstr: LLVMValueRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMGetAllocatedType(Alloca: LLVMValueRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMIsInBounds(GEP: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMSetIsInBounds(GEP: LLVMValueRef, InBounds: LLVMBool);
}
extern "C" {
    pub fn LLVMAddIncoming(
        PhiNode: LLVMValueRef,
        IncomingValues: *mut LLVMValueRef,
        IncomingBlocks: *mut LLVMBasicBlockRef,
        Count: ::std::os::raw::c_uint,
    );
}
extern "C" {
    pub fn LLVMCountIncoming(PhiNode: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetIncomingValue(
        PhiNode: LLVMValueRef,
        Index: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetIncomingBlock(
        PhiNode: LLVMValueRef,
        Index: ::std::os::raw::c_uint,
    ) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMGetNumIndices(Inst: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetIndices(Inst: LLVMValueRef) -> *const ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMCreateBuilderInContext(C: LLVMContextRef) -> LLVMBuilderRef;
}
extern "C" {
    pub fn LLVMCreateBuilder() -> LLVMBuilderRef;
}
extern "C" {
    pub fn LLVMPositionBuilder(
        Builder: LLVMBuilderRef,
        Block: LLVMBasicBlockRef,
        Instr: LLVMValueRef,
    );
}
extern "C" {
    pub fn LLVMPositionBuilderBefore(Builder: LLVMBuilderRef, Instr: LLVMValueRef);
}
extern "C" {
    pub fn LLVMPositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMGetInsertBlock(Builder: LLVMBuilderRef) -> LLVMBasicBlockRef;
}
extern "C" {
    pub fn LLVMClearInsertionPosition(Builder: LLVMBuilderRef);
}
extern "C" {
    pub fn LLVMInsertIntoBuilder(Builder: LLVMBuilderRef, Instr: LLVMValueRef);
}
extern "C" {
    pub fn LLVMInsertIntoBuilderWithName(
        Builder: LLVMBuilderRef,
        Instr: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    );
}
extern "C" {
    pub fn LLVMDisposeBuilder(Builder: LLVMBuilderRef);
}
extern "C" {
    pub fn LLVMGetCurrentDebugLocation2(Builder: LLVMBuilderRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMSetCurrentDebugLocation2(Builder: LLVMBuilderRef, Loc: LLVMMetadataRef);
}
extern "C" {
    pub fn LLVMSetInstDebugLocation(Builder: LLVMBuilderRef, Inst: LLVMValueRef);
}
extern "C" {
    pub fn LLVMBuilderGetDefaultFPMathTag(Builder: LLVMBuilderRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMBuilderSetDefaultFPMathTag(Builder: LLVMBuilderRef, FPMathTag: LLVMMetadataRef);
}
extern "C" {
    pub fn LLVMSetCurrentDebugLocation(Builder: LLVMBuilderRef, L: LLVMValueRef);
}
extern "C" {
    pub fn LLVMGetCurrentDebugLocation(Builder: LLVMBuilderRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildRetVoid(arg1: LLVMBuilderRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildRet(arg1: LLVMBuilderRef, V: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildAggregateRet(
        arg1: LLVMBuilderRef,
        RetVals: *mut LLVMValueRef,
        N: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildBr(arg1: LLVMBuilderRef, Dest: LLVMBasicBlockRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildCondBr(
        arg1: LLVMBuilderRef,
        If: LLVMValueRef,
        Then: LLVMBasicBlockRef,
        Else: LLVMBasicBlockRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildSwitch(
        arg1: LLVMBuilderRef,
        V: LLVMValueRef,
        Else: LLVMBasicBlockRef,
        NumCases: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildIndirectBr(
        B: LLVMBuilderRef,
        Addr: LLVMValueRef,
        NumDests: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildInvoke(
        arg1: LLVMBuilderRef,
        Fn: LLVMValueRef,
        Args: *mut LLVMValueRef,
        NumArgs: ::std::os::raw::c_uint,
        Then: LLVMBasicBlockRef,
        Catch: LLVMBasicBlockRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildInvoke2(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Fn: LLVMValueRef,
        Args: *mut LLVMValueRef,
        NumArgs: ::std::os::raw::c_uint,
        Then: LLVMBasicBlockRef,
        Catch: LLVMBasicBlockRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildUnreachable(arg1: LLVMBuilderRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildResume(B: LLVMBuilderRef, Exn: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildLandingPad(
        B: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        PersFn: LLVMValueRef,
        NumClauses: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildCleanupRet(
        B: LLVMBuilderRef,
        CatchPad: LLVMValueRef,
        BB: LLVMBasicBlockRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildCatchRet(
        B: LLVMBuilderRef,
        CatchPad: LLVMValueRef,
        BB: LLVMBasicBlockRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildCatchPad(
        B: LLVMBuilderRef,
        ParentPad: LLVMValueRef,
        Args: *mut LLVMValueRef,
        NumArgs: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildCleanupPad(
        B: LLVMBuilderRef,
        ParentPad: LLVMValueRef,
        Args: *mut LLVMValueRef,
        NumArgs: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildCatchSwitch(
        B: LLVMBuilderRef,
        ParentPad: LLVMValueRef,
        UnwindBB: LLVMBasicBlockRef,
        NumHandlers: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMAddCase(Switch: LLVMValueRef, OnVal: LLVMValueRef, Dest: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMAddDestination(IndirectBr: LLVMValueRef, Dest: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMGetNumClauses(LandingPad: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetClause(LandingPad: LLVMValueRef, Idx: ::std::os::raw::c_uint) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMAddClause(LandingPad: LLVMValueRef, ClauseVal: LLVMValueRef);
}
extern "C" {
    pub fn LLVMIsCleanup(LandingPad: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMSetCleanup(LandingPad: LLVMValueRef, Val: LLVMBool);
}
extern "C" {
    pub fn LLVMAddHandler(CatchSwitch: LLVMValueRef, Dest: LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMGetNumHandlers(CatchSwitch: LLVMValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetHandlers(CatchSwitch: LLVMValueRef, Handlers: *mut LLVMBasicBlockRef);
}
extern "C" {
    pub fn LLVMGetArgOperand(Funclet: LLVMValueRef, i: ::std::os::raw::c_uint) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMSetArgOperand(Funclet: LLVMValueRef, i: ::std::os::raw::c_uint, value: LLVMValueRef);
}
extern "C" {
    pub fn LLVMGetParentCatchSwitch(CatchPad: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMSetParentCatchSwitch(CatchPad: LLVMValueRef, CatchSwitch: LLVMValueRef);
}
extern "C" {
    pub fn LLVMBuildAdd(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildNSWAdd(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildNUWAdd(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFAdd(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildSub(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildNSWSub(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildNUWSub(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFSub(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildMul(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildNSWMul(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildNUWMul(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFMul(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildUDiv(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildExactUDiv(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildSDiv(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildExactSDiv(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFDiv(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildURem(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildSRem(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFRem(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildShl(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildLShr(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildAShr(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildAnd(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildOr(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildXor(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildBinOp(
        B: LLVMBuilderRef,
        Op: LLVMOpcode,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildNeg(
        arg1: LLVMBuilderRef,
        V: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildNSWNeg(
        B: LLVMBuilderRef,
        V: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildNUWNeg(
        B: LLVMBuilderRef,
        V: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFNeg(
        arg1: LLVMBuilderRef,
        V: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildNot(
        arg1: LLVMBuilderRef,
        V: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildMalloc(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildArrayMalloc(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Val: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildMemSet(
        B: LLVMBuilderRef,
        Ptr: LLVMValueRef,
        Val: LLVMValueRef,
        Len: LLVMValueRef,
        Align: ::std::os::raw::c_uint,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildMemCpy(
        B: LLVMBuilderRef,
        Dst: LLVMValueRef,
        DstAlign: ::std::os::raw::c_uint,
        Src: LLVMValueRef,
        SrcAlign: ::std::os::raw::c_uint,
        Size: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildMemMove(
        B: LLVMBuilderRef,
        Dst: LLVMValueRef,
        DstAlign: ::std::os::raw::c_uint,
        Src: LLVMValueRef,
        SrcAlign: ::std::os::raw::c_uint,
        Size: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildAlloca(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildArrayAlloca(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Val: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFree(arg1: LLVMBuilderRef, PointerVal: LLVMValueRef) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildLoad(
        arg1: LLVMBuilderRef,
        PointerVal: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildLoad2(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        PointerVal: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildStore(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        Ptr: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildGEP(
        B: LLVMBuilderRef,
        Pointer: LLVMValueRef,
        Indices: *mut LLVMValueRef,
        NumIndices: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildInBoundsGEP(
        B: LLVMBuilderRef,
        Pointer: LLVMValueRef,
        Indices: *mut LLVMValueRef,
        NumIndices: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildStructGEP(
        B: LLVMBuilderRef,
        Pointer: LLVMValueRef,
        Idx: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildGEP2(
        B: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Pointer: LLVMValueRef,
        Indices: *mut LLVMValueRef,
        NumIndices: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildInBoundsGEP2(
        B: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Pointer: LLVMValueRef,
        Indices: *mut LLVMValueRef,
        NumIndices: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildStructGEP2(
        B: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Pointer: LLVMValueRef,
        Idx: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildGlobalString(
        B: LLVMBuilderRef,
        Str: *const ::std::os::raw::c_char,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildGlobalStringPtr(
        B: LLVMBuilderRef,
        Str: *const ::std::os::raw::c_char,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMGetVolatile(MemoryAccessInst: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMSetVolatile(MemoryAccessInst: LLVMValueRef, IsVolatile: LLVMBool);
}
extern "C" {
    pub fn LLVMGetWeak(CmpXchgInst: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMSetWeak(CmpXchgInst: LLVMValueRef, IsWeak: LLVMBool);
}
extern "C" {
    pub fn LLVMGetOrdering(MemoryAccessInst: LLVMValueRef) -> LLVMAtomicOrdering;
}
extern "C" {
    pub fn LLVMSetOrdering(MemoryAccessInst: LLVMValueRef, Ordering: LLVMAtomicOrdering);
}
extern "C" {
    pub fn LLVMGetAtomicRMWBinOp(AtomicRMWInst: LLVMValueRef) -> LLVMAtomicRMWBinOp;
}
extern "C" {
    pub fn LLVMSetAtomicRMWBinOp(AtomicRMWInst: LLVMValueRef, BinOp: LLVMAtomicRMWBinOp);
}
extern "C" {
    pub fn LLVMBuildTrunc(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildZExt(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildSExt(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFPToUI(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFPToSI(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildUIToFP(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildSIToFP(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFPTrunc(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFPExt(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildPtrToInt(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildIntToPtr(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildBitCast(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildAddrSpaceCast(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildZExtOrBitCast(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildSExtOrBitCast(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildTruncOrBitCast(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildCast(
        B: LLVMBuilderRef,
        Op: LLVMOpcode,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildPointerCast(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildIntCast2(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        IsSigned: LLVMBool,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFPCast(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildIntCast(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildICmp(
        arg1: LLVMBuilderRef,
        Op: LLVMIntPredicate,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFCmp(
        arg1: LLVMBuilderRef,
        Op: LLVMRealPredicate,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildPhi(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildCall(
        arg1: LLVMBuilderRef,
        Fn: LLVMValueRef,
        Args: *mut LLVMValueRef,
        NumArgs: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildCall2(
        arg1: LLVMBuilderRef,
        arg2: LLVMTypeRef,
        Fn: LLVMValueRef,
        Args: *mut LLVMValueRef,
        NumArgs: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildSelect(
        arg1: LLVMBuilderRef,
        If: LLVMValueRef,
        Then: LLVMValueRef,
        Else: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildVAArg(
        arg1: LLVMBuilderRef,
        List: LLVMValueRef,
        Ty: LLVMTypeRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildExtractElement(
        arg1: LLVMBuilderRef,
        VecVal: LLVMValueRef,
        Index: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildInsertElement(
        arg1: LLVMBuilderRef,
        VecVal: LLVMValueRef,
        EltVal: LLVMValueRef,
        Index: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildShuffleVector(
        arg1: LLVMBuilderRef,
        V1: LLVMValueRef,
        V2: LLVMValueRef,
        Mask: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildExtractValue(
        arg1: LLVMBuilderRef,
        AggVal: LLVMValueRef,
        Index: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildInsertValue(
        arg1: LLVMBuilderRef,
        AggVal: LLVMValueRef,
        EltVal: LLVMValueRef,
        Index: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFreeze(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildIsNull(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildIsNotNull(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildPtrDiff(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildFence(
        B: LLVMBuilderRef,
        ordering: LLVMAtomicOrdering,
        singleThread: LLVMBool,
        Name: *const ::std::os::raw::c_char,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildAtomicRMW(
        B: LLVMBuilderRef,
        op: LLVMAtomicRMWBinOp,
        PTR: LLVMValueRef,
        Val: LLVMValueRef,
        ordering: LLVMAtomicOrdering,
        singleThread: LLVMBool,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMBuildAtomicCmpXchg(
        B: LLVMBuilderRef,
        Ptr: LLVMValueRef,
        Cmp: LLVMValueRef,
        New: LLVMValueRef,
        SuccessOrdering: LLVMAtomicOrdering,
        FailureOrdering: LLVMAtomicOrdering,
        SingleThread: LLVMBool,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMIsAtomicSingleThread(AtomicInst: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMSetAtomicSingleThread(AtomicInst: LLVMValueRef, SingleThread: LLVMBool);
}
extern "C" {
    pub fn LLVMGetCmpXchgSuccessOrdering(CmpXchgInst: LLVMValueRef) -> LLVMAtomicOrdering;
}
extern "C" {
    pub fn LLVMSetCmpXchgSuccessOrdering(CmpXchgInst: LLVMValueRef, Ordering: LLVMAtomicOrdering);
}
extern "C" {
    pub fn LLVMGetCmpXchgFailureOrdering(CmpXchgInst: LLVMValueRef) -> LLVMAtomicOrdering;
}
extern "C" {
    pub fn LLVMSetCmpXchgFailureOrdering(CmpXchgInst: LLVMValueRef, Ordering: LLVMAtomicOrdering);
}
extern "C" {
    pub fn LLVMCreateModuleProviderForExistingModule(M: LLVMModuleRef) -> LLVMModuleProviderRef;
}
extern "C" {
    pub fn LLVMDisposeModuleProvider(M: LLVMModuleProviderRef);
}
extern "C" {
    pub fn LLVMCreateMemoryBufferWithContentsOfFile(
        Path: *const ::std::os::raw::c_char,
        OutMemBuf: *mut LLVMMemoryBufferRef,
        OutMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMCreateMemoryBufferWithSTDIN(
        OutMemBuf: *mut LLVMMemoryBufferRef,
        OutMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMCreateMemoryBufferWithMemoryRange(
        InputData: *const ::std::os::raw::c_char,
        InputDataLength: size_t,
        BufferName: *const ::std::os::raw::c_char,
        RequiresNullTerminator: LLVMBool,
    ) -> LLVMMemoryBufferRef;
}
extern "C" {
    pub fn LLVMCreateMemoryBufferWithMemoryRangeCopy(
        InputData: *const ::std::os::raw::c_char,
        InputDataLength: size_t,
        BufferName: *const ::std::os::raw::c_char,
    ) -> LLVMMemoryBufferRef;
}
extern "C" {
    pub fn LLVMGetBufferStart(MemBuf: LLVMMemoryBufferRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetBufferSize(MemBuf: LLVMMemoryBufferRef) -> size_t;
}
extern "C" {
    pub fn LLVMDisposeMemoryBuffer(MemBuf: LLVMMemoryBufferRef);
}
extern "C" {
    pub fn LLVMGetGlobalPassRegistry() -> LLVMPassRegistryRef;
}
extern "C" {
    pub fn LLVMCreatePassManager() -> LLVMPassManagerRef;
}
extern "C" {
    pub fn LLVMCreateFunctionPassManagerForModule(M: LLVMModuleRef) -> LLVMPassManagerRef;
}
extern "C" {
    pub fn LLVMCreateFunctionPassManager(MP: LLVMModuleProviderRef) -> LLVMPassManagerRef;
}
extern "C" {
    pub fn LLVMRunPassManager(PM: LLVMPassManagerRef, M: LLVMModuleRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMInitializeFunctionPassManager(FPM: LLVMPassManagerRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMRunFunctionPassManager(FPM: LLVMPassManagerRef, F: LLVMValueRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMFinalizeFunctionPassManager(FPM: LLVMPassManagerRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMDisposePassManager(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMStartMultithreaded() -> LLVMBool;
}
extern "C" {
    pub fn LLVMStopMultithreaded();
}
extern "C" {
    pub fn LLVMIsMultithreaded() -> LLVMBool;
}
pub const LLVMDIFlags_LLVMDIFlagZero: LLVMDIFlags = 0;
pub const LLVMDIFlags_LLVMDIFlagPrivate: LLVMDIFlags = 1;
pub const LLVMDIFlags_LLVMDIFlagProtected: LLVMDIFlags = 2;
pub const LLVMDIFlags_LLVMDIFlagPublic: LLVMDIFlags = 3;
pub const LLVMDIFlags_LLVMDIFlagFwdDecl: LLVMDIFlags = 4;
pub const LLVMDIFlags_LLVMDIFlagAppleBlock: LLVMDIFlags = 8;
pub const LLVMDIFlags_LLVMDIFlagReservedBit4: LLVMDIFlags = 16;
pub const LLVMDIFlags_LLVMDIFlagVirtual: LLVMDIFlags = 32;
pub const LLVMDIFlags_LLVMDIFlagArtificial: LLVMDIFlags = 64;
pub const LLVMDIFlags_LLVMDIFlagExplicit: LLVMDIFlags = 128;
pub const LLVMDIFlags_LLVMDIFlagPrototyped: LLVMDIFlags = 256;
pub const LLVMDIFlags_LLVMDIFlagObjcClassComplete: LLVMDIFlags = 512;
pub const LLVMDIFlags_LLVMDIFlagObjectPointer: LLVMDIFlags = 1024;
pub const LLVMDIFlags_LLVMDIFlagVector: LLVMDIFlags = 2048;
pub const LLVMDIFlags_LLVMDIFlagStaticMember: LLVMDIFlags = 4096;
pub const LLVMDIFlags_LLVMDIFlagLValueReference: LLVMDIFlags = 8192;
pub const LLVMDIFlags_LLVMDIFlagRValueReference: LLVMDIFlags = 16384;
pub const LLVMDIFlags_LLVMDIFlagReserved: LLVMDIFlags = 32768;
pub const LLVMDIFlags_LLVMDIFlagSingleInheritance: LLVMDIFlags = 65536;
pub const LLVMDIFlags_LLVMDIFlagMultipleInheritance: LLVMDIFlags = 131072;
pub const LLVMDIFlags_LLVMDIFlagVirtualInheritance: LLVMDIFlags = 196608;
pub const LLVMDIFlags_LLVMDIFlagIntroducedVirtual: LLVMDIFlags = 262144;
pub const LLVMDIFlags_LLVMDIFlagBitField: LLVMDIFlags = 524288;
pub const LLVMDIFlags_LLVMDIFlagNoReturn: LLVMDIFlags = 1048576;
pub const LLVMDIFlags_LLVMDIFlagTypePassByValue: LLVMDIFlags = 4194304;
pub const LLVMDIFlags_LLVMDIFlagTypePassByReference: LLVMDIFlags = 8388608;
pub const LLVMDIFlags_LLVMDIFlagEnumClass: LLVMDIFlags = 16777216;
pub const LLVMDIFlags_LLVMDIFlagFixedEnum: LLVMDIFlags = 16777216;
pub const LLVMDIFlags_LLVMDIFlagThunk: LLVMDIFlags = 33554432;
pub const LLVMDIFlags_LLVMDIFlagNonTrivial: LLVMDIFlags = 67108864;
pub const LLVMDIFlags_LLVMDIFlagBigEndian: LLVMDIFlags = 134217728;
pub const LLVMDIFlags_LLVMDIFlagLittleEndian: LLVMDIFlags = 268435456;
pub const LLVMDIFlags_LLVMDIFlagIndirectVirtualBase: LLVMDIFlags = 36;
pub const LLVMDIFlags_LLVMDIFlagAccessibility: LLVMDIFlags = 3;
pub const LLVMDIFlags_LLVMDIFlagPtrToMemberRep: LLVMDIFlags = 196608;
pub type LLVMDIFlags = u32;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageC89: LLVMDWARFSourceLanguage = 0;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageC: LLVMDWARFSourceLanguage = 1;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageAda83: LLVMDWARFSourceLanguage = 2;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageC_plus_plus: LLVMDWARFSourceLanguage = 3;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageCobol74: LLVMDWARFSourceLanguage = 4;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageCobol85: LLVMDWARFSourceLanguage = 5;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageFortran77: LLVMDWARFSourceLanguage = 6;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageFortran90: LLVMDWARFSourceLanguage = 7;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguagePascal83: LLVMDWARFSourceLanguage = 8;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageModula2: LLVMDWARFSourceLanguage = 9;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageJava: LLVMDWARFSourceLanguage = 10;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageC99: LLVMDWARFSourceLanguage = 11;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageAda95: LLVMDWARFSourceLanguage = 12;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageFortran95: LLVMDWARFSourceLanguage = 13;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguagePLI: LLVMDWARFSourceLanguage = 14;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageObjC: LLVMDWARFSourceLanguage = 15;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageObjC_plus_plus: LLVMDWARFSourceLanguage =
    16;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageUPC: LLVMDWARFSourceLanguage = 17;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageD: LLVMDWARFSourceLanguage = 18;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguagePython: LLVMDWARFSourceLanguage = 19;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageOpenCL: LLVMDWARFSourceLanguage = 20;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageGo: LLVMDWARFSourceLanguage = 21;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageModula3: LLVMDWARFSourceLanguage = 22;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageHaskell: LLVMDWARFSourceLanguage = 23;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageC_plus_plus_03: LLVMDWARFSourceLanguage =
    24;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageC_plus_plus_11: LLVMDWARFSourceLanguage =
    25;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageOCaml: LLVMDWARFSourceLanguage = 26;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageRust: LLVMDWARFSourceLanguage = 27;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageC11: LLVMDWARFSourceLanguage = 28;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageSwift: LLVMDWARFSourceLanguage = 29;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageJulia: LLVMDWARFSourceLanguage = 30;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageDylan: LLVMDWARFSourceLanguage = 31;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageC_plus_plus_14: LLVMDWARFSourceLanguage =
    32;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageFortran03: LLVMDWARFSourceLanguage = 33;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageFortran08: LLVMDWARFSourceLanguage = 34;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageRenderScript: LLVMDWARFSourceLanguage = 35;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageBLISS: LLVMDWARFSourceLanguage = 36;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageMips_Assembler: LLVMDWARFSourceLanguage =
    37;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageGOOGLE_RenderScript:
    LLVMDWARFSourceLanguage = 38;
pub const LLVMDWARFSourceLanguage_LLVMDWARFSourceLanguageBORLAND_Delphi: LLVMDWARFSourceLanguage =
    39;
pub type LLVMDWARFSourceLanguage = u32;
pub const LLVMDWARFEmissionKind_LLVMDWARFEmissionNone: LLVMDWARFEmissionKind = 0;
pub const LLVMDWARFEmissionKind_LLVMDWARFEmissionFull: LLVMDWARFEmissionKind = 1;
pub const LLVMDWARFEmissionKind_LLVMDWARFEmissionLineTablesOnly: LLVMDWARFEmissionKind = 2;
pub type LLVMDWARFEmissionKind = u32;
pub const LLVMMDStringMetadataKind: _bindgen_ty_3 = 0;
pub const LLVMConstantAsMetadataMetadataKind: _bindgen_ty_3 = 1;
pub const LLVMLocalAsMetadataMetadataKind: _bindgen_ty_3 = 2;
pub const LLVMDistinctMDOperandPlaceholderMetadataKind: _bindgen_ty_3 = 3;
pub const LLVMMDTupleMetadataKind: _bindgen_ty_3 = 4;
pub const LLVMDILocationMetadataKind: _bindgen_ty_3 = 5;
pub const LLVMDIExpressionMetadataKind: _bindgen_ty_3 = 6;
pub const LLVMDIGlobalVariableExpressionMetadataKind: _bindgen_ty_3 = 7;
pub const LLVMGenericDINodeMetadataKind: _bindgen_ty_3 = 8;
pub const LLVMDISubrangeMetadataKind: _bindgen_ty_3 = 9;
pub const LLVMDIEnumeratorMetadataKind: _bindgen_ty_3 = 10;
pub const LLVMDIBasicTypeMetadataKind: _bindgen_ty_3 = 11;
pub const LLVMDIDerivedTypeMetadataKind: _bindgen_ty_3 = 12;
pub const LLVMDICompositeTypeMetadataKind: _bindgen_ty_3 = 13;
pub const LLVMDISubroutineTypeMetadataKind: _bindgen_ty_3 = 14;
pub const LLVMDIFileMetadataKind: _bindgen_ty_3 = 15;
pub const LLVMDICompileUnitMetadataKind: _bindgen_ty_3 = 16;
pub const LLVMDISubprogramMetadataKind: _bindgen_ty_3 = 17;
pub const LLVMDILexicalBlockMetadataKind: _bindgen_ty_3 = 18;
pub const LLVMDILexicalBlockFileMetadataKind: _bindgen_ty_3 = 19;
pub const LLVMDINamespaceMetadataKind: _bindgen_ty_3 = 20;
pub const LLVMDIModuleMetadataKind: _bindgen_ty_3 = 21;
pub const LLVMDITemplateTypeParameterMetadataKind: _bindgen_ty_3 = 22;
pub const LLVMDITemplateValueParameterMetadataKind: _bindgen_ty_3 = 23;
pub const LLVMDIGlobalVariableMetadataKind: _bindgen_ty_3 = 24;
pub const LLVMDILocalVariableMetadataKind: _bindgen_ty_3 = 25;
pub const LLVMDILabelMetadataKind: _bindgen_ty_3 = 26;
pub const LLVMDIObjCPropertyMetadataKind: _bindgen_ty_3 = 27;
pub const LLVMDIImportedEntityMetadataKind: _bindgen_ty_3 = 28;
pub const LLVMDIMacroMetadataKind: _bindgen_ty_3 = 29;
pub const LLVMDIMacroFileMetadataKind: _bindgen_ty_3 = 30;
pub const LLVMDICommonBlockMetadataKind: _bindgen_ty_3 = 31;
pub type _bindgen_ty_3 = u32;
pub type LLVMMetadataKind = ::std::os::raw::c_uint;
pub type LLVMDWARFTypeEncoding = ::std::os::raw::c_uint;
pub const LLVMDWARFMacinfoRecordType_LLVMDWARFMacinfoRecordTypeDefine: LLVMDWARFMacinfoRecordType =
    1;
pub const LLVMDWARFMacinfoRecordType_LLVMDWARFMacinfoRecordTypeMacro: LLVMDWARFMacinfoRecordType =
    2;
pub const LLVMDWARFMacinfoRecordType_LLVMDWARFMacinfoRecordTypeStartFile:
    LLVMDWARFMacinfoRecordType = 3;
pub const LLVMDWARFMacinfoRecordType_LLVMDWARFMacinfoRecordTypeEndFile: LLVMDWARFMacinfoRecordType =
    4;
pub const LLVMDWARFMacinfoRecordType_LLVMDWARFMacinfoRecordTypeVendorExt:
    LLVMDWARFMacinfoRecordType = 255;
pub type LLVMDWARFMacinfoRecordType = u32;
extern "C" {
    pub fn LLVMDebugMetadataVersion() -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGetModuleDebugMetadataVersion(Module: LLVMModuleRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMStripModuleDebugInfo(Module: LLVMModuleRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMCreateDIBuilderDisallowUnresolved(M: LLVMModuleRef) -> LLVMDIBuilderRef;
}
extern "C" {
    pub fn LLVMCreateDIBuilder(M: LLVMModuleRef) -> LLVMDIBuilderRef;
}
extern "C" {
    pub fn LLVMDisposeDIBuilder(Builder: LLVMDIBuilderRef);
}
extern "C" {
    pub fn LLVMDIBuilderFinalize(Builder: LLVMDIBuilderRef);
}
extern "C" {
    pub fn LLVMDIBuilderCreateCompileUnit(
        Builder: LLVMDIBuilderRef,
        Lang: LLVMDWARFSourceLanguage,
        FileRef: LLVMMetadataRef,
        Producer: *const ::std::os::raw::c_char,
        ProducerLen: size_t,
        isOptimized: LLVMBool,
        Flags: *const ::std::os::raw::c_char,
        FlagsLen: size_t,
        RuntimeVer: ::std::os::raw::c_uint,
        SplitName: *const ::std::os::raw::c_char,
        SplitNameLen: size_t,
        Kind: LLVMDWARFEmissionKind,
        DWOId: ::std::os::raw::c_uint,
        SplitDebugInlining: LLVMBool,
        DebugInfoForProfiling: LLVMBool,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateFile(
        Builder: LLVMDIBuilderRef,
        Filename: *const ::std::os::raw::c_char,
        FilenameLen: size_t,
        Directory: *const ::std::os::raw::c_char,
        DirectoryLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateModule(
        Builder: LLVMDIBuilderRef,
        ParentScope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        ConfigMacros: *const ::std::os::raw::c_char,
        ConfigMacrosLen: size_t,
        IncludePath: *const ::std::os::raw::c_char,
        IncludePathLen: size_t,
        SysRoot: *const ::std::os::raw::c_char,
        SysRootLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateNameSpace(
        Builder: LLVMDIBuilderRef,
        ParentScope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        ExportSymbols: LLVMBool,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateFunction(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        LinkageName: *const ::std::os::raw::c_char,
        LinkageNameLen: size_t,
        File: LLVMMetadataRef,
        LineNo: ::std::os::raw::c_uint,
        Ty: LLVMMetadataRef,
        IsLocalToUnit: LLVMBool,
        IsDefinition: LLVMBool,
        ScopeLine: ::std::os::raw::c_uint,
        Flags: LLVMDIFlags,
        IsOptimized: LLVMBool,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateLexicalBlock(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        File: LLVMMetadataRef,
        Line: ::std::os::raw::c_uint,
        Column: ::std::os::raw::c_uint,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateLexicalBlockFile(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        File: LLVMMetadataRef,
        Discriminator: ::std::os::raw::c_uint,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateImportedModuleFromNamespace(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        NS: LLVMMetadataRef,
        File: LLVMMetadataRef,
        Line: ::std::os::raw::c_uint,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateImportedModuleFromAlias(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        ImportedEntity: LLVMMetadataRef,
        File: LLVMMetadataRef,
        Line: ::std::os::raw::c_uint,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateImportedModuleFromModule(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        M: LLVMMetadataRef,
        File: LLVMMetadataRef,
        Line: ::std::os::raw::c_uint,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateImportedDeclaration(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Decl: LLVMMetadataRef,
        File: LLVMMetadataRef,
        Line: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateDebugLocation(
        Ctx: LLVMContextRef,
        Line: ::std::os::raw::c_uint,
        Column: ::std::os::raw::c_uint,
        Scope: LLVMMetadataRef,
        InlinedAt: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDILocationGetLine(Location: LLVMMetadataRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMDILocationGetColumn(Location: LLVMMetadataRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMDILocationGetScope(Location: LLVMMetadataRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDILocationGetInlinedAt(Location: LLVMMetadataRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIScopeGetFile(Scope: LLVMMetadataRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIFileGetDirectory(
        File: LLVMMetadataRef,
        Len: *mut ::std::os::raw::c_uint,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMDIFileGetFilename(
        File: LLVMMetadataRef,
        Len: *mut ::std::os::raw::c_uint,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMDIFileGetSource(
        File: LLVMMetadataRef,
        Len: *mut ::std::os::raw::c_uint,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMDIBuilderGetOrCreateTypeArray(
        Builder: LLVMDIBuilderRef,
        Data: *mut LLVMMetadataRef,
        NumElements: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateSubroutineType(
        Builder: LLVMDIBuilderRef,
        File: LLVMMetadataRef,
        ParameterTypes: *mut LLVMMetadataRef,
        NumParameterTypes: ::std::os::raw::c_uint,
        Flags: LLVMDIFlags,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateMacro(
        Builder: LLVMDIBuilderRef,
        ParentMacroFile: LLVMMetadataRef,
        Line: ::std::os::raw::c_uint,
        RecordType: LLVMDWARFMacinfoRecordType,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        Value: *const ::std::os::raw::c_char,
        ValueLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateTempMacroFile(
        Builder: LLVMDIBuilderRef,
        ParentMacroFile: LLVMMetadataRef,
        Line: ::std::os::raw::c_uint,
        File: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateEnumerator(
        Builder: LLVMDIBuilderRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        Value: i64,
        IsUnsigned: LLVMBool,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateEnumerationType(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNumber: ::std::os::raw::c_uint,
        SizeInBits: u64,
        AlignInBits: u32,
        Elements: *mut LLVMMetadataRef,
        NumElements: ::std::os::raw::c_uint,
        ClassTy: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateUnionType(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNumber: ::std::os::raw::c_uint,
        SizeInBits: u64,
        AlignInBits: u32,
        Flags: LLVMDIFlags,
        Elements: *mut LLVMMetadataRef,
        NumElements: ::std::os::raw::c_uint,
        RunTimeLang: ::std::os::raw::c_uint,
        UniqueId: *const ::std::os::raw::c_char,
        UniqueIdLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateArrayType(
        Builder: LLVMDIBuilderRef,
        Size: u64,
        AlignInBits: u32,
        Ty: LLVMMetadataRef,
        Subscripts: *mut LLVMMetadataRef,
        NumSubscripts: ::std::os::raw::c_uint,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateVectorType(
        Builder: LLVMDIBuilderRef,
        Size: u64,
        AlignInBits: u32,
        Ty: LLVMMetadataRef,
        Subscripts: *mut LLVMMetadataRef,
        NumSubscripts: ::std::os::raw::c_uint,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateUnspecifiedType(
        Builder: LLVMDIBuilderRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateBasicType(
        Builder: LLVMDIBuilderRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        SizeInBits: u64,
        Encoding: LLVMDWARFTypeEncoding,
        Flags: LLVMDIFlags,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreatePointerType(
        Builder: LLVMDIBuilderRef,
        PointeeTy: LLVMMetadataRef,
        SizeInBits: u64,
        AlignInBits: u32,
        AddressSpace: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateStructType(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNumber: ::std::os::raw::c_uint,
        SizeInBits: u64,
        AlignInBits: u32,
        Flags: LLVMDIFlags,
        DerivedFrom: LLVMMetadataRef,
        Elements: *mut LLVMMetadataRef,
        NumElements: ::std::os::raw::c_uint,
        RunTimeLang: ::std::os::raw::c_uint,
        VTableHolder: LLVMMetadataRef,
        UniqueId: *const ::std::os::raw::c_char,
        UniqueIdLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateMemberType(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNo: ::std::os::raw::c_uint,
        SizeInBits: u64,
        AlignInBits: u32,
        OffsetInBits: u64,
        Flags: LLVMDIFlags,
        Ty: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateStaticMemberType(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNumber: ::std::os::raw::c_uint,
        Type: LLVMMetadataRef,
        Flags: LLVMDIFlags,
        ConstantVal: LLVMValueRef,
        AlignInBits: u32,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateMemberPointerType(
        Builder: LLVMDIBuilderRef,
        PointeeType: LLVMMetadataRef,
        ClassType: LLVMMetadataRef,
        SizeInBits: u64,
        AlignInBits: u32,
        Flags: LLVMDIFlags,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateObjCIVar(
        Builder: LLVMDIBuilderRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNo: ::std::os::raw::c_uint,
        SizeInBits: u64,
        AlignInBits: u32,
        OffsetInBits: u64,
        Flags: LLVMDIFlags,
        Ty: LLVMMetadataRef,
        PropertyNode: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateObjCProperty(
        Builder: LLVMDIBuilderRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNo: ::std::os::raw::c_uint,
        GetterName: *const ::std::os::raw::c_char,
        GetterNameLen: size_t,
        SetterName: *const ::std::os::raw::c_char,
        SetterNameLen: size_t,
        PropertyAttributes: ::std::os::raw::c_uint,
        Ty: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateObjectPointerType(
        Builder: LLVMDIBuilderRef,
        Type: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateQualifiedType(
        Builder: LLVMDIBuilderRef,
        Tag: ::std::os::raw::c_uint,
        Type: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateReferenceType(
        Builder: LLVMDIBuilderRef,
        Tag: ::std::os::raw::c_uint,
        Type: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateNullPtrType(Builder: LLVMDIBuilderRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateTypedef(
        Builder: LLVMDIBuilderRef,
        Type: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNo: ::std::os::raw::c_uint,
        Scope: LLVMMetadataRef,
        AlignInBits: u32,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateInheritance(
        Builder: LLVMDIBuilderRef,
        Ty: LLVMMetadataRef,
        BaseTy: LLVMMetadataRef,
        BaseOffset: u64,
        VBPtrOffset: u32,
        Flags: LLVMDIFlags,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateForwardDecl(
        Builder: LLVMDIBuilderRef,
        Tag: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        Scope: LLVMMetadataRef,
        File: LLVMMetadataRef,
        Line: ::std::os::raw::c_uint,
        RuntimeLang: ::std::os::raw::c_uint,
        SizeInBits: u64,
        AlignInBits: u32,
        UniqueIdentifier: *const ::std::os::raw::c_char,
        UniqueIdentifierLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateReplaceableCompositeType(
        Builder: LLVMDIBuilderRef,
        Tag: ::std::os::raw::c_uint,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        Scope: LLVMMetadataRef,
        File: LLVMMetadataRef,
        Line: ::std::os::raw::c_uint,
        RuntimeLang: ::std::os::raw::c_uint,
        SizeInBits: u64,
        AlignInBits: u32,
        Flags: LLVMDIFlags,
        UniqueIdentifier: *const ::std::os::raw::c_char,
        UniqueIdentifierLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateBitFieldMemberType(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNumber: ::std::os::raw::c_uint,
        SizeInBits: u64,
        OffsetInBits: u64,
        StorageOffsetInBits: u64,
        Flags: LLVMDIFlags,
        Type: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateClassType(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNumber: ::std::os::raw::c_uint,
        SizeInBits: u64,
        AlignInBits: u32,
        OffsetInBits: u64,
        Flags: LLVMDIFlags,
        DerivedFrom: LLVMMetadataRef,
        Elements: *mut LLVMMetadataRef,
        NumElements: ::std::os::raw::c_uint,
        VTableHolder: LLVMMetadataRef,
        TemplateParamsNode: LLVMMetadataRef,
        UniqueIdentifier: *const ::std::os::raw::c_char,
        UniqueIdentifierLen: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateArtificialType(
        Builder: LLVMDIBuilderRef,
        Type: LLVMMetadataRef,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDITypeGetName(
        DType: LLVMMetadataRef,
        Length: *mut size_t,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMDITypeGetSizeInBits(DType: LLVMMetadataRef) -> u64;
}
extern "C" {
    pub fn LLVMDITypeGetOffsetInBits(DType: LLVMMetadataRef) -> u64;
}
extern "C" {
    pub fn LLVMDITypeGetAlignInBits(DType: LLVMMetadataRef) -> u32;
}
extern "C" {
    pub fn LLVMDITypeGetLine(DType: LLVMMetadataRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMDITypeGetFlags(DType: LLVMMetadataRef) -> LLVMDIFlags;
}
extern "C" {
    pub fn LLVMDIBuilderGetOrCreateSubrange(
        Builder: LLVMDIBuilderRef,
        LowerBound: i64,
        Count: i64,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderGetOrCreateArray(
        Builder: LLVMDIBuilderRef,
        Data: *mut LLVMMetadataRef,
        NumElements: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateExpression(
        Builder: LLVMDIBuilderRef,
        Addr: *mut i64,
        Length: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateConstantValueExpression(
        Builder: LLVMDIBuilderRef,
        Value: i64,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateGlobalVariableExpression(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        Linkage: *const ::std::os::raw::c_char,
        LinkLen: size_t,
        File: LLVMMetadataRef,
        LineNo: ::std::os::raw::c_uint,
        Ty: LLVMMetadataRef,
        LocalToUnit: LLVMBool,
        Expr: LLVMMetadataRef,
        Decl: LLVMMetadataRef,
        AlignInBits: u32,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIGlobalVariableExpressionGetVariable(GVE: LLVMMetadataRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIGlobalVariableExpressionGetExpression(GVE: LLVMMetadataRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIVariableGetFile(Var: LLVMMetadataRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIVariableGetScope(Var: LLVMMetadataRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIVariableGetLine(Var: LLVMMetadataRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMTemporaryMDNode(
        Ctx: LLVMContextRef,
        Data: *mut LLVMMetadataRef,
        NumElements: size_t,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDisposeTemporaryMDNode(TempNode: LLVMMetadataRef);
}
extern "C" {
    pub fn LLVMMetadataReplaceAllUsesWith(
        TempTargetMetadata: LLVMMetadataRef,
        Replacement: LLVMMetadataRef,
    );
}
extern "C" {
    pub fn LLVMDIBuilderCreateTempGlobalVariableFwdDecl(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        Linkage: *const ::std::os::raw::c_char,
        LnkLen: size_t,
        File: LLVMMetadataRef,
        LineNo: ::std::os::raw::c_uint,
        Ty: LLVMMetadataRef,
        LocalToUnit: LLVMBool,
        Decl: LLVMMetadataRef,
        AlignInBits: u32,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderInsertDeclareBefore(
        Builder: LLVMDIBuilderRef,
        Storage: LLVMValueRef,
        VarInfo: LLVMMetadataRef,
        Expr: LLVMMetadataRef,
        DebugLoc: LLVMMetadataRef,
        Instr: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMDIBuilderInsertDeclareAtEnd(
        Builder: LLVMDIBuilderRef,
        Storage: LLVMValueRef,
        VarInfo: LLVMMetadataRef,
        Expr: LLVMMetadataRef,
        DebugLoc: LLVMMetadataRef,
        Block: LLVMBasicBlockRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMDIBuilderInsertDbgValueBefore(
        Builder: LLVMDIBuilderRef,
        Val: LLVMValueRef,
        VarInfo: LLVMMetadataRef,
        Expr: LLVMMetadataRef,
        DebugLoc: LLVMMetadataRef,
        Instr: LLVMValueRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMDIBuilderInsertDbgValueAtEnd(
        Builder: LLVMDIBuilderRef,
        Val: LLVMValueRef,
        VarInfo: LLVMMetadataRef,
        Expr: LLVMMetadataRef,
        DebugLoc: LLVMMetadataRef,
        Block: LLVMBasicBlockRef,
    ) -> LLVMValueRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateAutoVariable(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        File: LLVMMetadataRef,
        LineNo: ::std::os::raw::c_uint,
        Ty: LLVMMetadataRef,
        AlwaysPreserve: LLVMBool,
        Flags: LLVMDIFlags,
        AlignInBits: u32,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMDIBuilderCreateParameterVariable(
        Builder: LLVMDIBuilderRef,
        Scope: LLVMMetadataRef,
        Name: *const ::std::os::raw::c_char,
        NameLen: size_t,
        ArgNo: ::std::os::raw::c_uint,
        File: LLVMMetadataRef,
        LineNo: ::std::os::raw::c_uint,
        Ty: LLVMMetadataRef,
        AlwaysPreserve: LLVMBool,
        Flags: LLVMDIFlags,
    ) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMGetSubprogram(Func: LLVMValueRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMSetSubprogram(Func: LLVMValueRef, SP: LLVMMetadataRef);
}
extern "C" {
    pub fn LLVMDISubprogramGetLine(Subprogram: LLVMMetadataRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMInstructionGetDebugLoc(Inst: LLVMValueRef) -> LLVMMetadataRef;
}
extern "C" {
    pub fn LLVMInstructionSetDebugLoc(Inst: LLVMValueRef, Loc: LLVMMetadataRef);
}
extern "C" {
    pub fn LLVMGetMetadataKind(Metadata: LLVMMetadataRef) -> LLVMMetadataKind;
}
pub type LLVMDisasmContextRef = *mut ::std::os::raw::c_void;
pub type LLVMOpInfoCallback = ::std::option::Option<
    unsafe extern "C" fn(
        DisInfo: *mut ::std::os::raw::c_void,
        PC: u64,
        Offset: u64,
        Size: u64,
        TagType: ::std::os::raw::c_int,
        TagBuf: *mut ::std::os::raw::c_void,
    ) -> ::std::os::raw::c_int,
>;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpInfoSymbol1 {
    pub Present: u64,
    pub Name: *const ::std::os::raw::c_char,
    pub Value: u64,
}
#[test]
fn bindgen_test_layout_LLVMOpInfoSymbol1() {
    assert_eq!(
        ::std::mem::size_of::<LLVMOpInfoSymbol1>(),
        24usize,
        concat!("Size of: ", stringify!(LLVMOpInfoSymbol1))
    );
    assert_eq!(
        ::std::mem::align_of::<LLVMOpInfoSymbol1>(),
        8usize,
        concat!("Alignment of ", stringify!(LLVMOpInfoSymbol1))
    );
    assert_eq!(
        unsafe { &(*(::std::ptr::null::<LLVMOpInfoSymbol1>())).Present as *const _ as usize },
        0usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMOpInfoSymbol1),
            "::",
            stringify!(Present)
        )
    );
    assert_eq!(
        unsafe { &(*(::std::ptr::null::<LLVMOpInfoSymbol1>())).Name as *const _ as usize },
        8usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMOpInfoSymbol1),
            "::",
            stringify!(Name)
        )
    );
    assert_eq!(
        unsafe { &(*(::std::ptr::null::<LLVMOpInfoSymbol1>())).Value as *const _ as usize },
        16usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMOpInfoSymbol1),
            "::",
            stringify!(Value)
        )
    );
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpInfo1 {
    pub AddSymbol: LLVMOpInfoSymbol1,
    pub SubtractSymbol: LLVMOpInfoSymbol1,
    pub Value: u64,
    pub VariantKind: u64,
}
#[test]
fn bindgen_test_layout_LLVMOpInfo1() {
    assert_eq!(
        ::std::mem::size_of::<LLVMOpInfo1>(),
        64usize,
        concat!("Size of: ", stringify!(LLVMOpInfo1))
    );
    assert_eq!(
        ::std::mem::align_of::<LLVMOpInfo1>(),
        8usize,
        concat!("Alignment of ", stringify!(LLVMOpInfo1))
    );
    assert_eq!(
        unsafe { &(*(::std::ptr::null::<LLVMOpInfo1>())).AddSymbol as *const _ as usize },
        0usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMOpInfo1),
            "::",
            stringify!(AddSymbol)
        )
    );
    assert_eq!(
        unsafe { &(*(::std::ptr::null::<LLVMOpInfo1>())).SubtractSymbol as *const _ as usize },
        24usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMOpInfo1),
            "::",
            stringify!(SubtractSymbol)
        )
    );
    assert_eq!(
        unsafe { &(*(::std::ptr::null::<LLVMOpInfo1>())).Value as *const _ as usize },
        48usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMOpInfo1),
            "::",
            stringify!(Value)
        )
    );
    assert_eq!(
        unsafe { &(*(::std::ptr::null::<LLVMOpInfo1>())).VariantKind as *const _ as usize },
        56usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMOpInfo1),
            "::",
            stringify!(VariantKind)
        )
    );
}
pub type LLVMSymbolLookupCallback = ::std::option::Option<
    unsafe extern "C" fn(
        DisInfo: *mut ::std::os::raw::c_void,
        ReferenceValue: u64,
        ReferenceType: *mut u64,
        ReferencePC: u64,
        ReferenceName: *mut *const ::std::os::raw::c_char,
    ) -> *const ::std::os::raw::c_char,
>;
extern "C" {
    pub fn LLVMCreateDisasm(
        TripleName: *const ::std::os::raw::c_char,
        DisInfo: *mut ::std::os::raw::c_void,
        TagType: ::std::os::raw::c_int,
        GetOpInfo: LLVMOpInfoCallback,
        SymbolLookUp: LLVMSymbolLookupCallback,
    ) -> LLVMDisasmContextRef;
}
extern "C" {
    pub fn LLVMCreateDisasmCPU(
        Triple: *const ::std::os::raw::c_char,
        CPU: *const ::std::os::raw::c_char,
        DisInfo: *mut ::std::os::raw::c_void,
        TagType: ::std::os::raw::c_int,
        GetOpInfo: LLVMOpInfoCallback,
        SymbolLookUp: LLVMSymbolLookupCallback,
    ) -> LLVMDisasmContextRef;
}
extern "C" {
    pub fn LLVMCreateDisasmCPUFeatures(
        Triple: *const ::std::os::raw::c_char,
        CPU: *const ::std::os::raw::c_char,
        Features: *const ::std::os::raw::c_char,
        DisInfo: *mut ::std::os::raw::c_void,
        TagType: ::std::os::raw::c_int,
        GetOpInfo: LLVMOpInfoCallback,
        SymbolLookUp: LLVMSymbolLookupCallback,
    ) -> LLVMDisasmContextRef;
}
extern "C" {
    pub fn LLVMSetDisasmOptions(DC: LLVMDisasmContextRef, Options: u64) -> ::std::os::raw::c_int;
}
extern "C" {
    pub fn LLVMDisasmDispose(DC: LLVMDisasmContextRef);
}
extern "C" {
    pub fn LLVMDisasmInstruction(
        DC: LLVMDisasmContextRef,
        Bytes: *mut u8,
        BytesSize: u64,
        PC: u64,
        OutString: *mut ::std::os::raw::c_char,
        OutStringSize: size_t,
    ) -> size_t;
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueError {
    _unused: [u8; 0],
}
pub type LLVMErrorRef = *mut LLVMOpaqueError;
pub type LLVMErrorTypeId = *const ::std::os::raw::c_void;
extern "C" {
    pub fn LLVMGetErrorTypeId(Err: LLVMErrorRef) -> LLVMErrorTypeId;
}
extern "C" {
    pub fn LLVMConsumeError(Err: LLVMErrorRef);
}
extern "C" {
    pub fn LLVMGetErrorMessage(Err: LLVMErrorRef) -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMDisposeErrorMessage(ErrMsg: *mut ::std::os::raw::c_char);
}
extern "C" {
    pub fn LLVMGetStringErrorTypeId() -> LLVMErrorTypeId;
}
pub const LLVMByteOrdering_LLVMBigEndian: LLVMByteOrdering = 0;
pub const LLVMByteOrdering_LLVMLittleEndian: LLVMByteOrdering = 1;
pub type LLVMByteOrdering = u32;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueTargetData {
    _unused: [u8; 0],
}
pub type LLVMTargetDataRef = *mut LLVMOpaqueTargetData;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueTargetLibraryInfotData {
    _unused: [u8; 0],
}
pub type LLVMTargetLibraryInfoRef = *mut LLVMOpaqueTargetLibraryInfotData;
extern "C" {
    pub fn LLVMInitializeAArch64TargetInfo();
}
extern "C" {
    pub fn LLVMInitializeAMDGPUTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeARMTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeBPFTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeHexagonTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeLanaiTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeMipsTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeMSP430TargetInfo();
}
extern "C" {
    pub fn LLVMInitializeNVPTXTargetInfo();
}
extern "C" {
    pub fn LLVMInitializePowerPCTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeRISCVTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeSparcTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeSystemZTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeWebAssemblyTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeX86TargetInfo();
}
extern "C" {
    pub fn LLVMInitializeXCoreTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeAVRTargetInfo();
}
extern "C" {
    pub fn LLVMInitializeAArch64Target();
}
extern "C" {
    pub fn LLVMInitializeAMDGPUTarget();
}
extern "C" {
    pub fn LLVMInitializeARMTarget();
}
extern "C" {
    pub fn LLVMInitializeBPFTarget();
}
extern "C" {
    pub fn LLVMInitializeHexagonTarget();
}
extern "C" {
    pub fn LLVMInitializeLanaiTarget();
}
extern "C" {
    pub fn LLVMInitializeMipsTarget();
}
extern "C" {
    pub fn LLVMInitializeMSP430Target();
}
extern "C" {
    pub fn LLVMInitializeNVPTXTarget();
}
extern "C" {
    pub fn LLVMInitializePowerPCTarget();
}
extern "C" {
    pub fn LLVMInitializeRISCVTarget();
}
extern "C" {
    pub fn LLVMInitializeSparcTarget();
}
extern "C" {
    pub fn LLVMInitializeSystemZTarget();
}
extern "C" {
    pub fn LLVMInitializeWebAssemblyTarget();
}
extern "C" {
    pub fn LLVMInitializeX86Target();
}
extern "C" {
    pub fn LLVMInitializeXCoreTarget();
}
extern "C" {
    pub fn LLVMInitializeAVRTarget();
}
extern "C" {
    pub fn LLVMInitializeAArch64TargetMC();
}
extern "C" {
    pub fn LLVMInitializeAMDGPUTargetMC();
}
extern "C" {
    pub fn LLVMInitializeARMTargetMC();
}
extern "C" {
    pub fn LLVMInitializeBPFTargetMC();
}
extern "C" {
    pub fn LLVMInitializeHexagonTargetMC();
}
extern "C" {
    pub fn LLVMInitializeLanaiTargetMC();
}
extern "C" {
    pub fn LLVMInitializeMipsTargetMC();
}
extern "C" {
    pub fn LLVMInitializeMSP430TargetMC();
}
extern "C" {
    pub fn LLVMInitializeNVPTXTargetMC();
}
extern "C" {
    pub fn LLVMInitializePowerPCTargetMC();
}
extern "C" {
    pub fn LLVMInitializeRISCVTargetMC();
}
extern "C" {
    pub fn LLVMInitializeSparcTargetMC();
}
extern "C" {
    pub fn LLVMInitializeSystemZTargetMC();
}
extern "C" {
    pub fn LLVMInitializeWebAssemblyTargetMC();
}
extern "C" {
    pub fn LLVMInitializeX86TargetMC();
}
extern "C" {
    pub fn LLVMInitializeXCoreTargetMC();
}
extern "C" {
    pub fn LLVMInitializeAVRTargetMC();
}
extern "C" {
    pub fn LLVMInitializeAArch64AsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeAMDGPUAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeARMAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeBPFAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeHexagonAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeLanaiAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeMipsAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeMSP430AsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeNVPTXAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializePowerPCAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeRISCVAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeSparcAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeSystemZAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeWebAssemblyAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeX86AsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeXCoreAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeAVRAsmPrinter();
}
extern "C" {
    pub fn LLVMInitializeAArch64AsmParser();
}
extern "C" {
    pub fn LLVMInitializeAMDGPUAsmParser();
}
extern "C" {
    pub fn LLVMInitializeARMAsmParser();
}
extern "C" {
    pub fn LLVMInitializeBPFAsmParser();
}
extern "C" {
    pub fn LLVMInitializeHexagonAsmParser();
}
extern "C" {
    pub fn LLVMInitializeLanaiAsmParser();
}
extern "C" {
    pub fn LLVMInitializeMipsAsmParser();
}
extern "C" {
    pub fn LLVMInitializeMSP430AsmParser();
}
extern "C" {
    pub fn LLVMInitializePowerPCAsmParser();
}
extern "C" {
    pub fn LLVMInitializeRISCVAsmParser();
}
extern "C" {
    pub fn LLVMInitializeSparcAsmParser();
}
extern "C" {
    pub fn LLVMInitializeSystemZAsmParser();
}
extern "C" {
    pub fn LLVMInitializeWebAssemblyAsmParser();
}
extern "C" {
    pub fn LLVMInitializeX86AsmParser();
}
extern "C" {
    pub fn LLVMInitializeAVRAsmParser();
}
extern "C" {
    pub fn LLVMInitializeAArch64Disassembler();
}
extern "C" {
    pub fn LLVMInitializeAMDGPUDisassembler();
}
extern "C" {
    pub fn LLVMInitializeARMDisassembler();
}
extern "C" {
    pub fn LLVMInitializeBPFDisassembler();
}
extern "C" {
    pub fn LLVMInitializeHexagonDisassembler();
}
extern "C" {
    pub fn LLVMInitializeLanaiDisassembler();
}
extern "C" {
    pub fn LLVMInitializeMipsDisassembler();
}
extern "C" {
    pub fn LLVMInitializeMSP430Disassembler();
}
extern "C" {
    pub fn LLVMInitializePowerPCDisassembler();
}
extern "C" {
    pub fn LLVMInitializeRISCVDisassembler();
}
extern "C" {
    pub fn LLVMInitializeSparcDisassembler();
}
extern "C" {
    pub fn LLVMInitializeSystemZDisassembler();
}
extern "C" {
    pub fn LLVMInitializeWebAssemblyDisassembler();
}
extern "C" {
    pub fn LLVMInitializeX86Disassembler();
}
extern "C" {
    pub fn LLVMInitializeXCoreDisassembler();
}
extern "C" {
    pub fn LLVMInitializeAVRDisassembler();
}
extern "C" {
    pub fn LLVMGetModuleDataLayout(M: LLVMModuleRef) -> LLVMTargetDataRef;
}
extern "C" {
    pub fn LLVMSetModuleDataLayout(M: LLVMModuleRef, DL: LLVMTargetDataRef);
}
extern "C" {
    pub fn LLVMCreateTargetData(StringRep: *const ::std::os::raw::c_char) -> LLVMTargetDataRef;
}
extern "C" {
    pub fn LLVMDisposeTargetData(TD: LLVMTargetDataRef);
}
extern "C" {
    pub fn LLVMAddTargetLibraryInfo(TLI: LLVMTargetLibraryInfoRef, PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMCopyStringRepOfTargetData(TD: LLVMTargetDataRef) -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMByteOrder(TD: LLVMTargetDataRef) -> LLVMByteOrdering;
}
extern "C" {
    pub fn LLVMPointerSize(TD: LLVMTargetDataRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMPointerSizeForAS(
        TD: LLVMTargetDataRef,
        AS: ::std::os::raw::c_uint,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMIntPtrType(TD: LLVMTargetDataRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMIntPtrTypeForAS(TD: LLVMTargetDataRef, AS: ::std::os::raw::c_uint) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMIntPtrTypeInContext(C: LLVMContextRef, TD: LLVMTargetDataRef) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMIntPtrTypeForASInContext(
        C: LLVMContextRef,
        TD: LLVMTargetDataRef,
        AS: ::std::os::raw::c_uint,
    ) -> LLVMTypeRef;
}
extern "C" {
    pub fn LLVMSizeOfTypeInBits(
        TD: LLVMTargetDataRef,
        Ty: LLVMTypeRef,
    ) -> ::std::os::raw::c_ulonglong;
}
extern "C" {
    pub fn LLVMStoreSizeOfType(
        TD: LLVMTargetDataRef,
        Ty: LLVMTypeRef,
    ) -> ::std::os::raw::c_ulonglong;
}
extern "C" {
    pub fn LLVMABISizeOfType(TD: LLVMTargetDataRef, Ty: LLVMTypeRef)
        -> ::std::os::raw::c_ulonglong;
}
extern "C" {
    pub fn LLVMABIAlignmentOfType(TD: LLVMTargetDataRef, Ty: LLVMTypeRef)
        -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMCallFrameAlignmentOfType(
        TD: LLVMTargetDataRef,
        Ty: LLVMTypeRef,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMPreferredAlignmentOfType(
        TD: LLVMTargetDataRef,
        Ty: LLVMTypeRef,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMPreferredAlignmentOfGlobal(
        TD: LLVMTargetDataRef,
        GlobalVar: LLVMValueRef,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMElementAtOffset(
        TD: LLVMTargetDataRef,
        StructTy: LLVMTypeRef,
        Offset: ::std::os::raw::c_ulonglong,
    ) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMOffsetOfElement(
        TD: LLVMTargetDataRef,
        StructTy: LLVMTypeRef,
        Element: ::std::os::raw::c_uint,
    ) -> ::std::os::raw::c_ulonglong;
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueTargetMachine {
    _unused: [u8; 0],
}
pub type LLVMTargetMachineRef = *mut LLVMOpaqueTargetMachine;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMTarget {
    _unused: [u8; 0],
}
pub type LLVMTargetRef = *mut LLVMTarget;
pub const LLVMCodeGenOptLevel_LLVMCodeGenLevelNone: LLVMCodeGenOptLevel = 0;
pub const LLVMCodeGenOptLevel_LLVMCodeGenLevelLess: LLVMCodeGenOptLevel = 1;
pub const LLVMCodeGenOptLevel_LLVMCodeGenLevelDefault: LLVMCodeGenOptLevel = 2;
pub const LLVMCodeGenOptLevel_LLVMCodeGenLevelAggressive: LLVMCodeGenOptLevel = 3;
pub type LLVMCodeGenOptLevel = u32;
pub const LLVMRelocMode_LLVMRelocDefault: LLVMRelocMode = 0;
pub const LLVMRelocMode_LLVMRelocStatic: LLVMRelocMode = 1;
pub const LLVMRelocMode_LLVMRelocPIC: LLVMRelocMode = 2;
pub const LLVMRelocMode_LLVMRelocDynamicNoPic: LLVMRelocMode = 3;
pub const LLVMRelocMode_LLVMRelocROPI: LLVMRelocMode = 4;
pub const LLVMRelocMode_LLVMRelocRWPI: LLVMRelocMode = 5;
pub const LLVMRelocMode_LLVMRelocROPI_RWPI: LLVMRelocMode = 6;
pub type LLVMRelocMode = u32;
pub const LLVMCodeModel_LLVMCodeModelDefault: LLVMCodeModel = 0;
pub const LLVMCodeModel_LLVMCodeModelJITDefault: LLVMCodeModel = 1;
pub const LLVMCodeModel_LLVMCodeModelTiny: LLVMCodeModel = 2;
pub const LLVMCodeModel_LLVMCodeModelSmall: LLVMCodeModel = 3;
pub const LLVMCodeModel_LLVMCodeModelKernel: LLVMCodeModel = 4;
pub const LLVMCodeModel_LLVMCodeModelMedium: LLVMCodeModel = 5;
pub const LLVMCodeModel_LLVMCodeModelLarge: LLVMCodeModel = 6;
pub type LLVMCodeModel = u32;
pub const LLVMCodeGenFileType_LLVMAssemblyFile: LLVMCodeGenFileType = 0;
pub const LLVMCodeGenFileType_LLVMObjectFile: LLVMCodeGenFileType = 1;
pub type LLVMCodeGenFileType = u32;
extern "C" {
    pub fn LLVMGetFirstTarget() -> LLVMTargetRef;
}
extern "C" {
    pub fn LLVMGetNextTarget(T: LLVMTargetRef) -> LLVMTargetRef;
}
extern "C" {
    pub fn LLVMGetTargetFromName(Name: *const ::std::os::raw::c_char) -> LLVMTargetRef;
}
extern "C" {
    pub fn LLVMGetTargetFromTriple(
        Triple: *const ::std::os::raw::c_char,
        T: *mut LLVMTargetRef,
        ErrorMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetTargetName(T: LLVMTargetRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetTargetDescription(T: LLVMTargetRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMTargetHasJIT(T: LLVMTargetRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMTargetHasTargetMachine(T: LLVMTargetRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMTargetHasAsmBackend(T: LLVMTargetRef) -> LLVMBool;
}
extern "C" {
    pub fn LLVMCreateTargetMachine(
        T: LLVMTargetRef,
        Triple: *const ::std::os::raw::c_char,
        CPU: *const ::std::os::raw::c_char,
        Features: *const ::std::os::raw::c_char,
        Level: LLVMCodeGenOptLevel,
        Reloc: LLVMRelocMode,
        CodeModel: LLVMCodeModel,
    ) -> LLVMTargetMachineRef;
}
extern "C" {
    pub fn LLVMDisposeTargetMachine(T: LLVMTargetMachineRef);
}
extern "C" {
    pub fn LLVMGetTargetMachineTarget(T: LLVMTargetMachineRef) -> LLVMTargetRef;
}
extern "C" {
    pub fn LLVMGetTargetMachineTriple(T: LLVMTargetMachineRef) -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetTargetMachineCPU(T: LLVMTargetMachineRef) -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetTargetMachineFeatureString(
        T: LLVMTargetMachineRef,
    ) -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMCreateTargetDataLayout(T: LLVMTargetMachineRef) -> LLVMTargetDataRef;
}
extern "C" {
    pub fn LLVMSetTargetMachineAsmVerbosity(T: LLVMTargetMachineRef, VerboseAsm: LLVMBool);
}
extern "C" {
    pub fn LLVMTargetMachineEmitToFile(
        T: LLVMTargetMachineRef,
        M: LLVMModuleRef,
        Filename: *mut ::std::os::raw::c_char,
        codegen: LLVMCodeGenFileType,
        ErrorMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMTargetMachineEmitToMemoryBuffer(
        T: LLVMTargetMachineRef,
        M: LLVMModuleRef,
        codegen: LLVMCodeGenFileType,
        ErrorMessage: *mut *mut ::std::os::raw::c_char,
        OutMemBuf: *mut LLVMMemoryBufferRef,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetDefaultTargetTriple() -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMNormalizeTargetTriple(
        triple: *const ::std::os::raw::c_char,
    ) -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetHostCPUName() -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetHostCPUFeatures() -> *mut ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMAddAnalysisPasses(T: LLVMTargetMachineRef, PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMLinkInMCJIT();
}
extern "C" {
    pub fn LLVMLinkInInterpreter();
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueGenericValue {
    _unused: [u8; 0],
}
pub type LLVMGenericValueRef = *mut LLVMOpaqueGenericValue;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueExecutionEngine {
    _unused: [u8; 0],
}
pub type LLVMExecutionEngineRef = *mut LLVMOpaqueExecutionEngine;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueMCJITMemoryManager {
    _unused: [u8; 0],
}
pub type LLVMMCJITMemoryManagerRef = *mut LLVMOpaqueMCJITMemoryManager;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMMCJITCompilerOptions {
    pub OptLevel: ::std::os::raw::c_uint,
    pub CodeModel: LLVMCodeModel,
    pub NoFramePointerElim: LLVMBool,
    pub EnableFastISel: LLVMBool,
    pub MCJMM: LLVMMCJITMemoryManagerRef,
}
#[test]
fn bindgen_test_layout_LLVMMCJITCompilerOptions() {
    assert_eq!(
        ::std::mem::size_of::<LLVMMCJITCompilerOptions>(),
        24usize,
        concat!("Size of: ", stringify!(LLVMMCJITCompilerOptions))
    );
    assert_eq!(
        ::std::mem::align_of::<LLVMMCJITCompilerOptions>(),
        8usize,
        concat!("Alignment of ", stringify!(LLVMMCJITCompilerOptions))
    );
    assert_eq!(
        unsafe {
            &(*(::std::ptr::null::<LLVMMCJITCompilerOptions>())).OptLevel as *const _ as usize
        },
        0usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMMCJITCompilerOptions),
            "::",
            stringify!(OptLevel)
        )
    );
    assert_eq!(
        unsafe {
            &(*(::std::ptr::null::<LLVMMCJITCompilerOptions>())).CodeModel as *const _ as usize
        },
        4usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMMCJITCompilerOptions),
            "::",
            stringify!(CodeModel)
        )
    );
    assert_eq!(
        unsafe {
            &(*(::std::ptr::null::<LLVMMCJITCompilerOptions>())).NoFramePointerElim as *const _
                as usize
        },
        8usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMMCJITCompilerOptions),
            "::",
            stringify!(NoFramePointerElim)
        )
    );
    assert_eq!(
        unsafe {
            &(*(::std::ptr::null::<LLVMMCJITCompilerOptions>())).EnableFastISel as *const _ as usize
        },
        12usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMMCJITCompilerOptions),
            "::",
            stringify!(EnableFastISel)
        )
    );
    assert_eq!(
        unsafe { &(*(::std::ptr::null::<LLVMMCJITCompilerOptions>())).MCJMM as *const _ as usize },
        16usize,
        concat!(
            "Offset of field: ",
            stringify!(LLVMMCJITCompilerOptions),
            "::",
            stringify!(MCJMM)
        )
    );
}
extern "C" {
    pub fn LLVMCreateGenericValueOfInt(
        Ty: LLVMTypeRef,
        N: ::std::os::raw::c_ulonglong,
        IsSigned: LLVMBool,
    ) -> LLVMGenericValueRef;
}
extern "C" {
    pub fn LLVMCreateGenericValueOfPointer(P: *mut ::std::os::raw::c_void) -> LLVMGenericValueRef;
}
extern "C" {
    pub fn LLVMCreateGenericValueOfFloat(Ty: LLVMTypeRef, N: f64) -> LLVMGenericValueRef;
}
extern "C" {
    pub fn LLVMGenericValueIntWidth(GenValRef: LLVMGenericValueRef) -> ::std::os::raw::c_uint;
}
extern "C" {
    pub fn LLVMGenericValueToInt(
        GenVal: LLVMGenericValueRef,
        IsSigned: LLVMBool,
    ) -> ::std::os::raw::c_ulonglong;
}
extern "C" {
    pub fn LLVMGenericValueToPointer(GenVal: LLVMGenericValueRef) -> *mut ::std::os::raw::c_void;
}
extern "C" {
    pub fn LLVMGenericValueToFloat(TyRef: LLVMTypeRef, GenVal: LLVMGenericValueRef) -> f64;
}
extern "C" {
    pub fn LLVMDisposeGenericValue(GenVal: LLVMGenericValueRef);
}
extern "C" {
    pub fn LLVMCreateExecutionEngineForModule(
        OutEE: *mut LLVMExecutionEngineRef,
        M: LLVMModuleRef,
        OutError: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMCreateInterpreterForModule(
        OutInterp: *mut LLVMExecutionEngineRef,
        M: LLVMModuleRef,
        OutError: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMCreateJITCompilerForModule(
        OutJIT: *mut LLVMExecutionEngineRef,
        M: LLVMModuleRef,
        OptLevel: ::std::os::raw::c_uint,
        OutError: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMInitializeMCJITCompilerOptions(
        Options: *mut LLVMMCJITCompilerOptions,
        SizeOfOptions: size_t,
    );
}
extern "C" {
    pub fn LLVMCreateMCJITCompilerForModule(
        OutJIT: *mut LLVMExecutionEngineRef,
        M: LLVMModuleRef,
        Options: *mut LLVMMCJITCompilerOptions,
        SizeOfOptions: size_t,
        OutError: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMDisposeExecutionEngine(EE: LLVMExecutionEngineRef);
}
extern "C" {
    pub fn LLVMRunStaticConstructors(EE: LLVMExecutionEngineRef);
}
extern "C" {
    pub fn LLVMRunStaticDestructors(EE: LLVMExecutionEngineRef);
}
extern "C" {
    pub fn LLVMRunFunctionAsMain(
        EE: LLVMExecutionEngineRef,
        F: LLVMValueRef,
        ArgC: ::std::os::raw::c_uint,
        ArgV: *const *const ::std::os::raw::c_char,
        EnvP: *const *const ::std::os::raw::c_char,
    ) -> ::std::os::raw::c_int;
}
extern "C" {
    pub fn LLVMRunFunction(
        EE: LLVMExecutionEngineRef,
        F: LLVMValueRef,
        NumArgs: ::std::os::raw::c_uint,
        Args: *mut LLVMGenericValueRef,
    ) -> LLVMGenericValueRef;
}
extern "C" {
    pub fn LLVMFreeMachineCodeForFunction(EE: LLVMExecutionEngineRef, F: LLVMValueRef);
}
extern "C" {
    pub fn LLVMAddModule(EE: LLVMExecutionEngineRef, M: LLVMModuleRef);
}
extern "C" {
    pub fn LLVMRemoveModule(
        EE: LLVMExecutionEngineRef,
        M: LLVMModuleRef,
        OutMod: *mut LLVMModuleRef,
        OutError: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMFindFunction(
        EE: LLVMExecutionEngineRef,
        Name: *const ::std::os::raw::c_char,
        OutFn: *mut LLVMValueRef,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMRecompileAndRelinkFunction(
        EE: LLVMExecutionEngineRef,
        Fn: LLVMValueRef,
    ) -> *mut ::std::os::raw::c_void;
}
extern "C" {
    pub fn LLVMGetExecutionEngineTargetData(EE: LLVMExecutionEngineRef) -> LLVMTargetDataRef;
}
extern "C" {
    pub fn LLVMGetExecutionEngineTargetMachine(EE: LLVMExecutionEngineRef) -> LLVMTargetMachineRef;
}
extern "C" {
    pub fn LLVMAddGlobalMapping(
        EE: LLVMExecutionEngineRef,
        Global: LLVMValueRef,
        Addr: *mut ::std::os::raw::c_void,
    );
}
extern "C" {
    pub fn LLVMGetPointerToGlobal(
        EE: LLVMExecutionEngineRef,
        Global: LLVMValueRef,
    ) -> *mut ::std::os::raw::c_void;
}
extern "C" {
    pub fn LLVMGetGlobalValueAddress(
        EE: LLVMExecutionEngineRef,
        Name: *const ::std::os::raw::c_char,
    ) -> u64;
}
extern "C" {
    pub fn LLVMGetFunctionAddress(
        EE: LLVMExecutionEngineRef,
        Name: *const ::std::os::raw::c_char,
    ) -> u64;
}
pub type LLVMMemoryManagerAllocateCodeSectionCallback = ::std::option::Option<
    unsafe extern "C" fn(
        Opaque: *mut ::std::os::raw::c_void,
        Size: usize,
        Alignment: ::std::os::raw::c_uint,
        SectionID: ::std::os::raw::c_uint,
        SectionName: *const ::std::os::raw::c_char,
    ) -> *mut u8,
>;
pub type LLVMMemoryManagerAllocateDataSectionCallback = ::std::option::Option<
    unsafe extern "C" fn(
        Opaque: *mut ::std::os::raw::c_void,
        Size: usize,
        Alignment: ::std::os::raw::c_uint,
        SectionID: ::std::os::raw::c_uint,
        SectionName: *const ::std::os::raw::c_char,
        IsReadOnly: LLVMBool,
    ) -> *mut u8,
>;
pub type LLVMMemoryManagerFinalizeMemoryCallback = ::std::option::Option<
    unsafe extern "C" fn(
        Opaque: *mut ::std::os::raw::c_void,
        ErrMsg: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool,
>;
pub type LLVMMemoryManagerDestroyCallback =
    ::std::option::Option<unsafe extern "C" fn(Opaque: *mut ::std::os::raw::c_void)>;
extern "C" {
    pub fn LLVMCreateSimpleMCJITMemoryManager(
        Opaque: *mut ::std::os::raw::c_void,
        AllocateCodeSection: LLVMMemoryManagerAllocateCodeSectionCallback,
        AllocateDataSection: LLVMMemoryManagerAllocateDataSectionCallback,
        FinalizeMemory: LLVMMemoryManagerFinalizeMemoryCallback,
        Destroy: LLVMMemoryManagerDestroyCallback,
    ) -> LLVMMCJITMemoryManagerRef;
}
extern "C" {
    pub fn LLVMDisposeMCJITMemoryManager(MM: LLVMMCJITMemoryManagerRef);
}
extern "C" {
    pub fn LLVMCreateGDBRegistrationListener() -> LLVMJITEventListenerRef;
}
extern "C" {
    pub fn LLVMCreateIntelJITEventListener() -> LLVMJITEventListenerRef;
}
extern "C" {
    pub fn LLVMCreateOProfileJITEventListener() -> LLVMJITEventListenerRef;
}
extern "C" {
    pub fn LLVMCreatePerfJITEventListener() -> LLVMJITEventListenerRef;
}
extern "C" {
    pub fn LLVMParseIRInContext(
        ContextRef: LLVMContextRef,
        MemBuf: LLVMMemoryBufferRef,
        OutM: *mut LLVMModuleRef,
        OutMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMInitializeTransformUtils(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeScalarOpts(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeObjCARCOpts(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeVectorization(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeInstCombine(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeAggressiveInstCombiner(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeIPO(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeInstrumentation(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeAnalysis(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeIPA(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeCodeGen(R: LLVMPassRegistryRef);
}
extern "C" {
    pub fn LLVMInitializeTarget(R: LLVMPassRegistryRef);
}
pub const LLVMLinkerMode_LLVMLinkerDestroySource: LLVMLinkerMode = 0;
pub const LLVMLinkerMode_LLVMLinkerPreserveSource_Removed: LLVMLinkerMode = 1;
pub type LLVMLinkerMode = u32;
extern "C" {
    pub fn LLVMLinkModules2(Dest: LLVMModuleRef, Src: LLVMModuleRef) -> LLVMBool;
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueSectionIterator {
    _unused: [u8; 0],
}
pub type LLVMSectionIteratorRef = *mut LLVMOpaqueSectionIterator;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueSymbolIterator {
    _unused: [u8; 0],
}
pub type LLVMSymbolIteratorRef = *mut LLVMOpaqueSymbolIterator;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueRelocationIterator {
    _unused: [u8; 0],
}
pub type LLVMRelocationIteratorRef = *mut LLVMOpaqueRelocationIterator;
pub const LLVMBinaryType_LLVMBinaryTypeArchive: LLVMBinaryType = 0;
pub const LLVMBinaryType_LLVMBinaryTypeMachOUniversalBinary: LLVMBinaryType = 1;
pub const LLVMBinaryType_LLVMBinaryTypeCOFFImportFile: LLVMBinaryType = 2;
pub const LLVMBinaryType_LLVMBinaryTypeIR: LLVMBinaryType = 3;
pub const LLVMBinaryType_LLVMBinaryTypeWinRes: LLVMBinaryType = 4;
pub const LLVMBinaryType_LLVMBinaryTypeCOFF: LLVMBinaryType = 5;
pub const LLVMBinaryType_LLVMBinaryTypeELF32L: LLVMBinaryType = 6;
pub const LLVMBinaryType_LLVMBinaryTypeELF32B: LLVMBinaryType = 7;
pub const LLVMBinaryType_LLVMBinaryTypeELF64L: LLVMBinaryType = 8;
pub const LLVMBinaryType_LLVMBinaryTypeELF64B: LLVMBinaryType = 9;
pub const LLVMBinaryType_LLVMBinaryTypeMachO32L: LLVMBinaryType = 10;
pub const LLVMBinaryType_LLVMBinaryTypeMachO32B: LLVMBinaryType = 11;
pub const LLVMBinaryType_LLVMBinaryTypeMachO64L: LLVMBinaryType = 12;
pub const LLVMBinaryType_LLVMBinaryTypeMachO64B: LLVMBinaryType = 13;
pub const LLVMBinaryType_LLVMBinaryTypeWasm: LLVMBinaryType = 14;
pub type LLVMBinaryType = u32;
extern "C" {
    pub fn LLVMCreateBinary(
        MemBuf: LLVMMemoryBufferRef,
        Context: LLVMContextRef,
        ErrorMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBinaryRef;
}
extern "C" {
    pub fn LLVMDisposeBinary(BR: LLVMBinaryRef);
}
extern "C" {
    pub fn LLVMBinaryCopyMemoryBuffer(BR: LLVMBinaryRef) -> LLVMMemoryBufferRef;
}
extern "C" {
    pub fn LLVMBinaryGetType(BR: LLVMBinaryRef) -> LLVMBinaryType;
}
extern "C" {
    pub fn LLVMMachOUniversalBinaryCopyObjectForArch(
        BR: LLVMBinaryRef,
        Arch: *const ::std::os::raw::c_char,
        ArchLen: size_t,
        ErrorMessage: *mut *mut ::std::os::raw::c_char,
    ) -> LLVMBinaryRef;
}
extern "C" {
    pub fn LLVMObjectFileCopySectionIterator(BR: LLVMBinaryRef) -> LLVMSectionIteratorRef;
}
extern "C" {
    pub fn LLVMObjectFileIsSectionIteratorAtEnd(
        BR: LLVMBinaryRef,
        SI: LLVMSectionIteratorRef,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMObjectFileCopySymbolIterator(BR: LLVMBinaryRef) -> LLVMSymbolIteratorRef;
}
extern "C" {
    pub fn LLVMObjectFileIsSymbolIteratorAtEnd(
        BR: LLVMBinaryRef,
        SI: LLVMSymbolIteratorRef,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMDisposeSectionIterator(SI: LLVMSectionIteratorRef);
}
extern "C" {
    pub fn LLVMMoveToNextSection(SI: LLVMSectionIteratorRef);
}
extern "C" {
    pub fn LLVMMoveToContainingSection(Sect: LLVMSectionIteratorRef, Sym: LLVMSymbolIteratorRef);
}
extern "C" {
    pub fn LLVMDisposeSymbolIterator(SI: LLVMSymbolIteratorRef);
}
extern "C" {
    pub fn LLVMMoveToNextSymbol(SI: LLVMSymbolIteratorRef);
}
extern "C" {
    pub fn LLVMGetSectionName(SI: LLVMSectionIteratorRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetSectionSize(SI: LLVMSectionIteratorRef) -> u64;
}
extern "C" {
    pub fn LLVMGetSectionContents(SI: LLVMSectionIteratorRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetSectionAddress(SI: LLVMSectionIteratorRef) -> u64;
}
extern "C" {
    pub fn LLVMGetSectionContainsSymbol(
        SI: LLVMSectionIteratorRef,
        Sym: LLVMSymbolIteratorRef,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetRelocations(Section: LLVMSectionIteratorRef) -> LLVMRelocationIteratorRef;
}
extern "C" {
    pub fn LLVMDisposeRelocationIterator(RI: LLVMRelocationIteratorRef);
}
extern "C" {
    pub fn LLVMIsRelocationIteratorAtEnd(
        Section: LLVMSectionIteratorRef,
        RI: LLVMRelocationIteratorRef,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMMoveToNextRelocation(RI: LLVMRelocationIteratorRef);
}
extern "C" {
    pub fn LLVMGetSymbolName(SI: LLVMSymbolIteratorRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetSymbolAddress(SI: LLVMSymbolIteratorRef) -> u64;
}
extern "C" {
    pub fn LLVMGetSymbolSize(SI: LLVMSymbolIteratorRef) -> u64;
}
extern "C" {
    pub fn LLVMGetRelocationOffset(RI: LLVMRelocationIteratorRef) -> u64;
}
extern "C" {
    pub fn LLVMGetRelocationSymbol(RI: LLVMRelocationIteratorRef) -> LLVMSymbolIteratorRef;
}
extern "C" {
    pub fn LLVMGetRelocationType(RI: LLVMRelocationIteratorRef) -> u64;
}
extern "C" {
    pub fn LLVMGetRelocationTypeName(
        RI: LLVMRelocationIteratorRef,
    ) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMGetRelocationValueString(
        RI: LLVMRelocationIteratorRef,
    ) -> *const ::std::os::raw::c_char;
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueObjectFile {
    _unused: [u8; 0],
}
pub type LLVMObjectFileRef = *mut LLVMOpaqueObjectFile;
extern "C" {
    pub fn LLVMCreateObjectFile(MemBuf: LLVMMemoryBufferRef) -> LLVMObjectFileRef;
}
extern "C" {
    pub fn LLVMDisposeObjectFile(ObjectFile: LLVMObjectFileRef);
}
extern "C" {
    pub fn LLVMGetSections(ObjectFile: LLVMObjectFileRef) -> LLVMSectionIteratorRef;
}
extern "C" {
    pub fn LLVMIsSectionIteratorAtEnd(
        ObjectFile: LLVMObjectFileRef,
        SI: LLVMSectionIteratorRef,
    ) -> LLVMBool;
}
extern "C" {
    pub fn LLVMGetSymbols(ObjectFile: LLVMObjectFileRef) -> LLVMSymbolIteratorRef;
}
extern "C" {
    pub fn LLVMIsSymbolIteratorAtEnd(
        ObjectFile: LLVMObjectFileRef,
        SI: LLVMSymbolIteratorRef,
    ) -> LLVMBool;
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOrcOpaqueJITStack {
    _unused: [u8; 0],
}
pub type LLVMOrcJITStackRef = *mut LLVMOrcOpaqueJITStack;
pub type LLVMOrcModuleHandle = u64;
pub type LLVMOrcTargetAddress = u64;
pub type LLVMOrcSymbolResolverFn = ::std::option::Option<
    unsafe extern "C" fn(
        Name: *const ::std::os::raw::c_char,
        LookupCtx: *mut ::std::os::raw::c_void,
    ) -> u64,
>;
pub type LLVMOrcLazyCompileCallbackFn = ::std::option::Option<
    unsafe extern "C" fn(
        JITStack: LLVMOrcJITStackRef,
        CallbackCtx: *mut ::std::os::raw::c_void,
    ) -> u64,
>;
extern "C" {
    pub fn LLVMOrcCreateInstance(TM: LLVMTargetMachineRef) -> LLVMOrcJITStackRef;
}
extern "C" {
    pub fn LLVMOrcGetErrorMsg(JITStack: LLVMOrcJITStackRef) -> *const ::std::os::raw::c_char;
}
extern "C" {
    pub fn LLVMOrcGetMangledSymbol(
        JITStack: LLVMOrcJITStackRef,
        MangledSymbol: *mut *mut ::std::os::raw::c_char,
        Symbol: *const ::std::os::raw::c_char,
    );
}
extern "C" {
    pub fn LLVMOrcDisposeMangledSymbol(MangledSymbol: *mut ::std::os::raw::c_char);
}
extern "C" {
    pub fn LLVMOrcCreateLazyCompileCallback(
        JITStack: LLVMOrcJITStackRef,
        RetAddr: *mut LLVMOrcTargetAddress,
        Callback: LLVMOrcLazyCompileCallbackFn,
        CallbackCtx: *mut ::std::os::raw::c_void,
    ) -> LLVMErrorRef;
}
extern "C" {
    pub fn LLVMOrcCreateIndirectStub(
        JITStack: LLVMOrcJITStackRef,
        StubName: *const ::std::os::raw::c_char,
        InitAddr: LLVMOrcTargetAddress,
    ) -> LLVMErrorRef;
}
extern "C" {
    pub fn LLVMOrcSetIndirectStubPointer(
        JITStack: LLVMOrcJITStackRef,
        StubName: *const ::std::os::raw::c_char,
        NewAddr: LLVMOrcTargetAddress,
    ) -> LLVMErrorRef;
}
extern "C" {
    pub fn LLVMOrcAddEagerlyCompiledIR(
        JITStack: LLVMOrcJITStackRef,
        RetHandle: *mut LLVMOrcModuleHandle,
        Mod: LLVMModuleRef,
        SymbolResolver: LLVMOrcSymbolResolverFn,
        SymbolResolverCtx: *mut ::std::os::raw::c_void,
    ) -> LLVMErrorRef;
}
extern "C" {
    pub fn LLVMOrcAddLazilyCompiledIR(
        JITStack: LLVMOrcJITStackRef,
        RetHandle: *mut LLVMOrcModuleHandle,
        Mod: LLVMModuleRef,
        SymbolResolver: LLVMOrcSymbolResolverFn,
        SymbolResolverCtx: *mut ::std::os::raw::c_void,
    ) -> LLVMErrorRef;
}
extern "C" {
    pub fn LLVMOrcAddObjectFile(
        JITStack: LLVMOrcJITStackRef,
        RetHandle: *mut LLVMOrcModuleHandle,
        Obj: LLVMMemoryBufferRef,
        SymbolResolver: LLVMOrcSymbolResolverFn,
        SymbolResolverCtx: *mut ::std::os::raw::c_void,
    ) -> LLVMErrorRef;
}
extern "C" {
    pub fn LLVMOrcRemoveModule(
        JITStack: LLVMOrcJITStackRef,
        H: LLVMOrcModuleHandle,
    ) -> LLVMErrorRef;
}
extern "C" {
    pub fn LLVMOrcGetSymbolAddress(
        JITStack: LLVMOrcJITStackRef,
        RetAddr: *mut LLVMOrcTargetAddress,
        SymbolName: *const ::std::os::raw::c_char,
    ) -> LLVMErrorRef;
}
extern "C" {
    pub fn LLVMOrcGetSymbolAddressIn(
        JITStack: LLVMOrcJITStackRef,
        RetAddr: *mut LLVMOrcTargetAddress,
        H: LLVMOrcModuleHandle,
        SymbolName: *const ::std::os::raw::c_char,
    ) -> LLVMErrorRef;
}
extern "C" {
    pub fn LLVMOrcDisposeInstance(JITStack: LLVMOrcJITStackRef) -> LLVMErrorRef;
}
extern "C" {
    pub fn LLVMOrcRegisterJITEventListener(
        JITStack: LLVMOrcJITStackRef,
        L: LLVMJITEventListenerRef,
    );
}
extern "C" {
    pub fn LLVMOrcUnregisterJITEventListener(
        JITStack: LLVMOrcJITStackRef,
        L: LLVMJITEventListenerRef,
    );
}
extern "C" {
    pub fn LLVMLoadLibraryPermanently(Filename: *const ::std::os::raw::c_char) -> LLVMBool;
}
extern "C" {
    pub fn LLVMParseCommandLineOptions(
        argc: ::std::os::raw::c_int,
        argv: *const *const ::std::os::raw::c_char,
        Overview: *const ::std::os::raw::c_char,
    );
}
extern "C" {
    pub fn LLVMSearchForAddressOfSymbol(
        symbolName: *const ::std::os::raw::c_char,
    ) -> *mut ::std::os::raw::c_void;
}
extern "C" {
    pub fn LLVMAddSymbol(
        symbolName: *const ::std::os::raw::c_char,
        symbolValue: *mut ::std::os::raw::c_void,
    );
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueLTOModule {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueLTOCodeGenerator {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueThinLTOCodeGenerator {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaqueLTOInput {
    _unused: [u8; 0],
}
extern "C" {
    pub fn LLVMAddAggressiveInstCombinerPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddCoroEarlyPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddCoroSplitPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddCoroElidePass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddCoroCleanupPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddArgumentPromotionPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddConstantMergePass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddMergeFunctionsPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddCalledValuePropagationPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddDeadArgEliminationPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddFunctionAttrsPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddFunctionInliningPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddAlwaysInlinerPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddGlobalDCEPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddGlobalOptimizerPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddIPConstantPropagationPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddPruneEHPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddIPSCCPPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddInternalizePass(arg1: LLVMPassManagerRef, AllButMain: ::std::os::raw::c_uint);
}
extern "C" {
    pub fn LLVMAddInternalizePassWithMustPreservePredicate(
        PM: LLVMPassManagerRef,
        Context: *mut ::std::os::raw::c_void,
        MustPreserve: ::std::option::Option<
            unsafe extern "C" fn(arg1: LLVMValueRef, arg2: *mut ::std::os::raw::c_void) -> LLVMBool,
        >,
    );
}
extern "C" {
    pub fn LLVMAddStripDeadPrototypesPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddStripSymbolsPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddInstructionCombiningPass(PM: LLVMPassManagerRef);
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LLVMOpaquePassManagerBuilder {
    _unused: [u8; 0],
}
pub type LLVMPassManagerBuilderRef = *mut LLVMOpaquePassManagerBuilder;
extern "C" {
    pub fn LLVMPassManagerBuilderCreate() -> LLVMPassManagerBuilderRef;
}
extern "C" {
    pub fn LLVMPassManagerBuilderDispose(PMB: LLVMPassManagerBuilderRef);
}
extern "C" {
    pub fn LLVMPassManagerBuilderSetOptLevel(
        PMB: LLVMPassManagerBuilderRef,
        OptLevel: ::std::os::raw::c_uint,
    );
}
extern "C" {
    pub fn LLVMPassManagerBuilderSetSizeLevel(
        PMB: LLVMPassManagerBuilderRef,
        SizeLevel: ::std::os::raw::c_uint,
    );
}
extern "C" {
    pub fn LLVMPassManagerBuilderSetDisableUnitAtATime(
        PMB: LLVMPassManagerBuilderRef,
        Value: LLVMBool,
    );
}
extern "C" {
    pub fn LLVMPassManagerBuilderSetDisableUnrollLoops(
        PMB: LLVMPassManagerBuilderRef,
        Value: LLVMBool,
    );
}
extern "C" {
    pub fn LLVMPassManagerBuilderSetDisableSimplifyLibCalls(
        PMB: LLVMPassManagerBuilderRef,
        Value: LLVMBool,
    );
}
extern "C" {
    pub fn LLVMPassManagerBuilderUseInlinerWithThreshold(
        PMB: LLVMPassManagerBuilderRef,
        Threshold: ::std::os::raw::c_uint,
    );
}
extern "C" {
    pub fn LLVMPassManagerBuilderPopulateFunctionPassManager(
        PMB: LLVMPassManagerBuilderRef,
        PM: LLVMPassManagerRef,
    );
}
extern "C" {
    pub fn LLVMPassManagerBuilderPopulateModulePassManager(
        PMB: LLVMPassManagerBuilderRef,
        PM: LLVMPassManagerRef,
    );
}
extern "C" {
    pub fn LLVMPassManagerBuilderPopulateLTOPassManager(
        PMB: LLVMPassManagerBuilderRef,
        PM: LLVMPassManagerRef,
        Internalize: LLVMBool,
        RunInliner: LLVMBool,
    );
}
extern "C" {
    pub fn LLVMAddAggressiveDCEPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddDCEPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddBitTrackingDCEPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddAlignmentFromAssumptionsPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddCFGSimplificationPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddDeadStoreEliminationPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddScalarizerPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddMergedLoadStoreMotionPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddGVNPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddNewGVNPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddIndVarSimplifyPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddJumpThreadingPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLICMPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLoopDeletionPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLoopIdiomPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLoopRotatePass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLoopRerollPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLoopUnrollPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLoopUnrollAndJamPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLoopUnswitchPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLowerAtomicPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddMemCpyOptPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddPartiallyInlineLibCallsPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddReassociatePass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddSCCPPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddScalarReplAggregatesPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddScalarReplAggregatesPassSSA(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddScalarReplAggregatesPassWithThreshold(
        PM: LLVMPassManagerRef,
        Threshold: ::std::os::raw::c_int,
    );
}
extern "C" {
    pub fn LLVMAddSimplifyLibCallsPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddTailCallEliminationPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddConstantPropagationPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddDemoteMemoryToRegisterPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddVerifierPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddCorrelatedValuePropagationPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddEarlyCSEPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddEarlyCSEMemSSAPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLowerExpectIntrinsicPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLowerConstantIntrinsicsPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddTypeBasedAliasAnalysisPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddScopedNoAliasAAPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddBasicAliasAnalysisPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddUnifyFunctionExitNodesPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLowerSwitchPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddPromoteMemoryToRegisterPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddAddDiscriminatorsPass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddLoopVectorizePass(PM: LLVMPassManagerRef);
}
extern "C" {
    pub fn LLVMAddSLPVectorizePass(PM: LLVMPassManagerRef);
}

/// Globals buffer header layout for cancel/longjmp/trap infrastructure.
/// Shared between JIT, LLVM, VM, and FFI backends.
///
///   offset 0:  cancel_counter (i32) — decremented at backward jumps
///   offset 4:  call_depth (i32) — tracks current recursion depth for
///              stack-overflow detection in generated code
///   offset 8:  cancel_callback fn ptr — called when counter hits 0
///   offset 16: cancel_userdata ptr — passed to callback
///   offset 24: trap_reason (u32) — set by trap helpers before longjmp,
///              read by the host after setjmp returns non-zero
///   offset 32: jmp_buf — longjmp target for cancellation / traps
///
/// User globals start at `CANCEL_FLAG_RESERVED`.
/// The jmp_buf starts at offset 32 for 16-byte alignment (required on Linux x86_64
/// where setjmp may save SSE registers with aligned stores).

// Opaque jmp_buf: 512 bytes (64 * u64), 16-byte aligned.
// Must be at least as large as the platform's jmp_buf.
struct JmpBuf([u64; 64]);

pub const CANCEL_COUNTER_OFFSET: i32 = 0;
pub const CALL_DEPTH_OFFSET: i32 = 4;
pub const CANCEL_CALLBACK_OFFSET: usize = 8;
pub const CANCEL_USERDATA_OFFSET: usize = 16;
pub const TRAP_REASON_OFFSET: i32 = 24;
pub const JMPBUF_OFFSET: usize = 32;
pub const CANCEL_FLAG_RESERVED: i32 = (JMPBUF_OFFSET + std::mem::size_of::<JmpBuf>()) as i32;

/// How many backward jumps between cancel callback invocations.
pub const CANCEL_CHECK_INTERVAL: i32 = 1024;

/// Maximum recursion depth for JIT/LLVM backends before a
/// "call stack overflow" trap fires. The counter is decremented in
/// each function prologue and incremented in each epilogue so it
/// reflects the CURRENT depth (not total call count).
pub const MAX_CALL_DEPTH: i32 = 4096;

/// Trap reason codes written to the globals TRAP_REASON_OFFSET slot
/// before the helper longjmps back to the host entry point.
/// Must stay in sync with the VM and stack backends.
pub const TRAP_NONE: u32 = 0;
pub const TRAP_CANCELLED: u32 = 1;
pub const TRAP_CALL_STACK_OVERFLOW: u32 = 2;
pub const TRAP_ASSERTION_FAILED: u32 = 3;

/// Cancel callback type: returns true to cancel execution.
pub type CancelCallback = unsafe extern "C" fn(user_data: *mut u8) -> bool;

/// Write a cancel callback and user_data into a globals buffer.
/// Also initializes the cancel counter, call depth, and trap reason.
/// Call this before executing JIT code.
pub unsafe fn set_cancel_callback(
    globals: *mut u8,
    callback: Option<CancelCallback>,
    user_data: *mut u8,
) {
    // Set counter
    *(globals.add(CANCEL_COUNTER_OFFSET as usize) as *mut i32) = CANCEL_CHECK_INTERVAL;
    // Call depth starts at MAX_CALL_DEPTH and decrements in each prologue.
    // The first function call brings it to MAX_CALL_DEPTH - 1, and the
    // check is "if new_value < 0 → trap", so we get exactly MAX_CALL_DEPTH
    // nested calls before the trap fires.
    *(globals.add(CALL_DEPTH_OFFSET as usize) as *mut i32) = MAX_CALL_DEPTH;
    // No trap yet.
    *(globals.add(TRAP_REASON_OFFSET as usize) as *mut u32) = TRAP_NONE;
    // Set callback (null if None)
    let cb_ptr: usize = match callback {
        Some(f) => f as usize,
        None => 0,
    };
    *(globals.add(CANCEL_CALLBACK_OFFSET) as *mut usize) = cb_ptr;
    *(globals.add(CANCEL_USERDATA_OFFSET) as *mut usize) = user_data as usize;
}

/// Read the trap reason set by JIT/LLVM trap helpers.
/// Returns TRAP_NONE if no trap fired.
pub unsafe fn read_trap_reason(globals: *const u8) -> u32 {
    *(globals.add(TRAP_REASON_OFFSET as usize) as *const u32)
}

/// Decode a trap reason code into a static string, matching the labels
/// the VM/stack backends use so the CLI can report identically.
pub fn trap_reason_message(reason: u32) -> Option<&'static str> {
    match reason {
        TRAP_NONE => None,
        TRAP_CANCELLED => Some("cancelled"),
        TRAP_CALL_STACK_OVERFLOW => Some("call stack overflow"),
        TRAP_ASSERTION_FAILED => Some("assertion failed"),
        _ => Some("unknown trap"),
    }
}

/// Panics if our JmpBuf is smaller than the platform's actual jmp_buf.
/// The size is measured at build time by build.rs compiling a C snippet.
pub fn assert_jmpbuf_size() {
    if let Some(size) = option_env!("PLATFORM_JMPBUF_SIZE") {
        let platform_size: usize = size.parse().unwrap();
        assert!(
            std::mem::size_of::<JmpBuf>() >= platform_size,
            "JmpBuf ({} bytes) is too small for this platform's jmp_buf ({} bytes)",
            std::mem::size_of::<JmpBuf>(),
            platform_size,
        );
    }
}

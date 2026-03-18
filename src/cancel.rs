/// Globals buffer header layout for cancel/longjmp infrastructure.
/// Shared between JIT, LLVM, VM, and FFI backends.
///
///   offset 0:  cancel_counter (i32) — decremented at backward jumps
///   offset 8:  cancel_callback fn ptr — called when counter hits 0
///   offset 16: cancel_userdata ptr — passed to callback
///   offset 32: jmp_buf — longjmp target for cancellation
///
/// User globals start at `CANCEL_FLAG_RESERVED`.
/// The jmp_buf starts at offset 32 for 16-byte alignment (required on Linux x86_64
/// where setjmp may save SSE registers with aligned stores).

// Opaque jmp_buf: 512 bytes (64 * u64), 16-byte aligned.
// Must be at least as large as the platform's jmp_buf.
struct JmpBuf([u64; 64]);

pub const CANCEL_COUNTER_OFFSET: i32 = 0;
pub const CANCEL_CALLBACK_OFFSET: usize = 8;
pub const CANCEL_USERDATA_OFFSET: usize = 16;
pub const JMPBUF_OFFSET: usize = 32;
pub const CANCEL_FLAG_RESERVED: i32 = (JMPBUF_OFFSET + std::mem::size_of::<JmpBuf>()) as i32;

/// How many backward jumps between cancel callback invocations.
pub const CANCEL_CHECK_INTERVAL: i32 = 1024;

/// Cancel callback type: returns true to cancel execution.
pub type CancelCallback = unsafe extern "C" fn(user_data: *mut u8) -> bool;

/// Write a cancel callback and user_data into a globals buffer.
/// Also initializes the cancel counter. Call this before executing JIT code.
pub unsafe fn set_cancel_callback(
    globals: *mut u8,
    callback: Option<CancelCallback>,
    user_data: *mut u8,
) {
    // Set counter
    *(globals.add(CANCEL_COUNTER_OFFSET as usize) as *mut i32) = CANCEL_CHECK_INTERVAL;
    // Set callback (null if None)
    let cb_ptr: usize = match callback {
        Some(f) => f as usize,
        None => 0,
    };
    *(globals.add(CANCEL_CALLBACK_OFFSET) as *mut usize) = cb_ptr;
    *(globals.add(CANCEL_USERDATA_OFFSET) as *mut usize) = user_data as usize;
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

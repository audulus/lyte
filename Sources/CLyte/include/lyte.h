#ifndef LYTE_H
#define LYTE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// ============ Opaque handles ============

typedef struct LyteCompiler LyteCompiler;
typedef struct LyteProgram LyteProgram;

/// Cancel callback type. Return true to cancel execution.
typedef bool (*lyte_cancel_fn)(void* user_data);

/// Print callback type. Called with the text to display.
/// text is NOT null-terminated; use length to determine the extent.
typedef void (*lyte_print_fn)(const char* text, size_t length, void* user_data);


// ============ Compiler ============

/// Create a new compiler instance with the given entry point names.
/// Pass NULL/0 to default to a single "main" entry point.
LyteCompiler* lyte_compiler_new(const char** entry_points, size_t count);

/// Free a compiler instance.
void lyte_compiler_free(LyteCompiler* compiler);

/// Returns true if an internal compiler error (panic) occurred.
bool lyte_compiler_had_ice(const LyteCompiler* compiler);

/// Get the last error message, or NULL if no error.
/// Error messages include file, line, column, and description
/// (e.g. "node.lyte:4:5: expected Rparen, got Lbrace").
/// Multiple errors are separated by newlines.
const char* lyte_compiler_get_error(const LyteCompiler* compiler);

/// Add source code to the compiler. May be called multiple times
/// to add multiple files. Returns true on success.
bool lyte_compiler_add_source(LyteCompiler* compiler, const char* source, const char* filename);

/// Add trusted prelude source code to the compiler.
/// The prelude is allowed to use `assume` statements (like the stdlib).
/// Line numbers in the prelude are independent of the main source.
/// Call before lyte_compiler_add_source. Returns true on success.
bool lyte_compiler_add_prelude(LyteCompiler* compiler, const char* source);

/// Parse, type-check, specialize, and compile all added source into
/// a LyteProgram (auto-selects JIT or VM backend).
/// Returns NULL on error. Caller must free with lyte_program_free.
LyteProgram* lyte_compiler_compile(LyteCompiler* compiler);

// ============ Program ============

/// Free a compiled program.
void lyte_program_free(LyteProgram* program);

/// Get the size in bytes of the globals buffer.
size_t lyte_program_get_globals_size(const LyteProgram* program);

/// Get the number of global variables.
size_t lyte_program_get_globals_count(const LyteProgram* program);

/// Get the name of a global variable by index.
const char* lyte_program_get_global_name(const LyteProgram* program, size_t index);

/// Get the byte offset of a global variable within the globals buffer.
size_t lyte_program_get_global_offset(const LyteProgram* program, size_t index);

/// Get the size in bytes of a global variable.
size_t lyte_program_get_global_size(const LyteProgram* program, size_t index);

/// Get the type of a global variable as a string.
const char* lyte_program_get_global_type(const LyteProgram* program, size_t index);

/// Set a cancel callback. Called approximately every 1024 backward jumps.
/// If it returns true, execution is cancelled. Pass NULL to disable.
void lyte_program_set_cancel_callback(LyteProgram* program, lyte_cancel_fn callback, void* user_data);

/// Set a print callback. When set, all print/println/putc output from
/// lyte scripts is routed through this callback instead of stdout.
/// Pass NULL to restore default stdout behavior.
void lyte_program_set_print_callback(LyteProgram* program, lyte_print_fn callback, void* user_data);

// ============ Entry point invocation ============

/// Call an entry point by index with an external globals buffer.
/// The index corresponds to the order of entry points passed to lyte_compiler_new.
/// The buffer must be at least lyte_program_get_globals_size() bytes.
/// Returns true on success, false if cancelled, error, or invalid index.
bool lyte_entry_point_call(LyteProgram* program, size_t entry_point, uint8_t* globals);

// ============ Globals helpers ============

/// Bind an external buffer to a global slice variable.
/// Writes the data pointer and length into the globals buffer at the given offset.
/// The offset should come from lyte_program_get_global_offset for a slice-typed global.
/// The caller must ensure the buffer remains valid for the lifetime of the program execution.
void lyte_globals_bind_slice(uint8_t* globals, size_t offset, const void* data, int32_t len);

/// Allocate a zeroed globals buffer of the correct size.
/// Caller must free with lyte_globals_free().
uint8_t* lyte_globals_alloc(const LyteProgram* program);

/// Free a globals buffer allocated by lyte_globals_alloc.
void lyte_globals_free(uint8_t* globals, size_t size);

#ifdef __cplusplus
}
#endif

#endif // LYTE_H

import CLyte
import Foundation

/// A Swift wrapper around the lyte compiler.
public final class LyteCompiler {
    private let handle: OpaquePointer

    public init() {
        handle = lyte_compiler_new()
    }

    deinit {
        lyte_compiler_free(handle)
    }

    /// The last error message, or nil.
    public var lastError: String? {
        guard let cStr = lyte_compiler_get_error(handle) else { return nil }
        return String(cString: cStr)
    }

    /// Parse source code.
    @discardableResult
    public func parse(source: String, filename: String = "<string>") throws -> Bool {
        let ok = source.withCString { src in
            filename.withCString { file in
                lyte_compiler_parse(handle, src, file)
            }
        }
        if !ok { throw LyteError(message: lastError ?? "parse error") }
        return true
    }

    /// Type-check the parsed source.
    @discardableResult
    public func check() throws -> Bool {
        if !lyte_compiler_check(handle) {
            throw LyteError(message: lastError ?? "type check error")
        }
        return true
    }

    /// Monomorphize generics.
    @discardableResult
    public func specialize() throws -> Bool {
        if !lyte_compiler_specialize(handle) {
            throw LyteError(message: lastError ?? "specialization error")
        }
        return true
    }

    /// JIT compile. Returns the entry point and globals size.
    public func jit() throws -> CompiledProgram {
        if !lyte_compiler_jit(handle) {
            throw LyteError(message: lastError ?? "JIT error")
        }
        guard let ptr = lyte_compiler_get_code_ptr(handle) else {
            throw LyteError(message: "no code pointer after JIT")
        }
        let globalsSize = lyte_compiler_get_globals_size(handle)
        return CompiledProgram(codePtr: ptr, globalsSize: globalsSize)
    }

    /// Convenience: parse, check, specialize, and JIT in one call.
    public func compile(source: String, filename: String = "<string>") throws -> CompiledProgram {
        try parse(source: source, filename: filename)
        try check()
        try specialize()
        return try jit()
    }
}

/// A JIT-compiled lyte program ready to execute.
public struct CompiledProgram {
    public let codePtr: UnsafePointer<UInt8>
    public let globalsSize: Int

    /// Run the compiled program.
    public func run() {
        typealias EntryFn = @convention(c) (UnsafeMutablePointer<UInt8>?) -> Void
        let entry = unsafeBitCast(codePtr, to: EntryFn.self)
        var globals = [UInt8](repeating: 0, count: max(globalsSize, 1))
        globals.withUnsafeMutableBufferPointer { buf in
            entry(buf.baseAddress)
        }
    }
}

/// An error from the lyte compiler.
public struct LyteError: Error, CustomStringConvertible {
    public let message: String
    public var description: String { message }
}

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

        let count = lyte_compiler_get_globals_count(handle)
        var globals: [GlobalVariable] = []
        for i in 0..<count {
            guard let nameCStr = lyte_compiler_get_global_name(handle, i),
                  let typeCStr = lyte_compiler_get_global_type(handle, i) else { continue }
            globals.append(GlobalVariable(
                name: String(cString: nameCStr),
                offset: lyte_compiler_get_global_offset(handle, i),
                size: lyte_compiler_get_global_size(handle, i),
                type: String(cString: typeCStr)
            ))
        }

        return CompiledProgram(codePtr: ptr, globalsSize: globalsSize, globals: globals)
    }

    /// Convenience: parse, check, specialize, and JIT in one call.
    public func compile(source: String, filename: String = "<string>") throws -> CompiledProgram {
        try parse(source: source, filename: filename)
        try check()
        try specialize()
        return try jit()
    }
}

/// Information about a global variable in a compiled program.
public struct GlobalVariable {
    public let name: String
    public let offset: Int
    public let size: Int
    public let type: String
}

/// A JIT-compiled lyte program ready to execute.
public final class CompiledProgram {
    public let codePtr: UnsafePointer<UInt8>
    public let globalsSize: Int
    public let globals: [GlobalVariable]
    private var globalsBuffer: [UInt8]

    init(codePtr: UnsafePointer<UInt8>, globalsSize: Int, globals: [GlobalVariable]) {
        self.codePtr = codePtr
        self.globalsSize = globalsSize
        self.globals = globals
        self.globalsBuffer = [UInt8](repeating: 0, count: max(globalsSize, 1))
    }

    /// Find a global variable by name.
    public func global(named name: String) -> GlobalVariable? {
        globals.first { $0.name == name }
    }

    /// Run the compiled program.
    public func run() {
        typealias EntryFn = @convention(c) (UnsafeMutablePointer<UInt8>?) -> Void
        let entry = unsafeBitCast(codePtr, to: EntryFn.self)
        globalsBuffer.withUnsafeMutableBufferPointer { buf in
            entry(buf.baseAddress)
        }
    }

    /// Read a value from the globals buffer at the given byte offset.
    public func readGlobal<T>(at offset: Int, as type: T.Type) -> T {
        globalsBuffer.withUnsafeBufferPointer { buf in
            buf.baseAddress!.advanced(by: offset).withMemoryRebound(to: T.self, capacity: 1) { $0.pointee }
        }
    }

    /// Write a value to the globals buffer at the given byte offset.
    public func writeGlobal<T>(at offset: Int, value: T) {
        globalsBuffer.withUnsafeMutableBufferPointer { buf in
            buf.baseAddress!.advanced(by: offset).withMemoryRebound(to: T.self, capacity: 1) { $0.pointee = value }
        }
    }
}

/// An error from the lyte compiler.
public struct LyteError: Error, CustomStringConvertible {
    public let message: String
    public var description: String { message }
}

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

    /// Add source code to the compiler. May be called multiple times.
    @discardableResult
    public func addSource(_ source: String, filename: String = "<string>") throws -> Bool {
        let ok = source.withCString { src in
            filename.withCString { file in
                lyte_compiler_add_source(handle, src, file)
            }
        }
        if !ok { throw LyteError(message: lastError ?? "parse error") }
        return true
    }

    /// Set entry point function names. Must be called before compile().
    public func setEntryPoints(_ names: [String]) throws {
        let cNames = names.map { strdup($0)! }
        defer { cNames.forEach { free($0) } }
        var ptrs = cNames.map { UnsafePointer($0) as UnsafePointer<CChar>? }
        let ok = ptrs.withUnsafeMutableBufferPointer { buf in
            lyte_compiler_set_entry_points(
                handle,
                buf.baseAddress!.withMemoryRebound(to: UnsafePointer<CChar>?.self, capacity: buf.count) { $0 },
                buf.count
            )
        }
        if !ok { throw LyteError(message: lastError ?? "set entry points error") }
    }

    /// Parse, type-check, specialize, and compile all added source.
    /// Auto-selects JIT or VM backend.
    public func compile() throws -> Program {
        guard let ptr = lyte_compiler_compile(handle) else {
            throw LyteError(message: lastError ?? "compilation error")
        }
        return Program(handle: ptr)
    }

    /// Convenience: add source and compile in one call.
    public func compile(source: String, filename: String = "<string>") throws -> Program {
        try addSource(source, filename: filename)
        return try compile()
    }
}

/// Information about a global variable in a compiled program.
public struct GlobalVariable {
    public let name: String
    public let offset: Int
    public let size: Int
    public let type: String
}

/// A compiled lyte program, ready to execute.
public final class Program {
    let handle: OpaquePointer

    init(handle: OpaquePointer) {
        self.handle = handle
    }

    deinit {
        lyte_program_free(handle)
    }

    /// Size of the globals buffer in bytes.
    public var globalsSize: Int {
        lyte_program_get_globals_size(handle)
    }

    /// Global variable metadata.
    public var globals: [GlobalVariable] {
        let count = lyte_program_get_globals_count(handle)
        var result: [GlobalVariable] = []
        for i in 0..<count {
            guard let nameCStr = lyte_program_get_global_name(handle, i),
                  let typeCStr = lyte_program_get_global_type(handle, i) else { continue }
            result.append(GlobalVariable(
                name: String(cString: nameCStr),
                offset: lyte_program_get_global_offset(handle, i),
                size: lyte_program_get_global_size(handle, i),
                type: String(cString: typeCStr)
            ))
        }
        return result
    }

    /// Find a global variable by name.
    public func global(named name: String) -> GlobalVariable? {
        globals.first { $0.name == name }
    }

    /// Look up an entry point by name.
    public func entryPoint(named name: String) -> EntryPoint? {
        name.withCString { cName in
            guard let ptr = lyte_program_get_entry_point(handle, cName) else { return nil }
            return EntryPoint(handle: ptr)
        }
    }

    /// Set a cancel callback. Called periodically during execution.
    /// If it returns true, execution is cancelled.
    public func setCancelCallback(_ callback: @escaping @convention(c) (UnsafeMutableRawPointer?) -> Bool,
                                   userData: UnsafeMutableRawPointer? = nil) {
        lyte_program_set_cancel_callback(handle, callback, userData)
    }

    /// Allocate a zeroed globals buffer.
    public func allocGlobals() -> Globals {
        let size = globalsSize
        let ptr = lyte_globals_alloc(handle)!
        return Globals(ptr: ptr, size: size)
    }
}

/// An entry point handle for calling compiled functions.
public struct EntryPoint {
    let handle: UnsafePointer<LyteEntryPoint>

    /// Call this entry point with the given globals buffer.
    /// Returns true on success, false if cancelled.
    @discardableResult
    public func call(globals: UnsafeMutablePointer<UInt8>) -> Bool {
        lyte_entry_point_call(handle, globals)
    }

    /// Call this entry point with a Globals object.
    @discardableResult
    public func call(globals: Globals) -> Bool {
        lyte_entry_point_call(handle, globals.ptr)
    }
}

/// A globals buffer for a compiled program.
public final class Globals {
    let ptr: UnsafeMutablePointer<UInt8>
    let size: Int

    init(ptr: UnsafeMutablePointer<UInt8>, size: Int) {
        self.ptr = ptr
        self.size = size
    }

    deinit {
        lyte_globals_free(ptr, size)
    }

    /// Read a value from the globals buffer at the given byte offset.
    public func read<T>(at offset: Int, as type: T.Type) -> T {
        ptr.advanced(by: offset).withMemoryRebound(to: T.self, capacity: 1) { $0.pointee }
    }

    /// Write a value to the globals buffer at the given byte offset.
    public func write<T>(at offset: Int, value: T) {
        ptr.advanced(by: offset).withMemoryRebound(to: T.self, capacity: 1) { $0.pointee = value }
    }
}

/// An error from the lyte compiler.
public struct LyteError: Error, CustomStringConvertible {
    public let message: String
    public var description: String { message }
}

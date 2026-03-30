import CLyte
import Foundation

/// A Swift wrapper around the lyte compiler.
public final class LyteCompiler {
    private let handle: OpaquePointer
    private let entryPointNames: [String]

    /// Create a compiler with the given entry point names.
    /// Pass an empty array to default to ["main"].
    public init(entryPoints: [String] = []) {
        self.entryPointNames = entryPoints.isEmpty ? ["main"] : entryPoints
        if entryPoints.isEmpty {
            handle = lyte_compiler_new(nil, 0)
        } else {
            let cNames = entryPoints.map { strdup($0)! }
            defer { cNames.forEach { free($0) } }
            var ptrs = cNames.map { UnsafePointer($0) as UnsafePointer<CChar>? }
            handle = ptrs.withUnsafeMutableBufferPointer { buf in
                lyte_compiler_new(
                    buf.baseAddress!.withMemoryRebound(to: UnsafePointer<CChar>?.self, capacity: buf.count) { $0 },
                    buf.count
                )
            }
        }
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

    /// Parse, type-check, specialize, and compile all added source.
    /// Auto-selects JIT or VM backend.
    public func compile() throws -> Program {
        guard let ptr = lyte_compiler_compile(handle) else {
            throw LyteError(message: lastError ?? "compilation error")
        }
        return Program(handle: ptr, entryPointNames: entryPointNames)
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
    private let entryPointNames: [String]

    init(handle: OpaquePointer, entryPointNames: [String]) {
        self.handle = handle
        self.entryPointNames = entryPointNames
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

    /// Look up an entry point by name. Returns nil if not found.
    public func entryPoint(named name: String) -> EntryPoint? {
        guard let index = entryPointNames.firstIndex(of: name) else { return nil }
        return EntryPoint(program: self, index: index)
    }

    /// Get an entry point by index (matching the order passed to LyteCompiler.init).
    public func entryPoint(at index: Int) -> EntryPoint? {
        guard index >= 0 && index < entryPointNames.count else { return nil }
        return EntryPoint(program: self, index: index)
    }

    /// Set a cancel callback. Called periodically during execution.
    /// If it returns true, execution is cancelled.
    public func setCancelCallback(_ callback: @escaping @convention(c) (UnsafeMutableRawPointer?) -> Bool,
                                   userData: UnsafeMutableRawPointer? = nil) {
        lyte_program_set_cancel_callback(handle, callback, userData)
    }

    /// Set a print callback. When set, all print/println/putc output from
    /// lyte scripts is routed through this callback instead of stdout.
    /// Pass nil to restore default stdout behavior.
    public func setPrintCallback(_ callback: lyte_print_fn?,
                                  userData: UnsafeMutableRawPointer? = nil) {
        lyte_program_set_print_callback(handle, callback, userData)
    }

    /// Allocate a zeroed globals buffer.
    public func allocGlobals() -> Globals {
        let size = globalsSize
        if size == 0 {
            // No globals — allocate a minimal buffer so callers don't need to handle nil.
            let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 1)
            ptr.initialize(to: 0)
            return Globals(ptr: ptr, size: 0)
        }
        let ptr = lyte_globals_alloc(handle)!
        return Globals(ptr: ptr, size: size)
    }
}

/// An entry point handle for calling compiled functions.
public struct EntryPoint {
    let program: Program
    let index: Int

    /// Call this entry point with the given globals buffer.
    /// Returns true on success, false if cancelled.
    @discardableResult
    public func call(globals: UnsafeMutablePointer<UInt8>) -> Bool {
        lyte_entry_point_call(program.handle, index, globals)
    }

    /// Call this entry point with a Globals object.
    @discardableResult
    public func call(globals: Globals) -> Bool {
        lyte_entry_point_call(program.handle, index, globals.ptr)
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
        if size == 0 {
            ptr.deallocate()
        } else {
            lyte_globals_free(ptr, size)
        }
    }

    /// Read a value from the globals buffer at the given byte offset.
    public func read<T>(at offset: Int, as type: T.Type) -> T {
        ptr.advanced(by: offset).withMemoryRebound(to: T.self, capacity: 1) { $0.pointee }
    }

    /// Write a value to the globals buffer at the given byte offset.
    public func write<T>(at offset: Int, value: T) {
        ptr.advanced(by: offset).withMemoryRebound(to: T.self, capacity: 1) { $0.pointee = value }
    }

    /// Bind an external buffer to a global slice variable.
    /// The buffer must remain valid for the lifetime of program execution.
    public func bindSlice<T>(at offset: Int, to buffer: UnsafeBufferPointer<T>) {
        lyte_globals_bind_slice(ptr, offset, buffer.baseAddress, Int32(buffer.count))
    }

    /// Bind a mutable buffer to a global slice variable.
    /// The buffer must remain valid for the lifetime of program execution.
    public func bindSlice<T>(at offset: Int, to buffer: UnsafeMutableBufferPointer<T>) {
        lyte_globals_bind_slice(ptr, offset, buffer.baseAddress, Int32(buffer.count))
    }
}

/// An error from the lyte compiler.
public struct LyteError: Error, CustomStringConvertible {
    public let message: String
    public var description: String { message }
}

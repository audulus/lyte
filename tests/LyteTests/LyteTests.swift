import Testing
@testable import Lyte

@Test func compileAndRun() throws {
    let compiler = LyteCompiler(entryPoints: ["main"])
    let program = try compiler.compile(source: """
        main {
            assert(1 + 1 == 2)
        }
    """)
    let globals = program.allocGlobals()
    let main = try #require(program.entryPoint(named: "main"))
    main.call(globals: globals)
}

@Test func globalVariableMultiply() throws {
    let compiler = LyteCompiler(entryPoints: ["main"])
    let program = try compiler.compile(source: """
        var x: i32
        main {
            x = x * 2
        }
    """)

    let x = try #require(program.global(named: "x"))
    #expect(x.type == "i32")

    let globals = program.allocGlobals()
    globals.write(at: x.offset, value: Int32(2))

    let main = try #require(program.entryPoint(named: "main"))
    main.call(globals: globals)

    let result: Int32 = globals.read(at: x.offset, as: Int32.self)
    #expect(result == 4)
}

@Test func parseErrorContainsDetails() throws {
    let compiler = LyteCompiler(entryPoints: ["main"])
    do {
        try compiler.addSource("""
            main {
                var x: i32
                x = (
            }
        """, filename: "node.lyte")
        #expect(Bool(false), "should have thrown")
    } catch let error as LyteError {
        #expect(error.message.contains("node.lyte"))
        #expect(error.message.contains("expected"))
    }
}

@Test func typeErrorContainsDetails() throws {
    let compiler = LyteCompiler(entryPoints: ["main"])
    do {
        _ = try compiler.compile(source: """
            add(a: i32, b: i32) -> i32 { a + b }
            main {
                add(1)
            }
        """, filename: "node.lyte")
        #expect(Bool(false), "should have thrown")
    } catch let error as LyteError {
        #expect(error.message.contains("node.lyte"))
        #expect(error.message.contains("function expects 2 arguments but 1 was given"))
    }
}

@Test func safetyErrorContainsDetails() throws {
    let compiler = LyteCompiler(entryPoints: ["main"])
    do {
        _ = try compiler.compile(source: """
            main {
                var a: [i32; 4]
                a[10] = 1
            }
        """, filename: "node.lyte")
        #expect(Bool(false), "should have thrown")
    } catch let error as LyteError {
        #expect(error.message.contains("node.lyte"))
        #expect(error.message.contains("couldn't prove"))
    }
}

@Test func globalSliceBinding() throws {
    let compiler = LyteCompiler(entryPoints: ["process"])
    try compiler.addSource("""
        const MAX = 1024
        var input: [f32]
        var output: [f32]
        var n: i32
        assume n >= 0 && n <= MAX
        assume input.len >= MAX
        assume output.len >= MAX
    """, filename: "<prelude>")
    let program = try compiler.compile(source: """
        process {
            for i in 0 .. n {
                output[i] = input[i] * 2.0
            }
        }
    """)

    let inputGlobal = try #require(program.global(named: "input"))
    let outputGlobal = try #require(program.global(named: "output"))
    let nGlobal = try #require(program.global(named: "n"))

    #expect(inputGlobal.type == "[f32]")
    #expect(outputGlobal.type == "[f32]")

    var inputBuf: [Float] = [1.0, 2.0, 3.0, 4.0]
    var outputBuf: [Float] = [0.0, 0.0, 0.0, 0.0]

    let globals = program.allocGlobals()
    globals.write(at: nGlobal.offset, value: Int32(4))

    inputBuf.withUnsafeMutableBufferPointer { inPtr in
        outputBuf.withUnsafeMutableBufferPointer { outPtr in
            globals.bindSlice(at: inputGlobal.offset, to: UnsafeBufferPointer(inPtr))
            globals.bindSlice(at: outputGlobal.offset, to: outPtr)

            let process = program.entryPoint(named: "process")!
            process.call(globals: globals)
        }
    }

    #expect(outputBuf[0] == 2.0)
    #expect(outputBuf[1] == 4.0)
    #expect(outputBuf[2] == 6.0)
    #expect(outputBuf[3] == 8.0)
}

@Test func multiEntryPoints() throws {
    let compiler = LyteCompiler(entryPoints: ["init", "process"])
    let program = try compiler.compile(source: """
        var counter: i32
        init {
            counter = 10
        }
        process {
            counter = counter + 1
        }
    """)

    let counter = try #require(program.global(named: "counter"))
    let globals = program.allocGlobals()
    let initEP = try #require(program.entryPoint(named: "init"))
    let processEP = try #require(program.entryPoint(named: "process"))

    initEP.call(globals: globals)
    let val1: Int32 = globals.read(at: counter.offset, as: Int32.self)
    #expect(val1 == 10)

    processEP.call(globals: globals)
    let val2: Int32 = globals.read(at: counter.offset, as: Int32.self)
    #expect(val2 == 11)
}

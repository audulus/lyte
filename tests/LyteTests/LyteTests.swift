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

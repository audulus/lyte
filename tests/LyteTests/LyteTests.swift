import Testing
@testable import Lyte

@Test func compileAndRun() throws {
    let compiler = LyteCompiler()
    let program = try compiler.compile(source: """
        main {
            assert(1 + 1 == 2)
        }
    """)
    program.run()
}

@Test func globalVariableMultiply() throws {
    let compiler = LyteCompiler()
    let program = try compiler.compile(source: """
        var x: i32
        main {
            x = x * 2
        }
    """)

    let x = try #require(program.global(named: "x"))
    #expect(x.type == "i32")

    program.writeGlobal(at: x.offset, value: Int32(2))
    program.run()

    let result = program.readGlobal(at: x.offset, as: Int32.self)
    #expect(result == 4)
}

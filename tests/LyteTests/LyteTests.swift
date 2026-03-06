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

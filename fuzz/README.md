
Requires nightly and cargo-fuzz:

```
rustup toolchain install nightly
cargo install cargo-fuzz
```

Fuzz targets:

```
cargo +nightly fuzz run lexer         # tokenizer only
cargo +nightly fuzz run parser        # parse to AST
cargo +nightly fuzz run checker       # parse + type check
cargo +nightly fuzz run compile_jit   # full pipeline: parse, check, JIT compile & run
cargo +nightly fuzz run compile_vm    # full pipeline: parse, check, VM compile & run
```

Useful flags:

```
cargo +nightly fuzz run compile_jit -- -max_total_time=120 -max_len=256
```

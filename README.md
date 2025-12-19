# jsnpar - JSON Parser 🦀

[![Crates.io](https://img.shields.io/crates/v/jsnpar.svg)](https://crates.io/crates/jsnpar)
[![License](https://img.shields.io/crates/l/jsnpar.svg)](LICENSE)

Rust JSON parser implementation using parser combinators.

> [!NOTE]
> Educational Project: This is a learning project and not recommended for production use. For production, use [serde_json](https://github.com/serde-rs/json).

## Motivation

As I'm learning Rust, I decided to write a minimal JSON parser using parser combinators. This is not intended to be used in production, but rather as a learning exercise.
Although I plan to iterate until I achieve a more robust implementation, I'm open to suggestions and contributions.

I created this crate thanks to Bodil's article on [parser combinators](https://bodil.lol/parser-combinators/). I highly recommend checking it out.

## Installation

### Cargo

```bash
cargo add jsnpar
```

## Usage

```rust
let json = r#"{"name": "yarso", "awesome": true}"#;
let result = jsnpar::parse(json);

if let Ok(jsnpar::JsonValue::Object(map)) = result {
    println!("{:?}", map);
}
```

## Known Limitations

This is a custom implementation tailored to my own needs, so there is plenty of room for improvement.

Feel free to fork the project, open issues, or contribute suggestions.

### Missing Features

- General optimizations
- Memory optimizations
- Custom errors types (currently returns a `String`)
- Streaming parser for large files
- Custom number types (only f64)
- Partial parsing
- More comprehensive test suite
- Performance benchmarks
- Examples and tutorials

### Alternatives

For production use, consider these battle-tested alternatives:

- [serde_json](https://github.com/serde-rs/json) - The standard, highly optimized
- [json](https://github.com/maciejhirsz/json-rust) - Simple and lightweight
- [simd-json](https://github.com/simd-lite/simd-json) - SIMD-accelerated parsing

## Contributing

This is primarily a learning project, but contributions are welcome! Feel free to:

- Open issues for bugs or feature requests
- Submit pull requests
- Share feedback on the code

## Acknowledgments

- [Bodil Stokke](https://github.com/bodil) for the parser combinators tutorial

Built with:
- [Rust](https://www.rust-lang.org/) - Because learning is fun

## License

MIT License

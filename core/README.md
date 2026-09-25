# seven_wonders_core

Rust 7 Wonders engine. Helios loads it as the Rustler NIF `Helios.Core.Native`
(`helios/lib/helios/core/native.ex`); use the `Helios.Core` wrapper from Elixir.

## Development

The toolchain is pinned in the repo-root `mise.toml`. From this folder:

```shell
mise exec -- cargo fmt --check
mise exec -- cargo clippy --all-targets -- -D warnings
mise exec -- cargo test
```

Helios compiles this crate automatically on `mix compile` (`path: "../core"`).

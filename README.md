# YURT

YUniversal RunTime

## Goals

- High portability
- Support for reentrant control flow (exceptions, async, effect handlers, etc.)
- Sandboxed execution
- Capability-driven
- Memory safety (programs cannot perform ACE)
- Performance, where doing so does not sacrifice other goals
- Small
- Good intra-module FFI
- Eval (modules can build and invoke modules)

## Non-goals

- JIT/AoT compilation

## Design

- Threaded interpreter
- Safe wrapper API for code generator

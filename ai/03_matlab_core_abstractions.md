# 03 — Core Abstractions

## @Base

Universal superclass. Provides copy semantics, property introspection methods. All domain classes inherit from it. Uses MATLAB handle semantics for reference passing.
Uses MATLAB handle semantics for reference passing; mutability is explicit and relied upon throughout the system.

## @AstroImage

Central data container. Array-capable: each element holds one astronomical image plus all associated metadata. Contains:

- `@AstroHeader` — FITS header key-value storage
- `@AstroCatalog` — Source catalog for the image
- `@AstroPSF` — Point Spread Function model
- `@AstroWCS` — World Coordinate System (sky-to-pixel mapping)
- Variance image
- `@MaskImage` — Pixel-level bitmask

Supports arithmetic and logical operations on images directly.
`@AstroImage` is the primary unit of computation; most image-processing functions accept and return it rather than raw arrays.


## @Component

Registry-based component architecture. Supports dynamic component loading and discovery via `@ComponentMap`.
`@Component` manages discovery and lifecycle, not business logic.

## @Configuration

Configuration management class. Reads YAML/INI/JSON config files from `config/`.

## @LogFile / @MsgLogger

Logging infrastructure. `@MsgLogger` for message-level logging, `@LogFile` for file-based output.

## @Dictionary

Key-value store utility class.

## ULTRASAT Planner Classes

- `uplanner` — Core planning logic (deterministic, testable)
- `DataModule` — State mediator between GUI, planner core, and API
GUI class — Thin App Designer shell; contains no planning or scoring logic


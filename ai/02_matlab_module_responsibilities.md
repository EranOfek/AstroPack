# 02 — Module Responsibilities

## matlab/astro/

Core astronomy packages. Scientific domain logic organized into `+` packages (`+astro`, `+celestial`, `+ultrasat`, `+telescope`, `+timeSeries`, `+VO`). Also contains legacy `@` classes (`@catsHTM`, `@AstroAngle`, `@UltrasatPerf`).

## matlab/image/

Astronomical image processing. Houses the primary data container classes (`@AstroImage`, `@AstroHeader`, `@AstroCatalog`, `@AstroPSF`, `@AstroWCS`, `@MaskImage`) and algorithm packages (`+imProc`, `+pipeline`).

## matlab/base/

Infrastructure and base classes. `@Base` is the universal superclass. Also contains `@Component`, `@Configuration`, `@LogFile`, `@MsgLogger`, `@ComponentMap`, `@FileManager`.

## matlab/util/

General-purpose utilities. Sub-packages: `+db` (database access layer), `+io` (file I/O), `+tools` (checksum, OS utilities), `+plot` (plotting). Also contains `@Dictionary`.

## matlab/apps/

Application layer. App Designer GUIs, API client classes (`+api`), the ULTRASAT planner subsystem (`+planner`), and long-running MATLAB worker services (`services/`, non-GUI, file-IPC based)


## matlab/mex/

C/C++ source files for MEX compilation. Performance-critical numerical kernels.

## matlab/external/

Third-party libraries and MEX binaries (`+yaml`, `fast_median`, `binarySearch`, etc.). Read-only — never modify.
Changes to external code must be done by version bump or replacement, never by in-place edits.

## matlab/obsolete/

Deprecated code retained for historical reference. Read-only — never extend or refactor.

## matlab/startup/

MATLAB startup scripts for different platforms and configurations.

## tests/

Function-based unit tests. Directory structure mirrors `matlab/`. One test file per source file, validating public behavior rather than internal implementation.


## config/

Configuration files (YAML, INI, JSON): `Installer.yml`, `UltrasatPlanner.yml`, `Database.DbConnections.*`, `BitMask.*.yml`, and others.

## database/

SQL schema definitions for PostgreSQL and ClickHouse.

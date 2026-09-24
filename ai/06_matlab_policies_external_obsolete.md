# 06 — Policies: External, Obsolete, and Drafts

## External Code

Locations: `matlab/external/`, `external/`

- Do not modify unless explicitly instructed
- Contains third-party libraries and precompiled MEX binaries
- Examples: `+yaml`, `fast_median`, `binarySearch`

## Obsolete Code

Location: `matlab/obsolete/`

- Read-only reference for historical context
- Never extend, refactor, or call from new code
- Kept to preserve institutional knowledge of past approaches

## Draft Folders

Folders named `Drafts-*` appear in various locations.

- Document failed or incomplete attempts
- Read-only — do not build upon or resurrect
- Useful only for understanding what was tried and why it was abandoned

# 01 — Domain Map and Intent

## Missions Served

- **ULTRASAT** — UV space telescope; observation planning, SNR calculation, slew optimization, target-of-opportunity handling
- **LAST** — Large-array Survey Telescope; ground-based survey pipeline, image processing, catalog management

## Scientific Domains

| Package | Domain |
|---------|--------|
| `+astro` | Core astronomy: celestial mechanics, Kepler equation, ephemeris, gravitational lensing, binary systems, accretion, supernovae, GRBs, extinction, dispersion |
| `+celestial` | Coordinate systems, conversions, star maps, JPL Horizons queries |
| `+ultrasat` | ULTRASAT-specific: planner, PSF, visibility, simulation, mission tools |
| `+telescope` | Optics, signal-to-noise calculations, speckle simulations |
| `+timeSeries` | Periodicity searches, time delay estimation |
| `+VO` | Virtual Observatory access, catalog formatting (VizieR, SDSS, PS1, GALEX) |
| `+imProc` | Image processing algorithms (calibration, photometry, astrometry, subtraction, coaddition) |
| `+pipeline` | End-to-end, reproducible data processing workflows composed of imProc and astro primitives |


## Intent

The codebase is a long-lived scientific framework emphasizing correctness, reproducibility, and numerical reliability over development speed. It is production infrastructure for active telescope missions, not a prototyping environment.

Design decisions favor explicitness and traceability over convenience.



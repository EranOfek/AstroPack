# AstSpec → AstroSpec migration: analysis and plan

Status: **proposal — no call site has been migrated yet.** The one piece of missing AstroSpec
functionality identified below has been implemented (`zodiacSpectrum`, §9); the migration options
in §6 are still open. Analysis performed on `dev1`, Aug 2026, against MATLAB R2020b Update 8.

The obsolete class `matlab/obsolete/@AstSpec` (3676 lines, ~80 methods) is still used by active
code under `+telescope/+sn`, `+ultrasat`, `@UltrasatPerf` and `+astro/+spec`. This document
inventories that usage, maps the old API onto `matlab/image/@AstroSpec`, and proposes a phased
replacement.

---

## 1. How the inventory was built (and why grep alone is not enough)

Two searches are needed, and their union is the real file list:

1. **Class-name search** — `AstSpec` as a literal (constructor, static factories, `isa`, typed properties).
2. **Method-name search** — bare method calls on AstSpec objects **never mention the class**. MATLAB dispatches on the object's class, so `interp(Spec, W)` silently resolves to `@AstSpec/interp`. These are invisible to search #1 and are where a migration breaks quietly.

Search #2 found call sites that search #1 misses entirely:

| Call | File:line | AstroSpec equivalent |
|---|---|---|
| `interp(...)` | `snr.m:165,178`, `snr_chen.m:256,269`, `UltrasatPerf.m:737,738` | `interp1` (**different name**) |
| `synthetic_phot(SG,...)` | `back_comp.m:78` | `synphot` — note the unprefixed call dispatches to `@AstSpec/synthetic_phot`, *not* `astro.spec.synthetic_phot` |
| `astspec2mat(...)` | `sn_spec.m:105,137,139`, `spec2photons.m:63` | none — use `[Obj.Wave, Obj.Flux]` |
| `scale2mag(...)` | `sn_spec.m:64` | `scaleSynphot` |

---

## 2. Inventory

~42 non-comment references across 16 files. `@AstSpec` itself is out of scope — it stays until its
consumers are gone.

### Tier 1 — mechanical (rename + arg shuffle)

| File | Uses |
|---|---|
| `astro/+astro/+spec/blackbody_mag_c.m` | `blackbody`, `.Int` |
| `astro/+telescope/+sn/unitTest.m` | `get_pickles` |
| `astro/+VO/+PS1/add_meta_data2ps1.m` | `get_galspec`, `synphot` |
| `astro/+ultrasat/usim.m` | `isa(..,'AstSpec')` dual-path, `.Int` — **already handles both classes**; migration means deleting the AstSpec branch |

### Tier 2 — semantic (orientation, preallocation, field renames)

| File | Uses |
|---|---|
| `astro/+telescope/+sn/snr.m` | `isastspec`, `blackbody`, `AstSpec(size(..))`, `interp`, `.Int` |
| `astro/+telescope/+sn/spec2photons.m` | `isastspec`, `blackbody`, `astspec2mat` |
| `astro/+telescope/+sn/back_comp.m` | `AstSpec(4)`, `get_galspec`, `synthetic_phot` (method dispatch) |
| `astro/+telescope/+sn/sn_spec.m` | `get_galspec`, `isastspec`, `get_atmospheric_extinction`, `scale2mag`, `astspec2mat` |
| `astro/+astro/+spec/fit_bb.m` | `AstSpec` ctor, `.Int`, `.IntUnits`, `.source` |
| `astro/+ultrasat/zodiac_bck.m` | `AstSpec(N,1)`, `.Int` |

### Tier 3 — blocked on a decision about AstroSpec itself

| File | Blocker |
|---|---|
| `astro/@UltrasatPerf/UltrasatPerf.m` | typed property `Specs(:,1) AstSpec = []` + `.ObjName` logic (52 inbound refs) |
| `astro/@UltrasatPerf2GUI/UltrasatPerf2GUI.m` | `.ObjName` for the GUI source list |
| ~~`astro/+ultrasat/zodiac_spectrum.m`~~ | **resolved** — ported as `AstroSpec.zodiacSpectrum`; see §9 |

### Tier 4 — candidates for *not* migrating (zero inbound references)

| File | Refs removed if retired |
|---|---|
| `astro/+telescope/+sn/snr_chen.m` | 8 — a near-duplicate of `snr.m` |
| `astro/+astro/+spec/fit_template2phot.m` | 3 |
| `astro/+astro/+spec/zodiac_bck.m` | 2 — superseded by `ultrasat.zodiac_bck` |
| `astro/+astro/+spec/spec_photon_counts.m` | self-declared OBSOLETE; calls a bare `blackbody()` that resolves to nothing |
| `obsolete/+ImUtil/+calib/fit_phot_transmission.m` | 1 — already under `obsolete/` |

**Retiring Tier 4 removes ~14 of ~42 references (a third of the work) for near-zero risk.**

Doc/comment-only, no action: `wget_sdss_spec.m` (documents an `'AstSpec'` OutType that is never
implemented), `SnrGuiIni.m` (commented-out), `black_body.m`, `synthetic_phot.m`, `zodiac_bck_V.m`.

---

## 3. API mapping

| AstSpec | AstroSpec | Note |
|---|---|---|
| `AstSpec(N,1)` / `AstSpec(size(X))` | `AstroSpec(N)` or `repmat(AstroSpec,1,N)` | **orientation flips** — see §4.1 |
| `AstSpec.blackbody(T, Wave, UnitsOut, UnitsWave, OutType)` | `AstroSpec.blackBody(Wave, T, ...)` | **arguments swapped**; units now via `Args`; no `'mat'` OutType |
| `AstSpec.get_pickles(SpC, SpL)` | `AstroSpec.specStarsPickles(SpType, LumClass, OutType)` | verified equivalent |
| `AstSpec.get_galspec(Name, OutType)` | `AstroSpec.specGalQSO(Name, OutType)` | verified equivalent |
| `AstSpec.get_atmospheric_extinction(Name, OutType)` | `AstroSpec.atmosphericExtinction(File, Args)` | different file keys (`'VLT'`/`'KPNO'`/`'SNfactory'`), `'mag'`\|`'trans'` |
| `AstSpec.isastspec(X)` | `isa(X, 'AstroSpec')` | |
| `AstSpec.zodiac_spectrum` | `AstroSpec.zodiacSpectrum` | ported, see §9 |
| `interp(S, W)` | `interp1(S, W)` | |
| `resample`, `equalize_sampling` | `interp1` / `interpLogSpace` / `interpAndKeepOverlap` | |
| `synphot`, `synthetic_phot` | `synphot` | |
| `scale2mag` | `scaleSynphot` | |
| `astspec2mat(S)` | `[S.Wave, S.Flux]` | |
| `shift`, `shift_vel` | `redshift`, `shiftWave`, `scaleWave` | |
| `extinction`, `atmospheric_extinction` | `applyExtinctionZ`, `applyAtmosphericExt` | |
| `.Int` / `.IntUnits` | `.Flux` / `.FluxUnits` | |
| `.Wave` / `.Err` / `.Back` / `.Mask` | `.Wave` / `.FluxErr` / `.Back` / `.Mask` | |
| `.ObjName` / `.source` / `.comments` / `.FileName` / `.AddCol` | **no equivalent** | see §4.3 |

---

## 4. Semantic hazards

### 4.1 Array orientation flips — measured, not assumed

| Construct | Result |
|---|---|
| `AstSpec(5,1)` | **5×1** |
| `AstroSpec(5)` | **1×5** |
| `repmat(AstroSpec,1,5)` | **1×5** |
| `AstSpec.blackbody([5000;6000], W)` | **2×1** |
| `AstroSpec.blackBody(W, [5000;6000])` | **1×2** |
| `AstSpec.get_pickles('M','V')` | **7×1** |
| `AstroSpec.specStarsPickles('M','V')` | **1×7** |

Linear indexing (`S(I)`) is unaffected. What breaks silently: `size(X,1)`, `[S.Wave]`
concatenation, and column-shaped typed properties.

### 4.2 `UltrasatPerf.m:58` is the hardest single item

```matlab
Specs(:,1)  AstSpec = [];   % column-constrained typed property
```

Every AstroSpec producer yields a **row**. Swapping the type alone fails validation on assignment.
Requires either `Specs(1,:) AstroSpec` or transposing at each producer — and with 52 inbound
references, this is the one item that cannot be done incrementally within the class.

### 4.3 Metadata gap — AstroSpec has no per-spectrum name

AstroSpec properties: `Data, MaskData, Z, Vel, DistZ, LumDist, Ebv, Zext, R, Lines, Ref`. There is
no `ObjName`/`source`. This is not cosmetic — it drives **behaviour**:

- `UltrasatPerf.m:306`: `F_BB = contains({Obj.Specs.ObjName},{'Planck'})` selects black-body spectra by name
- `UltrasatPerf2GUI.m:53`: builds the GUI source list from `ObjName`
- `zodiac_spectrum.m:139`, `fit_bb.m:178` set `ObjName`/`source`

`Ref` was declared in AstroSpec but not read or written anywhere in the class — a free slot that
could carry this. Options: (a) reuse `Ref`, (b) add `ObjName`/`Source` properties to AstroSpec,
(c) replace the name-matching logic with an explicit flag array. (a) and (b) modify the new class.

`zodiacSpectrum` (§9) provisionally writes provenance into `Ref`, which is the only use of that
property so far. That is a placeholder, not a decision — if (b) or (c) is chosen, it is one line
to change.

---

## 5. Verification assets

Baseline captured on R2020b Update 8 during this analysis. The capture script is not committed;
the numbers below are the reference values.

**Runs today (usable as phase gates):**

| Path | Baseline |
|---|---|
| `telescope.sn.snr()` | 20 numeric fields, e.g. `SNR = 16.760056800977033`, `LimMag = 22.595671310823047`, `ZP = 29.424007108958364`, `IntSignal = 1805.6758976224146` |
| `telescope.sn.back_comp()` | 4×1 AstSpec; per element `(Npt, Wmin, Wmax, sum(Flux), max(Flux))` |
| `telescope.sn.unitTest()` | passes |
| `AstroSpec.unitTest()` | passes |
| `AstSpec.blackbody`, `get_pickles`, `get_galspec`, `zodiac_spectrum` | per-spectrum fingerprints |

**Cannot be tested here:** `telescope.sn.sn_spec()` fails on a hardcoded absolute path —
`/raid/eran/matlab/data/+cats/+spec/+SkyBack/Gemini_SkyBack_dark.mat`. Anything routed through it
is migrate-by-inspection only. `usim` and `UltrasatPerf` were not exercised and need their own
fixtures.

**The gate should be "numerically identical to baseline", not "no error."** A migration that
renames flux fields and flips array orientation is precisely the kind that keeps running while the
numbers move.

---

## 6. Options

### A — Big bang

All 16 files in one pass, delete `@AstSpec`. Fastest to a clean tree; one large untestable step;
`sn_spec`/`usim`/`UltrasatPerf` regressions would surface only in ULTRASAT runs. **Not recommended.**

### B — Retire Tier 4, then phased migration *(recommended)*

1. Decide Tier 4 (retire/delete) — removes ~⅓ of references at near-zero risk.
2. Resolve the two blockers (§4.3 metadata, §4.2 typed property) as explicit AstroSpec decisions.
3. Migrate Tier 1 → Tier 2 → Tier 3, gating each on the numeric baseline.
4. Retire `@AstSpec` once its reference count reaches zero.

Each phase is independently verifiable and revertible.

### C — Compatibility shim first

Add `AstroSpec.fromAstSpec` / `astSpec2astroSpec` plus thin deprecating aliases (`interp`→`interp1`,
`.Int`→`.Flux`), migrate gradually behind them. Lowest risk per step, but introduces new API
surface in the new class and a second deprecation cycle later. Reasonable if migration must be
spread over months.

### D — Freeze

Keep `@AstSpec` for legacy paths; forbid new usage; migrate only files under active development.
Zero risk, indefinite duplication. Legitimate if ULTRASAT delivery pressure outweighs cleanup —
`usim.m` already accepts both classes, which is evidence this is the de-facto current strategy.

---

## 7. Recommended sequence (Option B)

| Phase | Work | Gate |
|---|---|---|
| 0 | Tier 4 decision; extend baseline to `usim`/`UltrasatPerf` fixtures | baseline captured |
| 1 | Metadata decision (§4.3). ~~`zodiac_spectrum` port~~ — done, see §9 | new static matches `AstSpec.zodiac_spectrum` fingerprint |
| 2 | Tier 1 files (4) | `telescope.sn.unitTest`, `AstroSpec.unitTest` pass |
| 3 | Tier 2 files (6) | `snr()` numerically identical; `back_comp()` fingerprints identical |
| 4 | `UltrasatPerf` + GUI + `usim` AstSpec branch removal | UltrasatPerf fixture identical |
| 5 | Move `@AstSpec` to a dated attic or delete; drop the `blackbody`/`black_body` doc pointers | zero references |

Phases 2–5 are each a separate commit/PR.

---

## 8. Decisions needed before any code is written

1. **Tier 4** — migrate anyway, move to `obsolete/`, or delete? (~⅓ of the work)
2. **Metadata (§4.3)** — reuse `Ref`, add `ObjName`/`Source` to AstroSpec, or refactor the name-based logic away?
3. **`UltrasatPerf.Specs` orientation (§4.2)** — change the property to a row, or transpose at producers?
4. ~~**`zodiac_spectrum`**~~ — **decided**: ported into `@AstroSpec` as a static; `ultrasat.zodiac_spectrum` now delegates to it.
5. **Option A/B/C/D**, and whether `@AstSpec` is ultimately deleted or archived.

---

## 9. Related fixes already applied

- `@AstSpec/AstSpec.m` constructor: `AstSpec(size(X))` left `N` as the full size vector
  (issue #1184, fixed in acff1a42c).
- `@AstroSpec/AstroSpec.m` `funBinary`: `AstroSpec([Nobj,1])` hit the numeric-matrix branch of the
  constructor and built a 1×1 object holding a bogus `Wave=Nobj, Flux=1` row instead of an
  Nobj-element array. Harmless in practice — the loop overwrote every element — but the same class
  of bug as #1184. Now `AstroSpec(Nobj)`.

Note that `AstroSpec` deliberately **cannot** accept a size vector: `AstroSpec([100 2])` is
ambiguous between "100×2 array of spectra" and "a one-row data matrix", and the constructor
resolves it as data. Preallocation must use the scalar form.

- `@AstroSpec/AstroSpec.m` `funBinary`/`rdivide`: wrong element indexing and unimplemented
  divisor types (issue #1185, fixed in d5ce9c3a7). `funBinary` had broken `applyAtmosphericExt`
  for any vector airmass.
- `@AstroSpec/AstroSpec.m`: added the missing `zodiacSpectrum` static (the §3 gap). It carries the
  HST STIS sky-background table, supports `BackType` `'zodi'|'earthshine'|'total'|'all'` and
  `OutType` `'AstroSpec'|'mat'`, and returns a 1x3 object array for `'all'`. Provenance goes into
  the otherwise unused `Ref` property — a placeholder pending the §4.3 metadata decision, not a
  commitment. `ultrasat.zodiac_spectrum` now delegates to it and keeps its `'mat'` default and its
  `'astspec'` option; output is bit-identical to the previous implementation across all
  `BackType`/`Wave`/`InterpMethod` combinations. The obsolete `AstSpec.zodiac_spectrum` was left
  untouched; note it is buggy — supplying `Wave` with the default `'astspec'` output errors,
  so its own documented example does not run.

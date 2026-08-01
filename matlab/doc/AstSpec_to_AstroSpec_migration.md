# AstSpec → AstroSpec migration: analysis and plan

Status: **plan agreed, no call site migrated yet.** The strategy and all blocking questions are
decided (§8); Phase 0 has not started. The one piece of missing AstroSpec functionality has been
implemented (`zodiacSpectrum`, §9). Analysis performed on `dev1`, Aug 2026, against MATLAB R2020b
Update 8.

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
| ~~`astro/+telescope/+sn/unitTest.m`~~ | `get_pickles` — **moved to Tier 2**: it feeds `telescope.sn.snr`, which still tests `AstSpec.isastspec`. An AstroSpec fails that test and, since `numel` of a scalar object is 1, is silently treated as a blackbody *temperature* (`snr.m:181`). Must migrate with `snr`. |
| ~~`astro/+VO/+PS1/add_meta_data2ps1.m`~~ | **retired to `obsolete/`** — `checkcode` reports 3 parse errors (a literal `????` at line 66) and it has no callers, so it has never been runnable |
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

### Tier 3 — depends on the AstroSpec decisions in §8

| File | Blocker |
|---|---|
| `astro/@UltrasatPerf/UltrasatPerf.m` | typed property `Specs(:,1) AstSpec = []` (§4.2) + `.ObjName` logic (§4.3) + saved `.mat` (§4.4) |
| `astro/@UltrasatPerf2GUI/UltrasatPerf2GUI.m` | `.ObjName` for the GUI source list |
| ~~`astro/+ultrasat/zodiac_spectrum.m`~~ | **resolved** — ported as `AstroSpec.zodiacSpectrum`; see §9 |

### Tier 4 — not migrating; moving to `obsolete/` (zero inbound references)

| File | Refs removed if retired |
|---|---|
| `astro/+telescope/+sn/snr_chen.m` | 8 — a near-duplicate of `snr.m` |
| `astro/+astro/+spec/fit_template2phot.m` | 3 |
| `astro/+astro/+spec/zodiac_bck.m` | 2 — superseded by `ultrasat.zodiac_bck` |
| `astro/+astro/+spec/spec_photon_counts.m` | self-declared OBSOLETE; calls a bare `blackbody()` that resolves to nothing |
| `obsolete/+ImUtil/+calib/fit_phot_transmission.m` | 1 — already under `obsolete/` |

**Decided: move these to `obsolete/`** — removes ~14 of ~42 references (a third of the work)
for near-zero risk, and keeps them findable.

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
| `.ObjName` | `.ObjName` | added in Phase 1; `specStarsPickles` and `blackBody` populate it with the same strings AstSpec used |
| `.source` / `.comments` / `.FileName` / `.AddCol` | `.Ref` (provenance) or none | see §4.3 |

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

### 4.2 `UltrasatPerf.Specs` orientation — small, once measured

```matlab
Specs(:,1)  AstSpec = [];   % column-constrained typed property
```

Every AstroSpec producer yields a **row**, so the declared type cannot simply be swapped. But the
blast radius is two lines, not the whole class:

- exactly **one** write: `Obj.Specs = Obj.create_Specs(...)` (`UltrasatPerf.m:405`)
- every read is orientation-insensitive: `numel(Obj.Specs)` (`:410`, `:771`), `Obj.Specs(Sidx)`
  linear indexing (`:214`, `:423`, `:793-800`), `{Obj.Specs.ObjName}` cell expansion (`:306`,
  `UltrasatPerf2GUI.m:53`)

**Decided:** declare `Specs(1,:) AstroSpec` and have `create_Specs` build a row.

(An earlier revision of this document called this "the one item that cannot be done incrementally".
That was wrong — the 52 inbound references are to the *class*, not to this array's shape.)

### 4.3 Metadata gap — AstroSpec has no per-spectrum name

AstroSpec properties: `Data, MaskData, Z, Vel, DistZ, LumDist, Ebv, Zext, R, Lines, Ref`. There is
no `ObjName`/`source`. The two consumers need **different** things:

- `UltrasatPerf.m:306`: `F_BB = contains({Obj.Specs.ObjName},{'Planck'})` — used *only* to colour
  points in the `'ColorColor'` plot. `create_Specs` builds the Pickles block and then the blackbody
  loop itself, so the class already knows which spectra are blackbodies; matching on a name string
  is fragile for no benefit.
- `UltrasatPerf2GUI.m:53`: `Obj.Sources = string({...Specs.ObjName})` — a genuine need for
  human-readable names in the GUI source list. No refactor removes this.

**Decided:** add an `ObjName` property to AstroSpec for the display names, and replace the
`'Planck'` string match with an explicit flag set by `create_Specs`. `Ref` stays for provenance,
which is how `zodiacSpectrum` (§9) already uses it.

**Phase 1 (done):** `ObjName` added; `specStarsPickles` and `blackBody` populate it with the same
strings AstSpec produced — verified byte-identical for Pickles classes M/G/A/B (`'M 0.0 V'`) and for
blackbodies (`'Planck spectrum T=5800.000000'`). The filename parse lives in the new static
`AstroSpec.picklesName`. The `create_Specs` flag refactor stays in Phase 4.

**Gap found in Phase 1, blocking Phase 4:** `UltrasatPerf.create_Specs` calls
`AstSpec.get_pickles([], MStype)` — empty spectral type plus a luminosity class, returning all 35
class-V spectra. `AstroSpec.specStarsPickles([], 'V')` returns `[]`, because an empty first argument
means "list mode" there. Phase 4 needs `specStarsPickles` to support this, or `create_Specs` must
enumerate the classes itself.

Note also that the saved fixture holds 43 spectra whereas 35 Pickles + 11 blackbody temperatures is
46, so the stored object came from a different configuration. The Phase 4 gate is the
*recomputation* from the stored `Specs` (§5), which is unaffected; a regenerated `.mat` may
legitimately contain a different number of spectra.

### 4.4 Serialized objects on disk

`UltrasatPerf2GUI` loads `P90_UP_test_60_ZP_Var_Cern_21.mat` (present locally under
`~/matlab/data/ULTRASAT/`). It contains a live `UltrasatPerf` whose `Specs` is a **43x1 AstSpec
array** carrying real names:

```
Specs class AstSpec size [43 1]
ObjNames(1:5): A 0.0 V | A 2.0 V | A 3.0 V | A 5.0 V | A 7.0 V
```

Migrating the class does not migrate that file: deleting `@AstSpec` would make it unloadable, and
re-declaring `Specs` as `AstroSpec` makes the saved array fail property validation on load. This is
independent of orientation — it follows from the class change itself.

**Decided:** `@AstSpec` is **archived, not deleted**, so old files keep loading; the `.mat` is
regenerated from migrated code in Phase 4 and the GUI switched to the new file. Users with their own
saved objects keep working as long as `@AstSpec` remains on the path.

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

**UltrasatPerf fixture (Phase 0, gate for Phase 4)** — captured from the saved object the GUI
loads, `~/matlab/data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat`:

| Quantity | Value |
|---|---|
| `Specs` | `AstSpec`, 43x1 |
| `C_Gaia_BpRp` | 43x1, sum 22.626722855507 |
| `C_ULTRASAT_GaiaG` | 43x25, sum 3720.609014252 |
| `C_ULTRASAT_GalexNUV` | 43x25, sum -2496.2778528443 |
| `EffPSF` | 43x25, sum 26207.887805063 |
| `LimMag` | 43x25, sum 23531.575082717 |
| `SatMag` | 43x25, sum 12349.240925006 |
| `ZP` | 43x25, sum 30294.347771999 |
| `VarPerPix` | 43x25, sum 231223.08070326 |

The live gate is a **recomputation**, not just stored values:
`UltrasatPerf.calcColor(UP.Specs,'GAIA','BP','GAIA','RP')` reproduces the stored `C_Gaia_BpRp`
exactly (sum 22.626722855507, first -0.27322175073511, last -0.82393084118771). It runs
`Specs -> synthetic_phot` end to end, so Phase 4 must reproduce it after `Specs` becomes AstroSpec.

**usim fixture (Phase 0, gate for Phase 4)** — `usim` adds random noise, so it is only a gate when
the RNG is seeded. With a fixed seed it is bit-reproducible:

```matlab
rng(42,'twister');
Sim = ultrasat.usim('Cat',[2369 2369], 'SkyCat',false, 'Mag',12);
sum(Sim.Image, 'all')   % 4986678784   (4738x4738, max 480000)
```

Verified reproducible across runs. Without the seed, successive runs differ by ~2e-5 relative, so an
unseeded comparison needs a tolerance of about 1e-3 and only detects gross changes.

Notes: `usim` peaks at ~6.8 GB RSS and takes ~9 s at the default `ImRes=5`; on a 16 GB machine it can
die with a segmentation violation under memory pressure, which is a resource limit, not a code fault.
Lower `ImRes` values need PSF databases (`ULTRASATlabPSF<N>.mat`) that are not installed here — only
`ULTRASATlabPSF5.mat` is present.

**Cannot be tested here:** `telescope.sn.sn_spec()` — hardcoded absolute path
`/raid/eran/matlab/data/+cats/+spec/+SkyBack/Gemini_SkyBack_dark.mat`. In Tier 2, so Phase 3 is
migrate-by-inspection for that one file.

**The gate should be "numerically identical to baseline", not "no error."** A migration that
renames flux fields and flips array orientation is precisely the kind that keeps running while the
numbers move.

---

## 6. Options

### A — Big bang

All 16 files in one pass, delete `@AstSpec`. Fastest to a clean tree; one large untestable step;
`sn_spec`/`usim`/`UltrasatPerf` regressions would surface only in ULTRASAT runs. **Not recommended.**

### B — Retire Tier 4, then phased migration  ← **CHOSEN**

1. Retire Tier 4 — removes ~⅓ of references at near-zero risk.
2. Resolve the blockers (§4.2, §4.3, §4.4) as explicit AstroSpec decisions. **Done — see §8.**
3. Migrate Tier 1 → Tier 2 → Tier 3, gating each on the numeric baseline.
4. Archive `@AstSpec` once its reference count reaches zero.

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

## 7. Sequence (Option B, as decided)

| Phase | Work | Gate |
|---|---|---|
| 0 | Move Tier 4 to `obsolete/`; extend the baseline to `usim`/`UltrasatPerf` fixtures | **done** — see §5; both fixtures captured |
| 1 | Add `ObjName` to AstroSpec (§4.3) and populate it in `specStarsPickles` and `blackBody` | **done** — names byte-identical to AstSpec; `AstroSpec.unitTest` passes |
| 2 | Tier 1 files | **done** — `usim` and `blackbody_mag_c` migrated, `add_meta_data2ps1` retired, `telescope.sn.unitTest` deferred to Phase 3 |
| 3 | Tier 2 files (6) | `snr()` numerically identical; `back_comp()` fingerprints identical |
| 4 | `UltrasatPerf` (`Specs(1,:) AstroSpec` + `SpecIsBB` flag) + GUI + `usim` AstSpec branch removal; regenerate the `.mat` (§4.4) | UltrasatPerf fixture identical; GUI source list unchanged |
| 5 | Archive `@AstSpec` (keep loadable); drop the `blackbody`/`black_body` doc pointers | zero references |

Phases are each a separate commit/PR.

The `zodiac_spectrum` port that used to be Phase 1 is already done — see §9.

---

## 8. Decisions taken

| # | Question | Decision |
|---|---|---|
| 1 | Overall strategy | **Option B** — phased, per §7 |
| 2 | Tier 4 (5 files, zero inbound refs) | **Move to `obsolete/`** — keeps them findable, removes ~14 refs |
| 3 | Metadata (§4.3) | **Add `ObjName` to AstroSpec** + replace the `'Planck'` match with an explicit flag |
| 4 | `UltrasatPerf.Specs` orientation (§4.2) | **Declare `Specs(1,:) AstroSpec`**; `create_Specs` builds a row |
| 5 | Saved `.mat` files (§4.4) | **Archive `@AstSpec`, do not delete**; regenerate the `.mat` in Phase 4 |
| 6 | `zodiac_spectrum` | **Ported** to `AstroSpec.zodiacSpectrum`; `ultrasat.zodiac_spectrum` delegates — see §9 |

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

- `astro/+astro/+spec/black_body.m`: replaced the hardcoded `h`, `c`, `k` with `constant.h`,
  `constant.c`, `constant.kB` (Phase 2). The hardcoded `h = 6.6261e-27` differed from
  `constant.h = 6.6260755e-27` by 3.7e-6, which the Planck exponent compounded into a ~1.4e-5
  flux offset — so `AstroSpec.blackBody` (which delegates here) and `AstSpec.blackbody` (which
  used the `constant` class) disagreed by up to 4.4e-5. They now agree to 4.4e-16, and
  `blackbody_mag_c` migrated with **bit-identical** output. The comments in `black_body` already
  read `% = get_constant('h','cgs')`, so this restores the original intent.

  Note this shifts `astro.spec.black_body` output by ~1e-5 relative for its other callers —
  `accretion_disk`, `accretionDiskSpec`, `blackbody_flux`, `sn_cooling_msw`, `sn_cooling_rw_my`,
  `matchspec` — in the direction of the more accurate constants.

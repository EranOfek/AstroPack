# LAST / ULTRASAT Science Portal — Science-First Review

### A critical read of the query-workload document, from the science side

**What this is.** The previous document (`LAST_ULTRASAT_Science_Query_Workload.md`) asked
*"what queries will be run, and how do we make them fast?"* This document asks the prior
question: *"what does a scientist actually need to measure, and can this archive support a
publishable measurement?"* Those are not the same question, and the gap between them is where
archives fail.

**Reviewers' composite perspective.** Written as a panel would review it: explosive transients
and SN progenitors; gravitational-wave and multi-messenger follow-up; stellar activity and
exoplanets; solar system; survey statistics and rates; photometric calibration and systematics.
Where the panel would disagree with itself, I say so.

**Headline verdict.** The workload document is competent engineering built on an incomplete
model of what astronomy is. It catalogued **retrievals** — "give me the rows matching X" — when
almost every real result in time-domain astronomy is a **ratio**: detections divided by
opportunities, events divided by control time, variables divided by stars monitored at that
precision. The denominator was almost entirely absent. An archive that serves numerators
beautifully and denominators not at all produces fast queries and unpublishable papers.

Nine further challenges follow. Then §2 gives the science-question catalogue the document should
have been built on, §3 the cross-cutting requirements that fall out of it, §4 the questions the
archive genuinely cannot answer today, and §5 the decisions only the science team can make.

**Reference points assumed** (please correct if the flight/as-built values differ):
LAST — 48 × 28 cm f/2.2, 7.4 deg² each, ~355 deg² simultaneous in wide mode, 1.25″/pix,
delivered image quality ~2.2–2.8″, 5σ ≈ 19.6 mag in 20 s and ≈ 21.0 in a 20 × 20 s visit
(unfiltered, ≈ Gaia B_P), astrometric rms ≈ 60 (30) mas, absolute photometric calibration
≈ 10 mmag against Gaia, ~1 mmag precision at B_P ≈ 13 (single) / 14.5 (visit), 4 telescopes per
mount. ULTRASAT — 33 cm effective, 204 deg², 230–290 nm, 22.5 AB (5σ, 900 s) for a hot source,
~20.5–20.8 for an M dwarf, all-sky NUV to > 23.5 AB, 21 h/day on a single high-cadence field
(changing every ~6 months) plus wide-area survey, alerts in < 15 min, GEO, > 50 % of sky
instantaneously accessible, launch currently ~Q4 2027.

---

## 1. Ten challenges

### C1. You catalogued numerators. Science needs denominators.

Nearly every question in §4 of the workload document has the form *"find the objects that…"*.
Almost no publishable time-domain result has that form. The real forms are:

- *"The rate of X is R ± σ"* — needs events ÷ (effective volume × control time).
- *"Fewer than N objects of type X exist brighter than m"* — needs the completeness curve.
- *"The fraction of M dwarfs that flare above E is f"* — needs the monitored sample and per-star
  sensitivity, not the flare list.
- *"The early light curve rules out a progenitor radius > R"* — needs the **non-detection** and
  its true depth at that epoch.

The archive's central data product is therefore not the detection table. It is the
**selection function**: the probability of detecting a source of given brightness, colour,
morphology, angular speed and variability timescale, at a given position and time, in a given
local stellar density. That object does not exist in the current design, is not derivable
after the fact from what is stored, and cannot be added later without re-running injection tests.

**This is the single largest omission and it is not a database problem.** It requires
source-injection ("fake") runs through the pipeline, stored per image or per image-family, and it
requires deciding that now, while images are still on disk.

### C2. Non-detections were one scenario. They are half the data.

The workload document treated upper limits as scenario B5. Reverse the priority. For SN shock
cooling — the flagship of both projects — the scientifically decisive measurement is often the
**last non-detection before explosion** and its depth, not the detections. For rates, the entire
constraint comes from non-detections. For GW follow-up, "we covered 60 % of the localisation to
22.5 mag and saw nothing" is the result.

Three distinct states must be distinguishable, and today only the first is:
1. **Detected** — a row exists.
2. **Observed, not detected** — no row; you must reconstruct coverage and depth.
3. **Not observed** — no row; indistinguishable from (2) without a coverage product.

Conflating (2) and (3) does not make an analysis noisy. It makes it wrong, and wrong in the
direction of claiming rates that are too high.

**The panel's position: a light curve that does not include upper limits at every epoch the field
was observed is not a light curve, and the portal should not serve one.**

### C3. You made the 400 s visit coadd the atom. LAST's best science lives at 20 s.

The schema records `visit_src` (20 × 20 s coadded) as the fundamental catalogue. That choice
silently deletes the survey's most distinctive capability. Consequences the panel would raise
immediately:

- **Occultations by KBOs and asteroids** are sub-second to few-second events. Utterly invisible
  in a 400 s coadd. This is a stated LAST science goal and the current data model cannot support
  it at all.
- **Fast optical flashes** — GRB prompt/early afterglow optical emission, and any transient with
  a rise time under a few minutes — are diluted by a factor of ~20 in flux contrast when coadded.
- **Fast-moving objects trail.** A NEO moving at ≳ 0.5″/min moves ≳ 3–4″ during a 400 s visit
  window — larger than the PSF. PSF photometry then *underestimates* the flux and the position is
  smeared. The coadd therefore imposes a selection bias against precisely the closest, fastest,
  most interesting solar system objects. The individual 20 s frames do not have this problem.
- **Intra-visit variability is already computed and then thrown away.** The pipeline applies a
  relative zero point across the 20 epochs of a visit; those 20 points are exactly the data for
  white-dwarf pulsations, flare rise times, and eclipse ingress timing.

The pragmatic panel view: you cannot keep per-frame photometry for every source forever at
2.2 Gbit/s. But you can keep it **selectively and by policy** — for a curated target list (WDs,
M dwarfs, occultation candidates, known variables, solar system predictions), for anything the
visit-level analysis flags as variable, and for a rolling recent window. What you cannot do is
discover, three years in, that you needed it. **This is a now-or-never decision.**

### C4. "Unique source" was treated as a spatial hash. It is a physical model.

Matching detections at a HEALPix resolution is an implementation. Deciding *what counts as the
same object across three years* is astrophysics, and it has at least five failure modes that the
workload document did not consider:

1. **Proper motion.** At LAST's 30–60 mas astrometric precision, a star with μ = 200 mas/yr moves
   ~0.6″ over three years — comparable to or larger than a fine HEALPix cell and to the match
   radius. Nearby stars (the ones with interesting flares and planets) are exactly the high-PM
   population. Position-only matching will split them into multiple "objects", each with a
   truncated light curve, and will do so *preferentially for the most scientifically valuable
   targets*.
2. **Blending.** With ~2.2–2.8″ image quality and 1.25″ pixels, in the Galactic plane a
   substantial fraction of "sources" are blends whose centroid moves with seeing. Object identity
   becomes seeing-dependent — and so does the photometry.
3. **Variability-induced astrometric shift.** A variable in a blend pulls the centroid as it
   brightens. The astrometric wobble is a *signal*, not noise (it is how you tell which component
   varies), and it breaks naive matching.
4. **Objects with no static counterpart.** SNe, novae in outburst, orphan afterglows exist only
   in difference images. They need object identity too, in the same namespace, or cross-referring
   between a transient and its host/progenitor becomes a manual exercise.
5. **Moving objects** must be *excluded* from static object identity, and linked into a separate
   identity concept (tracklet → orbit → designation).

**The panel's position: match against Gaia positions propagated to the epoch of observation,
not against a static mean position**, and carry an explicit identity-quality flag (clean / high-PM
/ blended / crowded / ambiguous). Also record the *alternatives* — for an ambiguous match, which
other objects were within the radius. That information is unrecoverable later.

### C5. There are two photometries, and you stored the wrong one for the best science.

`mag_psf` is absolutely calibrated against Gaia at ~10 mmag. That is excellent for transients and
useless for the mmag science. Exoplanet transits around bright stars (~1 % depth, but ~0.1 % for
the interesting shallow cases), WD g-mode pulsations, δ Scuti, and low-amplitude eclipsing
binaries all live *below* the absolute calibration floor. They are done with **relative /
ensemble photometry**: differential magnitudes against a comparison ensemble on the same
detector, in the same image, with common-mode systematics divided out.

The pipeline already computes a relative zero point within a visit. The archive does not expose
it, and there is no per-epoch relative magnitude column, no comparison-ensemble definition, and
no per-image residual/systematics map.

**Consequence:** every mmag user will re-derive their own ensemble photometry by downloading all
sources in the field for all epochs — the single most expensive access pattern in the archive,
performed repeatedly, by every user, to recompute something the pipeline already knows.

**The panel's position: store both.** `mag_psf` (absolute, for transients) and `mag_rel`
(ensemble-corrected, with the ensemble definition and per-image correction recorded, for
variability). One extra column and one small per-image table converts the archive's worst access
pattern into its cheapest.

### C6. Your time axis is under-specified, and periodicity science needs the sampling, not the samples.

Four separate issues the workload document flattened into "jd":

- **Which time?** Shutter open, mid-exposure, mid-visit-window, or flux-weighted mid-point? For a
  400 s window containing 20 × 20 s exposures with dead time, these differ by up to ~200 s. For
  eclipse timing and pulsation phasing that is a large systematic. The `id` encoding (start time)
  and the `jd` column (mid-exposure) already differ by exactly half the exposure — that difference
  must be documented, not discovered.
- **Exposure smearing.** A light curve point is an integral over 400 s, not a sample. For anything
  varying on comparable timescales (eclipse ingress, flare rise, occultation) the archive must
  return the *integration window*, and analysis must forward-model it. Store the effective
  window, not just a timestamp.
- **Barycentric correction depends on the target position** — fine for a static star, wrong for a
  moving object, where light-time to the *object* (not the barycentre) is what matters. Solar
  system light curves need light-time-corrected epochs, which requires the geocentric distance,
  which is not stored (see §2, S11).
- **The window function.** Period searches on ground-based data are dominated by aliases at
  1 sidereal day and its harmonics, the lunar cycle, and the annual observability window
  (`raw_images` even carries the observability metadata to compute it). A period without the
  window function is uninterpretable. **The per-object sampling pattern is a data product**, and
  for the ULTRASAT high-cadence field — 21 h/day of 300 s exposures with Earth-eclipse and
  ToO interruptions — it is a *much* better-behaved but still non-trivial one.

### C7. Difference-image photometry is relative to a reference you never asked about.

`diff_src` gives flux relative to `id_ref_im`. The workload document treated the reference as a
join key. Scientifically it is a source of silent bias:

- If the object was **present and variable in the reference epochs**, the difference flux is
  offset by an unknown constant. Total flux = reference flux + difference flux, and the pipeline
  helpfully provides `r_flux_psf` — but if users are not *told* to add them, half of them won't.
- If the transient itself is **in** the reference (late-time reference, recurring transient,
  slow-fading SN), you get self-subtraction and systematically underestimated fluxes.
- Reference **depth and epoch range** set the effective detection threshold for the difference,
  which varies across the survey and across the sky.

**Requirement:** reference images must be first-class, queryable objects with their own epoch
range, depth, and contributing-image list, and every difference-based light curve must carry
that provenance. "Which reference, built from which nights" is a question a referee will ask.

### C8. "Single band, so no colour science" is exactly backwards.

The workload document noted `filter='clear'` and concluded colour is out of scope. The opposite
is true: because both instruments have **broad, non-standard bandpasses, the detected flux and
the limiting magnitude depend strongly on the source SED.**

- LAST's unfiltered band resembles Gaia B_P, and its measured limiting magnitude is an explicit
  function of B_P − R_P.
- ULTRASAT's 5σ depth differs by roughly **two magnitudes** between a ~20,000 K blackbody
  (≈ 22.5 in 900 s) and an M4 dwarf (≈ 20.5) — the same exposure, the same image, two very
  different depths depending on what you are looking for.

Therefore `limmag` is **not a scalar property of an image.** It is a function, `limmag(SED)`, and
storing one number per image bakes in a hidden, colour-dependent selection function. Worse, the
photometric calibration is tied to Gaia colours (`ph_col1` exists) — so the objects calibrated
*worst* are the ones with no Gaia colour, which is to say **the transients**.

**Requirement:** store `limmag` for a small set of reference SEDs (a hot blackbody, a solar
analogue, an M dwarf, and a typical SN spectrum at a couple of phases), or store the throughput
plus sky/noise terms so limmag can be computed for any SED on demand. This is cheap at the image
level and it is the difference between a rate measurement and a rate estimate.

### C9. Multi-messenger follow-up is a probability integral, not a cone search.

The workload document's F1 finds candidates inside a GW localisation. The question a GW paper
actually asks is:

> Given the skymap, our realised coverage (which tiles, when, to what depth, with what
> completeness), and an assumed kilonova luminosity function and light-curve model — **what is the
> probability that we would have detected a counterpart, and hence what does our non-detection
> constrain?**

That is a convolution of the skymap with coverage × depth(SED, time) × efficiency, and it
requires C1, C2, and C8 to exist. It also must be computable **within minutes**, because the
answer drives whether to keep observing. And it must be recomputable *retrospectively* with the
final skymap, which arrives days later — so the coverage record must be immutable and versioned.

The same integral, with different models, is the answer for SN rates, for orphan afterglow
limits, and for FRB optical counterparts. **Build it once as a service, not per paper.**

### C10. There is no notion of a *sample*, and a sample is the unit of science.

Papers do not cite queries; they define samples. "Our sample comprises all sources with
≥ 50 epochs, |b| > 15°, mean 14 < m < 17, in fields with median seeing < 4″, excluding blends."
That definition must be: expressible, frozen, versioned, citable, re-runnable against a specific
data release, and shareable with a collaborator and a referee.

Today a user has a SQL string in a notebook. That is not reproducible science, and when the
archive is re-reduced the paper silently becomes unverifiable.

**Requirement:** a **sample registry** — named, versioned, immutable sample definitions, each
bound to a data-release version, each with its selection function attached, each with a DOI-able
identifier. This is a modest engineering item and a large scientific one.

### C11 (bonus). Whole science domains were missing.

Absent from the workload document entirely: occultations by trans-Neptunian objects and
asteroids; comets and active asteroids; space debris and satellite streaks (both a contaminant
and a deliverable); microlensing; lensed quasars and lensed SNe (a stated LAST cosmology goal);
Galactic novae; AGN/blazar variability and UV reverberation mapping; white-dwarf science, which is
a major LAST driver (~10⁵ WDs monitored, planets and planetesimals around WDs, g-mode
pulsations); ULTRASAT's cosmology programme; and polarimetry, if the LAST-P node is in scope.
§2 covers these.

---

## 2. The science-question catalogue

Organised by physics, not by table. Each entry: the **question** a scientist asks, what the
archive must **return** for it to be answerable, the **trap** (the systematic or selection effect
that makes the naive answer wrong), and a **verdict** on the current design:
✅ answerable · ⚠️ answerable but biased or expensive · ❌ not answerable today.

---

### S1 — Supernova shock breakout and early emission
*The flagship science of both projects (ULTRASAT WG1; LAST early-SN programme).*

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S1.1 | When did this SN explode, to within hours? | Detections **plus** every non-detection with its true depth, from the last pre-explosion epoch onward | Depth is SED-dependent (C8) and the SN colour evolves fast; a single `limmag` gives the wrong explosion time | ❌ |
| S1.2 | What was the progenitor radius / envelope structure? | Densely sampled first ~24 h light curve with correct integration windows | 400 s smearing on a rising light curve; the visit coadd averages over the steepest part | ⚠️ |
| S1.3 | Is there a shock-cooling excess above the ⁵⁶Ni-powered rise? | Absolute photometry with well-characterised systematics near the detection limit | Faint-end flux bias (Eddington/Malmquist at low S/N); forced photometry needed, not detections | ⚠️ |
| S1.4 | What is the UV–optical colour temperature in the first hours? | LAST and ULTRASAT points **simultaneous to within minutes**, cross-calibrated | Different bandpasses, different SED-dependent calibration; no simultaneity index exists | ❌ |
| S1.5 | How many SNe do we detect within 1 day of explosion, per year? | Rate = events ÷ (control time × volume), with the SN-SED selection function | Pure denominator problem (C1) | ❌ |
| S1.6 | Did anything happen at this position in the months **before** explosion (precursor outbursts)? | Forced photometry at the SN position on every prior epoch, with limits, plus a stacked deep limit | Requires forced photometry on demand at arbitrary positions and epochs — a *service*, not a query | ❌ |
| S1.7 | What is the host galaxy, offset, and local environment? | Host association with offset in kpc, host redshift, local surface brightness | Needs external galaxy catalogue with redshifts; `gal_dist` alone is in arcsec and unusable for physics | ⚠️ |

**Panel note.** S1.6 deserves emphasis. Retroactive forced photometry at an arbitrary position
over the full archive is *the* most-requested capability of every transient archive ever built,
and it is not a SELECT — it is a pipeline job that reads pixels. Decide now whether the portal
offers it, because the answer determines whether processed images stay on spinning disk.

---

### S2 — Explosion physics and SN populations

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S2.1 | Rise-time distribution for Type II SNe | Homogeneous light curves + explosion-epoch constraints for a defined sample | Selection favours fast, bright, nearby; needs the efficiency curve vs rise time | ❌ |
| S2.2 | Which SNe show early flash-ionisation signatures (CSM)? | Blue/UV excess in the first days → colour → cross-mission | Same as S1.4 | ❌ |
| S2.3 | Volumetric rate of rare classes (Ca-rich, .Ia, SN Ibn) | Detections, control time, and completeness at low luminosity | Rare classes are found near the limit where the selection function is steepest and least known | ❌ |
| S2.4 | Are there SNe with no host detected? | Deep coadd photometry at the position + limits | Requires a deep static coadd catalogue, separate from visit catalogues | ⚠️ |
| S2.5 | Do SN Ia light curves in our band show the expected width–luminosity relation? | Well-calibrated peak magnitudes with colour-dependent zero points | Broad unfiltered band + SED-dependent calibration = large systematic for cosmology use | ⚠️ |
| S2.6 | Lensed SNe / lensed quasar time delays | Multi-year light curves of resolved or blended multiple images, with sub-arcsec astrometry | Image separations are typically < 2″ — below LAST resolution; needs the blend model, not a catalogue row | ❌ |

---

### S3 — Gravitational waves, kilonovae and multi-messenger
*(ULTRASAT WG2; LAST GW programme.)*

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S3.1 | What did we observe inside this GW localisation, and when? | Realised coverage per skymap pixel, with time and depth | Coverage, not detections. Requires an immutable coverage record | ❌ |
| S3.2 | What fraction of the localisation probability did we cover to depth m? | Probability-weighted coverage integral | Skymaps are revised days later — must be recomputable against any skymap version | ❌ |
| S3.3 | Are there new sources in the region, not present before? | Difference detections filtered against a reference built *before* the event | If the reference includes post-event epochs, the counterpart self-subtracts | ⚠️ |
| S3.4 | Given no detection, what kilonova models are excluded? | Coverage × depth(kilonova SED, phase) × efficiency, convolved with the skymap | The full C9 integral | ❌ |
| S3.5 | Rule out the known-variable and asteroid false positives, in < 15 min | Candidate context: known variable? Gaia star? asteroid? AGN? previous outbursts? | Needs sub-second context lookups against several catalogues under real-time load | ⚠️ |
| S3.6 | Was there UV emission simultaneous with the optical? | Cross-mission simultaneity within minutes | No simultaneity index | ❌ |
| S3.7 | Neutrino / FRB / GRB counterpart search in a large error region | Same machinery, different error region geometry and time window | Error regions can be tens to thousands of deg² with non-trivial shapes | ⚠️ |

**Panel note.** S3.1–S3.4 are the same product. Build the **coverage-and-efficiency service**
once; GW, neutrino, FRB, GRB, SN rates and orphan-afterglow limits are all queries against it.

---

### S4 — Fast transients, GRB afterglows, and the short-timescale sky

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S4.1 | Are there optical flashes lasting seconds to minutes? | Per-**frame** (20 s) photometry, not visit coadds | Structurally impossible on the current schema (C3) | ❌ |
| S4.2 | Orphan afterglows: rate of fast, fading, hostless transients | Detections in ≥ 2 frames, fading > ~1 mag/day, no host, no proper motion, not an asteroid | Every one of those vetoes needs a different catalogue join; false-positive rate dominated by asteroids and satellites | ⚠️ |
| S4.3 | Did we see anything at the position of this GRB within minutes of the trigger? | Serendipitous coverage lookup at (position, time) with per-frame limits | Needs coverage at *frame* granularity, not visit | ❌ |
| S4.4 | Fast blue optical transients (FBOTs), .Ia, rapidly evolving events | Days-long light curves with ≥ nightly cadence and reliable early limits | Selection is entirely cadence-driven; needs realised-cadence maps | ⚠️ |
| S4.5 | What is the false-positive population at the fastest timescales? | Labelled artefact/CR/satellite/asteroid samples with full feature vectors | Requires a labels store and preserved cutouts; labels are currently nowhere | ❌ |
| S4.6 | Single-frame transients: real or cosmic ray? | The individual frames plus their pixel data | Single-epoch detections are exactly where per-frame data is mandatory | ❌ |

---

### S5 — Tidal disruption events, AGN and nuclear variability

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S5.1 | Is this nuclear transient a TDE or an AGN flare? | Multi-year prior variability history at the position + host centroid offset (milliarcsec-level) | Requires a *pre-outburst* light curve, i.e. forced photometry over the archive (S1.6) | ⚠️ |
| S5.2 | UV–optical lag / reverberation in AGN | Densely sampled, precisely cross-calibrated LAST + ULTRASAT curves | Lag measurement is systematics-limited; needs relative photometry (C5), not absolute | ❌ |
| S5.3 | Structure function of AGN variability across the population | Per-object variability statistics with the sampling window attached | Window function is the dominant systematic in structure functions | ⚠️ |
| S5.4 | Changing-look AGN | Long-baseline, well-calibrated light curves with consistent object identity | Identity across re-reductions and data releases | ⚠️ |
| S5.5 | Nuclear transient offsets — is it really nuclear? | Astrometric offset with realistic uncertainty, relative to the host centroid in a deep coadd | Needs the deep static coadd astrometry and its error model | ⚠️ |

---

### S6 — Stellar flares, activity, and the star–planet connection
*(ULTRASAT WG3/WG5; a major LAST driver.)*

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S6.1 | What fraction of M dwarfs flare above energy E, per unit time? | Flare list **and** monitored-star list with per-star sensitivity and monitored time | The denominator is per-star, time-resolved, and depth-dependent — nowhere in the design | ❌ |
| S6.2 | Flare frequency distribution (FFD) slope for a stellar population | Energies (needs distance + bandpass correction) and completeness vs energy | Energy requires distance (Gaia) and an SED-dependent bolometric correction | ⚠️ |
| S6.3 | What is the flare rise/decay morphology? | Per-frame photometry through the flare | 400 s smearing destroys the rise; UV flares are minutes-long | ❌ (LAST) / ✅ (ULTRASAT 300 s, marginally) |
| S6.4 | Do flares on planet hosts differ? | Join to exoplanet host catalogues, per-star statistics | External catalogue currency and cross-identification | ⚠️ |
| S6.5 | Is the UV flare energy sufficient to affect habitability? | NUV energies with a real bandpass correction | ULTRASAT's M-dwarf depth is ~2 mag shallower than for hot sources (C8) — the selection function is *strongly* SED-dependent for exactly this population | ⚠️ |
| S6.6 | Superflares on solar-type stars | Long-baseline monitoring of a well-defined bright sample with mmag stability | Requires relative photometry (C5) | ⚠️ |

**Panel note.** S6.1 is the cleanest possible illustration of C1. The flare table is easy. The
**"stars monitored, when, to what flare-energy sensitivity"** table is the science, and it does
not exist.

---

### S7 — Variable stars, binaries and asteroseismology

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S7.1 | Find all periodic variables in this region with P < 1 d | Light curves + period search results + the window function | Sidereal-day and lunar aliases; period must be stored with the window that produced it | ⚠️ |
| S7.2 | Eclipsing binary ephemerides and O−C timing (third bodies, apsidal motion) | Precise mid-eclipse times → correct time semantics and integration windows (C6) | 400 s smearing on ingress/egress; BJD convention must be exact | ⚠️ |
| S7.3 | White-dwarf g-mode pulsations (minutes, mmag) | Per-frame photometry, relative to an ensemble | Requires C3 **and** C5 simultaneously — the two things not stored | ❌ |
| S7.4 | Compact WD binaries and their period distribution | Short-period search over a large WD sample (~10⁵ objects) | Same | ❌ |
| S7.5 | Amplitude/period distributions of RR Lyrae, δ Scuti, Cepheids across the survey | Per-object variability statistics + completeness vs amplitude and period | Detection efficiency for periodic variables depends on amplitude, period **and** the per-object window | ⚠️ |
| S7.6 | Which stars are *not* variable, to what limit? | Per-star photometric noise floor and monitored time | The denominator again; also the basis for the RMS-vs-magnitude systematics model | ❌ |
| S7.7 | Long-term secular variability (years) | Stable multi-year calibration across pipeline versions and hardware changes | Re-reductions and telescope maintenance introduce steps; needs DR-level cross-calibration | ⚠️ |

---

### S8 — Exoplanets

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S8.1 | Blind transit search around bright stars (B_P ≈ 13–15.5) | mmag-precision relative light curves, long baselines, per-star noise model | Absolute calibration floor (~10 mmag) is above the signal; C5 is mandatory | ❌ |
| S8.2 | Transits and planetesimals around white dwarfs | Per-frame photometry for ~10⁵ WDs (transits are minutes long) | C3 + C5 | ❌ |
| S8.3 | Transit-timing variations | Precise mid-transit times → C6 | Integration-window forward modelling | ⚠️ |
| S8.4 | Follow-up/confirmation of TESS or ground-based candidates | Photometry at a specified ephemeris; on-target and comparison stars | Requires ephemeris-driven retrieval ("give me all points within ±2 h of predicted transit") — a query shape absent from the workload document | ⚠️ |
| S8.5 | Occurrence rates | Injection–recovery of transit signals per star | The selection function again, in its hardest form | ❌ |

---

### S9 — Cataclysmic variables, novae, and accreting systems

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S9.1 | Find dwarf-nova outbursts in real time | Rapid brightening vs a well-defined quiescent baseline | Needs per-object baseline statistics available at alert time | ⚠️ |
| S9.2 | Outburst recurrence times and duty cycles | Full outburst history with non-detections between outbursts | Duty cycle is a coverage-weighted quantity — C1/C2 | ❌ |
| S9.3 | Galactic nova discovery and early light curves | Fast alerting in crowded fields | Crowding: difference imaging in the plane has a very different false-positive rate; the density term must enter the threshold | ⚠️ |
| S9.4 | Eclipsing/superhumping CVs, period bouncers | Minute-scale photometry | C3 | ❌ |
| S9.5 | Are there UV-bright accreting sources with no optical counterpart? | Cross-mission negative-match search | Simultaneity + cross-matching two different selection functions | ❌ |

---

### S10 — Solar system: asteroids, NEOs and comets

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S10.1 | Give me all our observations of asteroid X | Light curve by designation, with observing geometry | The `visit_asteroids` table is sorted spatially; a *moving* object's natural key is its name (raised in the technical doc, R2) | ⚠️ |
| S10.2 | Rotation period and shape from the light curve | Photometry corrected to unit heliocentric and geocentric distance, at known phase angle | **r, Δ, phase angle, and light-time are not stored and cannot be reconstructed without an ephemeris service.** This blocks essentially all asteroid photometry science | ❌ |
| S10.3 | Phase curves, absolute magnitude H and slope G, albedo proxies | Same geometry, over a range of phase angles | Same | ❌ |
| S10.4 | Do family members share photometric properties? | Join by orbital elements (a, e, i) / family membership → external orbit catalogue | Requires an MPC/JPL orbital-element table and joins by *dynamical* properties, a query shape entirely absent | ❌ |
| S10.5 | Discover unknown movers and submit tracklets to the MPC | Per-night unmatched detections, linked into tracklets, with astrometric residuals and MPC-format output | Needs the orphan-detection product and a linking stage; also needs per-frame data for fast movers (C3) | ⚠️ |
| S10.6 | NEO astrometry for orbit refinement | Sub-100 mas astrometry with correct timing and observatory code | Timing precision and trailing (C3, C6) | ⚠️ |
| S10.7 | Active asteroids and comet activity | Extendedness measured against the PSF, at known geometry | `x2/y2/xy` exist but there is no PSF-relative extendedness metric or coma-profile measure | ⚠️ |
| S10.8 | Did we serendipitously observe object X at time T? (pre-covery) | Coverage lookup along a *predicted ephemeris track*, not a fixed position | A moving-target coverage query — not a cone search; nothing in the design supports it | ❌ |

**Panel note.** S10.2/S10.3/S10.8 are cheap to fix and expensive to retrofit. Three Float32
columns (r, Δ, phase angle) computed at ingest, plus a moving-target coverage query, unlock an
entire science programme. This is the highest science-per-byte item in the review.

---

### S11 — Occultations and the sub-second sky

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S11.1 | Detect a KBO occultation of a background star | Per-frame (or faster) photometry of many stars simultaneously, with precise timing | Requires a fundamentally different data product; a stated LAST goal that the current archive cannot support at all | ❌ |
| S11.2 | What is the size distribution of small KBOs? | Event rate ÷ (star-hours monitored at the required precision and cadence) | Denominator = star-hours, per star, per timescale | ❌ |
| S11.3 | Asteroid occultations of catalogued stars (size, shape, binarity) | Predicted event lookup + high-cadence photometry at the predicted time | Prediction-driven scheduling and retrieval; needs the ephemeris service | ❌ |
| S11.4 | Diffraction-regime (Fresnel) signatures | Sub-second sampling and a well-characterised noise model | Beyond the archive as designed | ❌ |

**Panel position.** Either occultation science is in scope — in which case a per-frame,
high-cadence data product must be designed now, for at least a subset of fields and stars — or it
is out of scope and should be stated as such publicly, so the community does not assume otherwise.

---

### S12 — Astrometry, Galactic structure and moving stars

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S12.1 | Proper motions from multi-year astrometry | Per-epoch positions with error model, matched consistently for high-PM objects | Circular problem: matching needs PM, PM needs matching (C4) | ⚠️ |
| S12.2 | Find high-PM / nearby objects missed by Gaia | Objects whose position drifts systematically | Requires storing the *unmatched* and *ambiguously matched* detections, which are currently discarded | ❌ |
| S12.3 | Astrometric microlensing / photometric microlensing events | Long, well-sampled light curves + astrometric time series | Blending; the event is often in the blend | ⚠️ |
| S12.4 | Systematic astrometric residuals vs field, detector, colour | Per-detection astrometric residual against the reference catalogue | Residuals are not stored per source (only per-image `ast_arms`); the science value is in the per-source residual | ⚠️ |

---

### S13 — Artificial objects
*Both a contaminant and a genuine product; LAST's cadence makes it unavoidable.*

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S13.1 | Which detections are satellite/debris streaks? | Streak detection and a labelled class, per detection | Streaks fragment into multiple point-like detections and pollute the transient stream | ⚠️ |
| S13.2 | Satellite brightness statistics vs constellation and elevation | Streak photometry + orbital identification (TLE join) | Needs an external TLE/ephemeris join at the epoch — a moving-target crossmatch (S10.8) | ❌ |
| S13.3 | How much survey time is lost to streaks? | Per-image affected-area fraction | Feeds the effective-area term of the selection function (C1) | ❌ |

---

### S14 — Survey characterisation as science
*Not "ops". These are numbers that appear in the methods section of every paper from the survey.*

| # | Question | Must return | |
|---|---|---|---|
| S14.1 | What is the realised cadence distribution per field, per year? | Coverage product with per-field, per-night sampling | ❌ |
| S14.2 | What is the effective survey area above depth m, integrated over time? | Coverage × depth(SED) integral | ❌ |
| S14.3 | What is the photometric repeatability vs magnitude, per telescope, per night? | RMS-vs-mag from a stable comparison sample | ⚠️ |
| S14.4 | What is the detection efficiency vs magnitude, crowding and PSF quality? | **Injection–recovery results, stored** | ❌ |
| S14.5 | How does depth degrade with moon phase, airmass, seeing? | Per-image conditions joined to depth | ✅ |
| S14.6 | Are there systematic zero-point offsets between the 48 telescopes, and do they drift? | Standard-star ensemble monitoring per telescope over time | ⚠️ |
| S14.7 | Did a re-reduction change my published photometry? | Data-release versioning and per-DR comparison | ❌ |

---

### S15 — Cross-mission UV–optical joint science
*The reason to build one portal rather than two.*

| # | Question | Must return | Trap | |
|---|---|---|---|---|
| S15.1 | Which LAST exposures are simultaneous (± minutes) with an ULTRASAT exposure of the same sky? | A **simultaneity index**: (sky cell, time window) → both missions' exposures | Neither a spatial nor a temporal query alone; it is a temporal *join across two survey footprints* | ❌ |
| S15.2 | UV–optical colour temperature evolution over hours for a SN or kilonova | Cross-calibrated, epoch-matched photometry with matched integration windows | Different exposure times (400 s vs 300 s) and different SED-dependent zero points | ❌ |
| S15.3 | UV-bright but optically-faint transients (and vice versa) | Negative cross-matching, with each mission's selection function | Two different selection functions must both be known to interpret a non-match | ❌ |
| S15.4 | Ground-based optical constraints on an ULTRASAT alert within minutes | Real-time coverage lookup on the LAST archive from the ULTRASAT alert stream | Latency and the coverage product | ⚠️ |
| S15.5 | Combined UV+optical variability catalogue of stars | Object identity shared across missions | Requires a common object namespace, not just a common HEALPix convention | ❌ |

**Panel position.** A shared **object namespace and coordinate/time convention across both
missions** is far more important than shared database technology, and it is much harder to
retrofit. If one decision from this document is made this quarter, make it this one.

---

### S16 — Serendipity and the unknown

| # | Question | Must return | |
|---|---|---|---|
| S16.1 | What is in our data that we have no name for? | Outlier detection in a feature space of light-curve statistics, over the whole survey | ❌ |
| S16.2 | Show me everything that violates its own history by > 5σ, tonight | Per-object baseline + fast comparison of new detections against it | ⚠️ |
| S16.3 | Re-analyse the archive for a phenomenon nobody was looking for in 2026 | Preserved raw/processed pixels, immutable coverage, versioned reductions | ⚠️ |
| S16.4 | Reproduce a 2027 result in 2032 | Frozen DR + sample registry + selection function of that DR | ❌ |

**Panel note.** S16.3–S16.4 are the reason archives outlive the instruments that fill them. Most
of the highest-impact results from historical surveys came from questions the builders did not
anticipate. The design decisions that preserve that possibility — pixel retention, immutable
coverage, versioned reductions, stored selection functions — are all being made **now**.

---

## 3. The scientific data products the archive is missing

§2 has ~85 questions and roughly 40 of them fail for one of ten reasons. Those ten reasons are
data products, not indexes. Ordered by scientific impact.

### P1 — The selection function (detection efficiency)
**What it is:** ε(magnitude, SED/colour, position, epoch, local stellar density, PSF quality,
angular speed, variability timescale) — the probability that a source with those properties
appears in the catalogue.
**How you get it:** source injection into real images, run through the real pipeline, results
stored per image family (e.g. per telescope per night per field), fitted to a compact parametric
form so it is queryable rather than a pile of test outputs.
**Blocks:** S1.5, S2.1, S2.3, S6.1, S7.5, S7.6, S8.5, S10.5, S11.2, S14.4 — every rate, every
occurrence fraction, every luminosity function.
**Cost of delay:** injection runs need the images. Retrofitting after images are archived to cold
storage is 10–100× more expensive, and after a re-reduction the old efficiency curves are invalid.

### P2 — The coverage record (where, when, how deep — including where nothing was found)
**What it is:** an immutable, versioned map from (sky cell, time) → exposures, with depth as a
function of SED, plus the effective area actually usable (masked, streaked, saturated regions
removed).
**Blocks:** S1.1, S1.5, S3.1–S3.4, S4.3, S9.2, S10.8, S13.3, S14.1, S14.2 — and every upper limit
in the archive.
**Note:** must be **immutable and versioned**, because GW skymaps and orbit solutions are revised
after the fact and the question "what did we cover" must give the same answer in 2032 as in 2027.

### P3 — Forced photometry as a service
**What it is:** "measure the flux at this position (or along this ephemeris track) in every image
that covers it, whether or not anything was detected", returning fluxes and limits.
**Blocks:** S1.3, S1.6, S2.4, S5.1, S9.2 — all pre-explosion and pre-outburst history.
**Note:** this is not a query; it reads pixels. It determines your image-retention policy. The
`is_forced` flag exists, which suggests forced photometry is already produced *at pipeline-chosen
positions* — the science need is forced photometry at **user-chosen** positions, retrospectively.

### P4 — A short-timescale data product (per-frame photometry)
**What it is:** photometry from individual 20 s frames, at least for a curated target list, a
rolling recent window, and anything flagged variable at visit level.
**Blocks:** S4.1, S4.6, S6.3, S7.3, S7.4, S8.2, S9.4, S11.1–S11.4 — i.e. a large fraction of what
makes LAST distinctive rather than a smaller ZTF.
**Panel disagreement, stated honestly:** the transient/SN reviewers regard the visit coadd as
correct and per-frame data as a luxury; the stellar, WD and occultation reviewers regard the
absence of per-frame data as disqualifying for their science. Both are right. The resolution is
policy-based selectivity, not a global answer — but the policy must be written now.

### P5 — Relative (ensemble) photometry
**What it is:** per-epoch differential magnitudes against a defined comparison ensemble, with the
ensemble definition and the per-image correction stored.
**Blocks:** S5.2, S6.6, S7.3, S7.5, S8.1–S8.3 — everything below ~10 mmag.
**Also fixes:** the archive's worst access pattern (every mmag user downloading whole fields).

### P6 — An SED-dependent depth model
**What it is:** limiting magnitude for a small basis set of spectra (hot blackbody, solar, M dwarf,
SN Ia at peak, SN II early), or the throughput and noise terms needed to compute it for any SED.
**Blocks:** correct interpretation of every limit and every rate; acute for ULTRASAT, where depth
varies by ~2 mag with source temperature, and for LAST's colour-dependent unfiltered band.

### P7 — Object identity as a physical model
**What it is:** matching against proper-motion-propagated positions; identity-quality flags
(clean / high-PM / blended / crowded / ambiguous); retained alternative matches; a shared
namespace covering static sources, difference-only transients and (separately) moving objects.
**Blocks:** S5.4, S7.7, S12.1, S12.2, S15.5 — and quietly corrupts light curves of nearby stars,
which are the most valuable targets in the survey.

### P8 — Solar system observing geometry
**What it is:** heliocentric and geocentric distance, phase angle, light-time correction and
apparent rate of motion, per detection; plus an ephemeris service supporting moving-target
coverage queries and orbital-element joins.
**Blocks:** S10.2–S10.8, S11.3, S13.2. Cheapest large win in this review.

### P9 — The sampling/window function
**What it is:** per-object and per-field observing-time series (including planned-but-lost time),
queryable, so periods, structure functions and duty cycles are interpretable.
**Blocks:** S5.3, S7.1, S7.5, S9.2, S14.1.

### P10 — Sample registry and release versioning
**What it is:** named, frozen, citable sample definitions bound to a data-release version, each
carrying its selection function; plus the ability to compare a result across releases.
**Blocks:** S14.7, S16.4 — and, in practice, the referee's second question on every paper.

---

## 4. Ten questions this archive cannot answer today

Stated bluntly, because this is the useful form of the review.

1. **"When did this supernova explode?"** — needs true, SED-dependent depths on the pre-explosion
   non-detections (P2, P6).
2. **"What is the rate of X per unit volume per year?"** — any X. No denominator (P1, P2).
3. **"What fraction of M dwarfs flare above energy E?"** — no monitored-sample product (P1, P9).
4. **"What did we cover of this GW localisation, and what does our non-detection exclude?"** —
   no immutable coverage, no efficiency (P2, P1, P6).
5. **"Was there anything at this position six months before the outburst?"** — no retrospective
   forced photometry (P3).
6. **"What is this asteroid's rotation period and phase curve?"** — no observing geometry (P8).
7. **"Did we serendipitously observe this comet / this satellite / this NEO on that date?"** —
   no moving-target coverage query (P2, P8).
8. **"Show me the minute-scale light curve of this flare / pulsating WD / occultation."** — the
   data product does not exist (P4).
9. **"Give me millimagnitude photometry of this bright star."** — only absolutely calibrated
   magnitudes with a ~10 mmag floor are stored (P5).
10. **"What were the UV and optical fluxes at the same moment?"** — no cross-mission simultaneity
    index or shared object namespace (S15).

Note what these have in common: **none is fixed by indexing.** Every one is fixed by deciding to
compute and store a scientific product. The technical document optimises the retrieval of things
you have; this list is about things you don't.

---

## 5. Decisions only the science team can make

These are the questions I could not answer from the schema, phrased so they can be forwarded
directly to the LAST/ULTRASAT science leads. They are ordered by how expensive they get if
deferred.

**Now-or-never (the answer changes what must be captured while data is being taken):**

1. **Is per-frame (20 s) photometry in scope at all?** If yes: for which targets, which fields,
   what retention? If no, say so publicly. (Blocks all of §S11 and much of §S6–S9.)
2. **Will source-injection efficiency runs be performed, and at what granularity?** (Per image?
   Per night per telescope? Per field?) Without a decision here, no rate measurement from this
   survey is defensible.
3. **How long are processed images retained on retrievable storage?** This decides whether
   retrospective forced photometry (P3) is ever possible.
4. **What is the survey's official statement on data-release cadence and immutability?** Frozen
   DRs, or a continuously-updated archive, or both?

**Structural (cheap now, painful later):**

5. **Object identity:** what matching radius, against which reference catalogue, at which epoch,
   with proper motion propagated or not? Should ambiguous matches be retained?
6. **Are difference-only transients given identities in the same namespace as static sources?**
7. **Will LAST and ULTRASAT share an object namespace and coordinate/time conventions?**
   (Sharing a HEALPix scheme is necessary but not sufficient.)
8. **Which set of reference SEDs should define the depth model** (P6)? Three to six spectra,
   chosen by the science WGs, would settle it.
9. **Which minimal per-detection solar-system geometry columns are wanted** (r, Δ, phase angle,
   apparent rate, light-time)? And do you want joins by orbital elements / family?
10. **What are the flag bit definitions**, and which are the "never trust this measurement" bits
    that the portal should apply by default? A default quality cut is a scientific statement and
    should be blessed by the science team, not chosen by the portal developers.

**Programmatic:**

11. **What is the required alert latency for LAST** (ULTRASAT's < 15 min is fixed)? And what
    false-positive rate is acceptable to the follow-up community?
12. **Which external catalogues must be resident** in the archive rather than fetched
    (Gaia, MPC orbits, TNS, GALEX, host-galaxy catalogue with redshifts, TESS/exoplanet hosts,
    variable-star catalogues, TLE feeds)?
13. **Who owns classification labels**, and are human vetting decisions part of the archive?
14. **Is polarimetry (LAST-P) in scope for the portal?** It adds a data dimension, not just
    columns.
15. **Which three science cases should the portal be benchmarked against at acceptance?** The
    panel's suggestion: (a) full pre-explosion history plus limits for a SN within 60 s;
    (b) probability-weighted coverage of a GW skymap within 5 min of the alert; (c) a
    millimagnitude light curve of a B_P = 13 star over three years. If those three work, the rest
    follows.

---

## 6. What this means for the technical design

Briefly, because the previous document covers the mechanics. The science review does not
invalidate its recommendations — the spatial primary key, the object catalogue, the time-ordered
transient working set and the coverage/footprint table remain correct — but it **re-orders the
priorities and adds items that no amount of indexing produces**.

| Science product (§3) | Nature of the work | Technical doc coverage |
|---|---|---|
| P1 Selection function | Pipeline + injection campaign; new table | Absent |
| P2 Coverage record | New product; must be immutable + versioned | Partly (`image_moc`, coverage MV) — but not immutable, not SED-dependent |
| P3 Forced photometry service | Compute service over pixels; storage policy | Absent |
| P4 Per-frame photometry | New data product + retention policy | Raised only as an open question |
| P5 Relative photometry | Pipeline output + two columns + small table | Absent |
| P6 SED-dependent depth | A few columns per image | Absent (`limmag` treated as scalar) |
| P7 Object identity model | Matching algorithm + quality flags | Treated as an id-encoding problem only |
| P8 Solar system geometry | Three to five columns at ingest + ephemeris service | Raised (E3) but under-weighted |
| P9 Window function | Derived from coverage | Absent |
| P10 Sample registry | Small service; large scientific value | Absent |

**Revised order of work.** The technical document's Phase 0 (object-identifier design, HEALPix
convention, version freeze) stands — those are genuinely now-or-never. But **P1, P2 and P4 belong
in Phase 0 as well**, because they depend on data and images that exist only while the survey is
running. Query performance can be fixed in any quarter. A missing selection function cannot.

---

## 7. Summary

The workload document answered *"how do we serve these rows quickly?"* well. The scientific
question is *"can a referee believe the number that comes out?"*, and for a large fraction of the
survey's headline science the honest answer today is no — not because the queries are slow, but
because the denominators, the limits, the short-timescale data and the observing geometry were
never stored.

Three sentences, if that is all that gets read:

1. **Store the denominator.** Coverage, depth as a function of SED, and injection-measured
   efficiency are the archive's most important products, and none of them exists.
2. **Decide about 20 s data now.** The visit coadd quietly deletes occultations, fast flashes,
   WD pulsations and fast-mover photometry — several of the reasons LAST was built.
3. **Object identity, observing geometry and a shared LAST/ULTRASAT namespace are cheap this year
   and very expensive in three.**

Everything else in this review is elaboration on those three.

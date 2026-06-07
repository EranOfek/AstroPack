# LCS CP-SAT Model

Formal description of the ULTRASAT Low Cadence Survey CP-SAT scheduling model.

## Inputs

| File | Description |
|------|-------------|
| `lcs_fields.csv` | Field catalog: `field_id`, `ra`, `dec`, `A_U` |
| `lcs_visibility_windows.csv` | Continuous visibility intervals per field |
| `lcs_field_eligibility.csv` | Hard eligibility flags per field |
| `lcs_params.json` | Scalar campaign parameters |
| `lcs_daily_visibility.csv` | Optional day×field visibility for slot ordering |

Visibility is precomputed in MATLAB; Python does not reimplement astronomy.

## Time model

- Visibility horizon: `first_day` .. `last_day` (default 1..420), used by MATLAB inputs.
- **LCS plan output:** days 1..`capacity_last_day` (default 360 = 8 × 45-day windows).
- Daily capacity is enforced only on plan days 1..360.
- Eight fixed 45-day windows: W1=[1,45], W2=[46,90], …, W8=[316,360].
- Six 135-day spans: W1–W3, W2–W4, …, W6–W8.

A field is feasible for a 45-day window W if some visibility row satisfies
`vis_start_day <= W.start` and `vis_end_day >= W.end`.

A field is feasible for a 135-day span starting at window k if it can cover
days `[Wk.start, W(k+2).end]`.

## Sets

| Set | Count | Pattern |
|-----|-------|---------|
| A | 48 | One 45-day window, daily cadence |
| B | 16 | Three 45-day windows: one daily + two sparse (every 4 days) |
| C | 16 | One 135-day window, sparse cadence (every 4 days) |
| D | 4 | One 45-day window, daily cadence, high extinction |

## Decision variables

- `a[f, w]` — field f assigned to Set A in 45-day window w
- `b_sel[f]` — field f selected for Set B
- `b_daily[f, w]`, `b_sparse[f, w]` — B field daily/sparse block in window w
- `c[f, w135]` — field f assigned to Set C in 135-day span starting at w135
- `d[f, w]` — field f assigned to Set D in 45-day window w

Variables are only created for feasible (field, window) pairs.

## Constraints

1. Each field used in at most one set (A/B/C/D).
2. Exactly 48 A, 16 B, 16 C, 4 D assignments.
3. A: eligible (`A_U <= max_extinction`, `eligible_abc`) and visible for window.
4. B: eligible and visible; if selected, exactly one daily and two sparse windows, all distinct.
5. C: eligible (`eligible_abc`, `eligible_long_window`) and visible for 135-day span.
6. D: eligible (`eligible_d`), in ranked list, visible for window.
7. Daily capacity: on each day 1..360, scheduled observations ≤ 11.
   - A, D, B-daily: every day in block.
   - B-sparse, C: every 4th day with configurable phase.
8. Optional: B's three windows must be consecutive (`require_b_consecutive`).

## Objective

Maximize weighted sum (feasibility first, then quality):

- High weight: prefer higher-ranked D fields
- Medium weight: visibility slack beyond required window
- Low weight: lower extinction for A/B/C

## Outputs

- `schedule_windows.csv` — per-field window assignments
- `daily_schedule.csv` — day × slot observations
- `validation_report.csv` — constraint checks
- `solver_summary.json` — status, timing, counts

## Known assumptions

- Fixed 45-day window grid (not shiftable in v1).
- Sparse cadence phase is configurable, default 0.
- Slot order within a day is arbitrary in v1.
- Solver selects A/B/C/D categories automatically from eligibility.
- Daily capacity uses precomputed visibility only for slot ordering, not feasibility.

## Open questions

1. Should B's three 45-day blocks be required to be consecutive, or any three visible windows?
   - **Current:** `require_b_consecutive=False`; any three distinct windows.
2. Should sparse cadence phase be fixed by window index, field, or optimized?
   - **Current:** fixed global `sparse_phase` (default 0).
3. Should Set A windows be fixed or allowed to shift?
   - **Current:** selectable from all feasible 45-day windows.
4. Should Set C 135-day windows be fixed or selectable from all consecutive triples?
   - **Current:** selectable from W1–W3 .. W6–W8.
5. Should daily slot order use per-slot visibility in the first version?
   - **Current:** no; arbitrary slot order.
6. Should the solver select A/B/C categories automatically or use precomputed lists?
   - **Current:** automatic selection from eligibility flags.

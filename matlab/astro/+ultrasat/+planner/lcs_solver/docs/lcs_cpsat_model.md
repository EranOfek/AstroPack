# LCS CP-SAT Model

The current solver model is documented in:

```text
docs/lcs_solver_v3_alignment.md
```

That document describes the fixed CP-SAT implementation and its relationship to
`LcsHelper_v3.m`, including:

- MATLAB input export
- 45-day and 135-day window geometry
- Sets A/B/C/D
- v3 Set B division table
- moved Set A rows (`group >= 7`)
- v3 window-index capacity
- Set D placement and Set A bumping
- cadence expansion
- validation and expected outputs

This file is kept as a compatibility pointer because earlier notes used this
filename.


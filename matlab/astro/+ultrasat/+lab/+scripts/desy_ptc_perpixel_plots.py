"""Single-pixel version of the (Var - RN^2)/(Mean*Gain) panels.

Each of the 10 000 pixels of the DESY region has its own mean M_i(step), its own
temporal variance V_i(step) (3 frames -> 2 dof, exponential distribution) and its
own read noise RN_i (5 ZE frames). The panels show the per-pixel ratio
R_i = (V_i - RN_i^2)/(M_i*Gain), combined over pixels by the median / ln2
(unbiased for 2 dof), with the 16-84 % pixel-to-pixel band.
"""
import json, numpy as np, matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
P = '/home/sasha/claude/ptc_gain_check/perpixel/'
META = json.load(open(P+'meta.json'));  META = [META] if isinstance(META, dict) else META
LN2 = np.log(2)

def load(tag, kind, field, ny, nx, ns):
    a = np.fromfile(P+'%s_%s_%s.bin' % (tag, kind, field), dtype=np.float32)
    return a.reshape((ny, nx, ns), order='F').reshape(ny*nx, ns)

gains = np.arange(1.02, 1.105, 0.01); cols = plt.cm.viridis(np.linspace(0, 1, len(gains)))
fig, axes = plt.subplots(2, len(META), figsize=(6.2*len(META), 9.6), sharey=True)
summary = []
for j, M in enumerate(META):
    ny, nx = [int(v) for v in M['Size']]
    rn2 = np.fromfile(P+M['Tag']+'_zeronoise.bin', dtype=np.float32).astype(float).ravel()**2
    for i, kind in enumerate(['bright', 'dark']):
        Mean = load(M['Tag'], kind, 'mean', ny, nx, len(np.atleast_1d(M['BrightX' if kind=='bright' else 'DarkX'])))
        Var  = load(M['Tag'], kind, 'vartemporal', ny, nx, Mean.shape[1])
        med  = np.median(Mean, axis=0)
        ok   = (med > 20) & (med < 13000)
        R    = (Var[:, ok] - rn2[:, None]) / Mean[:, ok]          # per pixel, per step, gain = 1
        typ = np.median(R, axis=0)/LN2                              # typical single pixel
        lo   = np.percentile(R, 16, axis=0)/LN2; hi = np.percentile(R, 84, axis=0)/LN2
        mmed = med[ok]
        ax = axes[i, j]
        ax.fill_between(mmed, lo/1.06, hi/1.06, color='0.85', zorder=0,
                        label='16-84 % of single pixels (gain 1.06)')
        for g, c in zip(gains, cols):
            ax.plot(mmed, typ/g, 'o-', ms=4, lw=1, color=c, label='gain %.2f' % g)
        ax.axhline(1, color='k', lw=.8, ls='--')
        ax.set_xscale('log'); ax.grid(alpha=.4, which='both'); ax.set_ylim(0.0, 2.2)
        ax.set_xlabel('mean signal of the pixel [ADU]')
        if j == 0: ax.set_ylabel('(Var - RN$^2$) / (Mean x Gain),  per pixel')
        ax.set_title('%s run %s - %s ladder, single pixels (median of 10 000)' % (M['Die'], M['Run'], 'light' if kind=='bright' else 'dark'), fontsize=9.5)
        if i == 0 and j == 0: ax.legend(fontsize=7, ncol=2, loc='upper right')
        # pixel-to-pixel scatter of the ratio averaged over the 1000-2500 ADU steps
        w = (mmed > 1000) & (mmed < 2600)
        if w.sum() >= 2:
            Rw = R[:, w].mean(axis=1)
            summary.append((M['Die'], M['Run'], kind, w.sum(), np.median(Rw)/1.0, Rw.std()/np.median(Rw), 1/np.sqrt(w.sum())))
fig.suptitle('Single-pixel (Var - RN$^2$)/(Mean x Gain), Gain = 1.02 ... 1.10 — DESY region pixels', y=.995)
fig.tight_layout(); fig.savefig('/home/sasha/claude/ptc_gain_check/perpixel_panels.png', dpi=100)
print('die run ladder  Nsteps(1-2.5k)  median ratio  observed rel.scatter  expected from 2-dof noise')
for s in summary: print('%-8s %-3s %-6s %2d  %.3f  %.3f  %.3f' % s)

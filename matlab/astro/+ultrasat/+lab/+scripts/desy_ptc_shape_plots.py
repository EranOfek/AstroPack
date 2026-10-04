"""(Var - RN^2)/(Mean*Gain) vs mean for Gain = 1.02...1.10, light and dark ladders, DESY region.
Variance per step = median over the 10 000 pixels of the per-pixel temporal variance / ln2
(unbiased for 3 frames, robust against particle hits); RN^2 from the ZE frames the same way."""
import json, numpy as np, matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
D = json.load(open('/home/sasha/claude/ptc_gain_check/dark_light2.json'))
if isinstance(D, dict): D = [D]
gains = np.arange(1.02, 1.105, 0.01)
cols  = plt.cm.viridis(np.linspace(0, 1, len(gains)))
relse = 1/(np.log(2)*np.sqrt(10000))                      # ~1.4 % per point
fig, axes = plt.subplots(2, len(D), figsize=(6.2*len(D), 9.6), sharey=True)
for j, S in enumerate(D):
    rn2 = S['RN2med']
    for i, kind in enumerate(['Bright', 'Dark']):
        L = S[kind]; m = np.array(L['Mean'], float); v = np.array(L['VarMed'], float)
        nf = int(np.atleast_1d(L['Nframes'])[0])
        ok = (m > 20) & (v > 0) & (m < 13000)
        m, v = m[ok], v[ok]; se = v*relse
        ax = axes[i, j]
        for g, c in zip(gains, cols):
            ax.errorbar(m, (v-rn2)/(m*g), yerr=se/(m*g), fmt='o-', ms=4, lw=1, capsize=1.5,
                        color=c, label='gain %.2f' % g)
        ax.axhline(1, color='k', lw=.8, ls='--')
        ax.set_xscale('log'); ax.grid(alpha=.4, which='both'); ax.set_ylim(0.75, 1.40)
        ax.set_xlabel('mean signal [ADU]')
        if j == 0: ax.set_ylabel('(Var - RN$^2$) / (Mean x Gain)')
        ax.set_title('%s run %s - %s ladder   RN$^2$ = %.1f ADU$^2$ (RN = %.2f ADU), %d frames/step'
                     % (S['Die'], S['Run'], 'light' if kind == 'Bright' else 'dark', rn2, np.sqrt(rn2), nf), fontsize=9.5)
        if i == 0 and j == 0: ax.legend(fontsize=7.5, ncol=3, loc='lower left')
fig.suptitle('(Var - RN$^2$) / (Mean x Gain), Gain = 1.02 ... 1.10 — DESY 100x100 region, robust per-step variance', y=.995)
fig.tight_layout(); fig.savefig('/home/sasha/claude/ptc_gain_check/rn_corrected_panels.png', dpi=100)
for S in D:
    for kind in ['Bright','Dark']:
        L=S[kind]; m=np.array(L['Mean'],float); v=np.array(L['VarMed'],float)
        ok=(m>20)&(m<13000); m,v=m[ok],v[ok]; r=(v-S['RN2med'])/m
        print('%-4s %-8s %-6s  ratio at lowest step (%.0f ADU) %.3f ; at 1-2.5k %.3f-%.3f ; min %.3f @ %.0f ; >6k %.3f-%.3f' % (
            S['Run'],S['Die'],kind,m[0],r[0],
            r[(m>1000)&(m<2600)].min() if ((m>1000)&(m<2600)).any() else np.nan,
            r[(m>1000)&(m<2600)].max() if ((m>1000)&(m<2600)).any() else np.nan,
            r.min(), m[r.argmin()], r[m>6000].min() if (m>6000).any() else np.nan, r[m>6000].max() if (m>6000).any() else np.nan))

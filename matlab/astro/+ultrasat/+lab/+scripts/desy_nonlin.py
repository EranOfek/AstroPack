"""The low-count non-linearity: the model, and the fit of it to a measured ladder.

   One place for both, because three things quote the same number and they must
   not drift apart: the per-unit figure, the per-unit report, and the campaign
   note. The per-unit plotting stage fits each ladder and writes nonlin_fit.json
   beside the other dumps; everything else reads that file.

       Q_out = Q * exp(-D / Q)          M = g * Q_out
       Var   = (dM/dQ)^2 * Q = g^2 * (dQ_out/dQ)^2 * Q

   D is a charge deficit in electrons. At large Q the response is g*(Q - D), the
   straight line whose intercept the four routes report; below Q ~ D it rolls off
   smoothly to zero. Because the variance is the derivative squared times the
   charge, ONE parameter fixes both the intercept and the curvature of the
   photon-transfer curve, which a straight line treats as independent.
"""
import numpy as np
from scipy.optimize import brentq, least_squares


def qout(Q, D):
    """read-out charge for a collected charge Q [e-]"""
    return Q*np.exp(-D/Q)


def dqout(Q, D):
    """dQ_out/dQ, the local slope of the response"""
    return np.exp(-D/Q)*(1.0 + D/Q)


def collected(M, g, D):
    """invert the response: the charge that reads out as M ADU"""
    M = np.atleast_1d(np.asarray(M, dtype=float))
    out = np.empty_like(M)
    for i, m in enumerate(M):
        if not np.isfinite(m) or m <= 0:
            out[i] = np.nan
            continue
        hi = max(10.0, 10*(m/g + D))
        while g*qout(hi, D) < m:
            hi *= 2
        out[i] = brentq(lambda q: g*qout(q, D) - m, 1e-9, hi, xtol=1e-12)
    return out


def variance(M, g, D):
    """the photon-transfer curve the model predicts at a measured signal M [ADU]"""
    Q = collected(M, g, D)
    return (g*dqout(Q, D))**2 * Q


def fit(M, V, g0, lo=3.0, hi=1000.0, vlo=10.0, vhi=200.0):
    """Fit (g, D) to one ladder's photon-transfer points.

    M, V : mean signal [ADU] and Var - RN^2 [ADU^2] of each step
    g0   : starting gain, normally the route's own
    lo, hi   : the signal range fitted. The whole ladder, so that the top
               constrains the gain and the bottom the deficit.
    vlo, vhi : a validation range quoted separately, not used by the fit.
    Returns a dict, or None when the ladder has too few usable points.
    """
    M = np.atleast_1d(np.asarray(M, dtype=float))
    V = np.atleast_1d(np.asarray(V, dtype=float))
    k = np.isfinite(M) & np.isfinite(V) & (M >= lo) & (M <= hi) & (V > 0)
    if k.sum() < 6:
        return None
    m, v = M[k], V[k]
    try:
        r = least_squares(lambda p: (variance(m, p[0], abs(p[1])) - v)/np.sqrt(np.maximum(v, 1.0)),
                          [float(g0), 5.0], method='lm', max_nfev=5000)
    except Exception:
        return None
    g, D = float(r.x[0]), float(abs(r.x[1]))
    if not (np.isfinite(g) and np.isfinite(D)) or g <= 0:
        return None
    pv = variance(m, g, D)
    kv = (m >= vlo) & (m <= vhi)
    return dict(g=g, D=D, n=int(k.sum()),
                Lo=float(m.min()), Hi=float(m.max()),
                Rms=float(np.sqrt(np.mean(((pv - v)/v)**2))*100),
                Worst=float(np.max(np.abs(pv - v)/v)*100),
                RmsValid=(float(np.sqrt(np.mean(((pv[kv] - v[kv])/v[kv])**2))*100)
                          if kv.any() else None),
                ValidLo=float(vlo), ValidHi=float(vhi), NValid=int(kv.sum()))

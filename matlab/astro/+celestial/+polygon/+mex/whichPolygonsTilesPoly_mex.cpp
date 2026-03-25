// mex -O CXXFLAGS='$CXXFLAGS -O3 -DNDEBUG' isPolygonsTilesPolygon_mex.cpp
#include "mex.h"
#include <vector>
#include <cmath>
#include <algorithm>
#include <limits>
#include <cstdint>
#include <numeric>

using std::vector;
using std::size_t;

static const double EPS_DIST = 1e-12;
static const double EPS_AREA = 1e-14;
static const double EPS_UNIQ = 1e-12;

struct Vec2 {
    double x, y;
};

struct Vec3 {
    double x, y, z;
};

struct Line {
    // a*x + b*y + c = 0, normalized so sqrt(a^2+b^2)=1
    double a, b, c;
};

struct Poly {
    vector<Vec2> v;
};

struct BitsetDyn {
    vector<uint64_t> w;

    BitsetDyn() {}
    BitsetDyn(size_t nbits) { w.assign((nbits + 63) >> 6, 0ULL); }

    inline void set(size_t i) { w[i >> 6] |= (1ULL << (i & 63)); }
    inline bool test(size_t i) const { return (w[i >> 6] >> (i & 63)) & 1ULL; }

    inline bool any() const {
        for (uint64_t x : w) if (x) return true;
        return false;
    }

    inline size_t count() const {
        size_t c = 0;
        for (uint64_t x : w) {
#if defined(_MSC_VER)
            c += __popcnt64(x);
#else
            c += (size_t)__builtin_popcountll(x);
#endif
        }
        return c;
    }

    inline int firstSetBit() const {
        for (size_t i = 0; i < w.size(); ++i) {
            uint64_t x = w[i];
            if (x) {
#if defined(_MSC_VER)
                unsigned long idx;
                _BitScanForward64(&idx, x);
                return (int)(i * 64 + idx);
#else
                return (int)(i * 64 + __builtin_ctzll(x));
#endif
            }
        }
        return -1;
    }
};

static inline double dot2(const Vec2& A, const Vec2& B) {
    return A.x * B.x + A.y * B.y;
}

static inline double cross2(const Vec2& A, const Vec2& B) {
    return A.x * B.y - A.y * B.x;
}

static inline Vec2 sub2(const Vec2& A, const Vec2& B) {
    return {A.x - B.x, A.y - B.y};
}

static inline Vec2 add2(const Vec2& A, const Vec2& B) {
    return {A.x + B.x, A.y + B.y};
}

static inline Vec2 mul2(const Vec2& A, double s) {
    return {A.x * s, A.y * s};
}

static inline double dot3(const Vec3& A, const Vec3& B) {
    return A.x * B.x + A.y * B.y + A.z * B.z;
}

static inline Vec3 cross3(const Vec3& A, const Vec3& B) {
    return {
        A.y * B.z - A.z * B.y,
        A.z * B.x - A.x * B.z,
        A.x * B.y - A.y * B.x
    };
}

static inline double norm3(const Vec3& A) {
    return std::sqrt(dot3(A, A));
}

static inline Vec3 normalize3(const Vec3& A) {
    double n = norm3(A);
    return {A.x / n, A.y / n, A.z / n};
}

static inline double signedArea(const Poly& P) {
    const size_t n = P.v.size();
    if (n < 3) return 0.0;
    double A = 0.0;
    for (size_t i = 0; i < n; ++i) {
        const Vec2& p = P.v[i];
        const Vec2& q = P.v[(i + 1) % n];
        A += cross2(p, q);
    }
    return 0.5 * A;
}

static inline double absArea(const Poly& P) {
    return std::abs(signedArea(P));
}

static inline void ensureCCW(Poly& P) {
    if (signedArea(P) < 0.0) {
        std::reverse(P.v.begin(), P.v.end());
    }
}

static inline void removeNearDuplicateVertices(Poly& P) {
    vector<Vec2> out;
    out.reserve(P.v.size());
    for (size_t i = 0; i < P.v.size(); ++i) {
        if (out.empty()) {
            out.push_back(P.v[i]);
        } else {
            Vec2 d = sub2(P.v[i], out.back());
            if (std::abs(d.x) > 1e-14 || std::abs(d.y) > 1e-14) {
                out.push_back(P.v[i]);
            }
        }
    }
    if (out.size() >= 2) {
        Vec2 d = sub2(out.front(), out.back());
        if (std::abs(d.x) <= 1e-14 && std::abs(d.y) <= 1e-14) {
            out.pop_back();
        }
    }
    P.v.swap(out);
}

static inline Vec2 lineSegmentIntersection(const Vec2& S, const Vec2& E, const Line& L) {
    double dS = L.a * S.x + L.b * S.y + L.c;
    double dE = L.a * E.x + L.b * E.y + L.c;
    double t = dS / (dS - dE);
    return {S.x + t * (E.x - S.x), S.y + t * (E.y - S.y)};
}

static Poly clipHalfPlane(const Poly& subject, const Line& L, bool keepPositive) {
    Poly out;
    const size_t n = subject.v.size();
    if (n < 3) return out;

    auto inside = [&](const Vec2& p)->bool {
        double d = L.a * p.x + L.b * p.y + L.c;
        return keepPositive ? (d >= -EPS_DIST) : (d <= EPS_DIST);
    };

    for (size_t i = 0; i < n; ++i) {
        const Vec2& S = subject.v[i];
        const Vec2& E = subject.v[(i + 1) % n];
        bool Sin = inside(S);
        bool Ein = inside(E);

        if (Sin && Ein) {
            out.v.push_back(E);
        } else if (Sin && !Ein) {
            out.v.push_back(lineSegmentIntersection(S, E, L));
        } else if (!Sin && Ein) {
            out.v.push_back(lineSegmentIntersection(S, E, L));
            out.v.push_back(E);
        }
    }

    removeNearDuplicateVertices(out);
    if (out.v.size() < 3 || absArea(out) <= EPS_AREA) {
        out.v.clear();
    } else {
        ensureCCW(out);
    }
    return out;
}

static Poly convexIntersect(const Poly& A, const Poly& B) {
    Poly out = A;
    if (A.v.size() < 3 || B.v.size() < 3) {
        out.v.clear();
        return out;
    }

    Poly BC = B;
    ensureCCW(BC);

    for (size_t i = 0; i < BC.v.size(); ++i) {
        const Vec2& p = BC.v[i];
        const Vec2& q = BC.v[(i + 1) % BC.v.size()];
        Vec2 e = sub2(q, p);

        // left side of edge for CCW clip polygon
        Line L;
        L.a = -e.y;
        L.b =  e.x;
        double nrm = std::sqrt(L.a * L.a + L.b * L.b);
        if (nrm <= 0.0) continue;
        L.a /= nrm;
        L.b /= nrm;
        L.c = -(L.a * p.x + L.b * p.y);

        out = clipHalfPlane(out, L, true);
        if (out.v.size() < 3) break;
    }

    return out;
}

static inline bool pointInConvexPoly(const Poly& P, const Vec2& X) {
    const size_t n = P.v.size();
    if (n < 3) return false;
    for (size_t i = 0; i < n; ++i) {
        Vec2 a = P.v[i];
        Vec2 b = P.v[(i + 1) % n];
        Vec2 e = sub2(b, a);
        Vec2 r = sub2(X, a);
        double c = cross2(e, r);
        if (c < -1e-12) return false;
    }
    return true;
}

static Vec2 centroidPoly(const Poly& P) {
    double A2 = 0.0;
    double Cx = 0.0;
    double Cy = 0.0;
    const size_t n = P.v.size();

    for (size_t i = 0; i < n; ++i) {
        const Vec2& p = P.v[i];
        const Vec2& q = P.v[(i + 1) % n];
        double cr = cross2(p, q);
        A2 += cr;
        Cx += (p.x + q.x) * cr;
        Cy += (p.y + q.y) * cr;
    }

    if (std::abs(A2) < 1e-20) {
        Vec2 c{0.0, 0.0};
        for (const auto& p : P.v) {
            c.x += p.x;
            c.y += p.y;
        }
        c.x /= (double)n;
        c.y /= (double)n;
        return c;
    }

    double inv = 1.0 / (3.0 * A2);
    return {Cx * inv, Cy * inv};
}

static Line canonicalLineFromEdge(const Vec2& p, const Vec2& q) {
    Vec2 e = sub2(q, p);
    double a = -e.y;
    double b =  e.x;
    double nrm = std::sqrt(a * a + b * b);
    a /= nrm;
    b /= nrm;
    double c = -(a * p.x + b * p.y);

    if (a < -EPS_UNIQ || (std::abs(a) <= EPS_UNIQ && b < -EPS_UNIQ)) {
        a = -a; b = -b; c = -c;
    }
    if (std::abs(a) <= EPS_UNIQ) a = 0.0;
    if (std::abs(b) <= EPS_UNIQ) b = 0.0;
    if (std::abs(c) <= EPS_UNIQ) c = 0.0;

    return {a, b, c};
}

static bool sameLine(const Line& L1, const Line& L2) {
    return std::abs(L1.a - L2.a) <= 1e-11 &&
           std::abs(L1.b - L2.b) <= 1e-11 &&
           std::abs(L1.c - L2.c) <= 1e-11;
}

static vector<Line> uniqueLinesFromPolys(const vector<Poly>& polys) {
    vector<Line> lines;
    for (const auto& P : polys) {
        for (size_t i = 0; i < P.v.size(); ++i) {
            const Vec2& p = P.v[i];
            const Vec2& q = P.v[(i + 1) % P.v.size()];
            Vec2 e = sub2(q, p);
            if (std::abs(e.x) + std::abs(e.y) <= 1e-15) continue;
            Line L = canonicalLineFromEdge(p, q);

            bool found = false;
            for (const auto& U : lines) {
                if (sameLine(L, U)) {
                    found = true;
                    break;
                }
            }
            if (!found) lines.push_back(L);
        }
    }
    return lines;
}

static vector<Poly> splitCellsByLines(const Poly& P1, const vector<Line>& lines) {
    vector<Poly> cells;
    cells.push_back(P1);

    for (const auto& L : lines) {
        vector<Poly> next;
        next.reserve(cells.size() * 2);

        for (const auto& C : cells) {
            Poly Cp = clipHalfPlane(C, L, true);
            Poly Cn = clipHalfPlane(C, L, false);

            bool hp = (Cp.v.size() >= 3 && absArea(Cp) > EPS_AREA);
            bool hn = (Cn.v.size() >= 3 && absArea(Cn) > EPS_AREA);

            if (hp && hn) {
                next.push_back(std::move(Cp));
                next.push_back(std::move(Cn));
            } else if (hp) {
                next.push_back(std::move(Cp));
            } else if (hn) {
                next.push_back(std::move(Cn));
            }
        }

        cells.swap(next);
    }

    return cells;
}

static Vec3 lonLatToXYZ(double lon, double lat) {
    double cl = std::cos(lat);
    return {cl * std::cos(lon), cl * std::sin(lon), std::sin(lat)};
}

static void buildBasis(const Vec3& center, Vec3& E1, Vec3& E2) {
    Vec3 ref = {0.0, 0.0, 1.0};
    if (std::abs(dot3(center, ref)) > 0.95) {
        ref = {1.0, 0.0, 0.0};
    }
    E1 = normalize3(cross3(ref, center));
    E2 = normalize3(cross3(center, E1));
}

static bool gnomonicProject(const Vec3& P, const Vec3& Center, const Vec3& E1, const Vec3& E2, Vec2& out) {
    double den = dot3(P, Center);
    if (den <= 1e-12) return false;
    out.x = dot3(P, E1) / den;
    out.y = dot3(P, E2) / den;
    return true;
}

static void reverseVectorPair(vector<double>& A, vector<double>& B) {
    std::reverse(A.begin(), A.end());
    std::reverse(B.begin(), B.end());
}

static Poly readProjectedPolygonFromColumn(const double* Lon, const double* Lat, mwSize nRows, mwSize col,
                                           const Vec3& Center, const Vec3& E1, const Vec3& E2,
                                           bool& ok, bool requireAllProjected) {
    ok = true;
    vector<Vec2> pts;
    pts.reserve(nRows);

    for (mwSize r = 0; r < nRows; ++r) {
        double lon = Lon[r + col * nRows];
        double lat = Lat[r + col * nRows];
        if (mxIsNaN(lon) || mxIsNaN(lat)) continue;

        Vec3 P = lonLatToXYZ(lon, lat);
        Vec2 Q;
        bool good = gnomonicProject(P, Center, E1, E2, Q);
        if (!good) {
            if (requireAllProjected) {
                ok = false;
                return Poly();
            } else {
                ok = false;
                return Poly();
            }
        }
        pts.push_back(Q);
    }

    Poly P;
    P.v = std::move(pts);
    removeNearDuplicateVertices(P);
    if (P.v.size() < 3 || absArea(P) <= EPS_AREA) {
        P.v.clear();
        ok = false;
        return P;
    }
    ensureCCW(P);
    return P;
}

static Poly readProjectedPolygonFromVectors(const double* Lon, const double* Lat, mwSize n,
                                            const Vec3& Center, const Vec3& E1, const Vec3& E2,
                                            bool& ok) {
    ok = true;
    Poly P;
    P.v.reserve(n);

    for (mwSize i = 0; i < n; ++i) {
        double lon = Lon[i];
        double lat = Lat[i];
        if (mxIsNaN(lon) || mxIsNaN(lat)) continue;
        Vec3 X = lonLatToXYZ(lon, lat);
        Vec2 Q;
        if (!gnomonicProject(X, Center, E1, E2, Q)) {
            ok = false;
            return Poly();
        }
        P.v.push_back(Q);
    }

    removeNearDuplicateVertices(P);
    if (P.v.size() < 3 || absArea(P) <= EPS_AREA) {
        ok = false;
        P.v.clear();
        return P;
    }
    ensureCCW(P);
    return P;
}

static bool bitsetSubsetOf(const BitsetDyn& A, const BitsetDyn& B) {
    for (size_t i = 0; i < A.w.size(); ++i) {
        if ((A.w[i] & ~B.w[i]) != 0ULL) return false;
    }
    return true;
}

static BitsetDyn bitsetAndNot(const BitsetDyn& A, const BitsetDyn& B) {
    BitsetDyn R;
    R.w.resize(A.w.size());
    for (size_t i = 0; i < A.w.size(); ++i) {
        R.w[i] = A.w[i] & ~B.w[i];
    }
    return R;
}

struct SearchData {
    vector<BitsetDyn> Cover;
    vector<vector<int>> CellToPolys;
    vector<int> Best;
    int BestSize;
};

static void dfsSetCover(SearchData& D, BitsetDyn uncovered, vector<int>& chosen) {
    if (!uncovered.any()) {
        if ((int)chosen.size() < D.BestSize) {
            D.BestSize = (int)chosen.size();
            D.Best = chosen;
        }
        return;
    }

    if ((int)chosen.size() >= D.BestSize) return;

    int cell = uncovered.firstSetBit();
    if (cell < 0) return;

    vector<std::pair<int,int>> cand; // (-gain, poly)
    for (int p : D.CellToPolys[(size_t)cell]) {
        BitsetDyn rem = bitsetAndNot(uncovered, D.Cover[(size_t)p]);
        int gain = (int)(uncovered.count() - rem.count());
        if (gain > 0) cand.push_back({-gain, p});
    }

    std::sort(cand.begin(), cand.end());

    for (const auto& cp : cand) {
        int p = cp.second;

        chosen.push_back(p);
        BitsetDyn nextUncovered = bitsetAndNot(uncovered, D.Cover[(size_t)p]);
        dfsSetCover(D, nextUncovered, chosen);
        chosen.pop_back();
    }
}

void mexFunction(int nlhs, mxArray* plhs[],
                 int nrhs, const mxArray* prhs[]) {
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("isPolygonsTilesPolygon_mex:Input",
                          "Usage: PolyFlag = isPolygonsTilesPolygon_mex(LonPoly1, LatPoly1, LonPoly2, LatPoly2)");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("isPolygonsTilesPolygon_mex:Output",
                          "One output only.");
    }

    for (int i = 0; i < 4; ++i) {
        if (!mxIsDouble(prhs[i]) || mxIsComplex(prhs[i])) {
            mexErrMsgIdAndTxt("isPolygonsTilesPolygon_mex:Type",
                              "All inputs must be real double.");
        }
    }

    const mxArray* mxLon1 = prhs[0];
    const mxArray* mxLat1 = prhs[1];
    const mxArray* mxLon2 = prhs[2];
    const mxArray* mxLat2 = prhs[3];

    mwSize n1 = mxGetNumberOfElements(mxLon1);
    if (mxGetNumberOfElements(mxLat1) != n1) {
        mexErrMsgIdAndTxt("isPolygonsTilesPolygon_mex:Size",
                          "LonPoly1 and LatPoly1 must have same number of elements.");
    }

    mwSize nRows2 = mxGetM(mxLon2);
    mwSize nCols2 = mxGetN(mxLon2);
    if (mxGetM(mxLat2) != nRows2 || mxGetN(mxLat2) != nCols2) {
        mexErrMsgIdAndTxt("isPolygonsTilesPolygon_mex:Size",
                          "LonPoly2 and LatPoly2 must have identical size.");
    }

    const double* Lon1 = mxGetPr(mxLon1);
    const double* Lat1 = mxGetPr(mxLat1);
    const double* Lon2 = mxGetPr(mxLon2);
    const double* Lat2 = mxGetPr(mxLat2);

    if (n1 < 3 || nCols2 == 0) {
        plhs[0] = mxCreateLogicalMatrix(0, 0);
        return;
    }

    // Projection center from Poly1 mean vector
    Vec3 Center{0.0, 0.0, 0.0};
    for (mwSize i = 0; i < n1; ++i) {
        if (mxIsNaN(Lon1[i]) || mxIsNaN(Lat1[i])) continue;
        Vec3 P = lonLatToXYZ(Lon1[i], Lat1[i]);
        Center.x += P.x;
        Center.y += P.y;
        Center.z += P.z;
    }
    double nc = norm3(Center);
    if (!(nc > 0.0)) {
        mexErrMsgIdAndTxt("isPolygonsTilesPolygon_mex:Geometry",
                          "Failed to determine projection center.");
    }
    Center = normalize3(Center);

    Vec3 E1, E2;
    buildBasis(Center, E1, E2);

    bool ok1 = true;
    Poly P1 = readProjectedPolygonFromVectors(Lon1, Lat1, n1, Center, E1, E2, ok1);
    if (!ok1 || P1.v.size() < 3) {
        mexErrMsgIdAndTxt("isPolygonsTilesPolygon_mex:Geometry",
                          "Poly1 is invalid or not fully contained in the projection hemisphere.");
    }

    // Clip each Poly2 by P1, keep only non-empty
    vector<Poly> P2keep;
    vector<int> OrigIdx;
    P2keep.reserve(nCols2);
    OrigIdx.reserve(nCols2);

    for (mwSize c = 0; c < nCols2; ++c) {
        bool ok = true;
        Poly P2 = readProjectedPolygonFromColumn(Lon2, Lat2, nRows2, c, Center, E1, E2, ok, true);
        if (!ok || P2.v.size() < 3) continue;

        Poly Pint = convexIntersect(P2, P1);
        if (Pint.v.size() < 3 || absArea(Pint) <= EPS_AREA) continue;

        P2keep.push_back(std::move(Pint));
        OrigIdx.push_back((int)c);
    }

    if (P2keep.empty()) {
        plhs[0] = mxCreateLogicalMatrix(0, 0);
        return;
    }

    // Build exact cells by splitting P1 with all unique edge lines
    vector<Line> lines = uniqueLinesFromPolys(P2keep);
    vector<Poly> cells = splitCellsByLines(P1, lines);

    // Keep only cells inside P1 with positive area
    vector<Poly> cells2;
    vector<Vec2> reps;
    cells2.reserve(cells.size());
    reps.reserve(cells.size());

    for (const auto& C : cells) {
        if (C.v.size() < 3 || absArea(C) <= EPS_AREA) continue;
        Vec2 cen = centroidPoly(C);
        if (!pointInConvexPoly(P1, cen)) continue;
        cells2.push_back(C);
        reps.push_back(cen);
    }
    cells.swap(cells2);

    const size_t M = cells.size();
    const size_t N = P2keep.size();

    if (M == 0) {
        // Degenerate but safe: nothing to cover
        plhs[0] = mxCreateLogicalMatrix(1, nCols2);
        mxLogical* out = mxGetLogicals(plhs[0]);
        for (mwSize i = 0; i < nCols2; ++i) out[i] = false;
        return;
    }

    // Coverage bitsets
    vector<BitsetDyn> Cover(N, BitsetDyn(M));
    vector<vector<int>> CellToPolys(M);

    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < M; ++j) {
            if (pointInConvexPoly(P2keep[i], reps[j])) {
                Cover[i].set(j);
                CellToPolys[j].push_back((int)i);
            }
        }
    }

    // Quick impossibility test
    for (size_t j = 0; j < M; ++j) {
        if (CellToPolys[j].empty()) {
            plhs[0] = mxCreateLogicalMatrix(0, 0);
            return;
        }
    }

    // Remove dominated polygons: if coverage(i) subset coverage(k), drop i
    vector<char> Active(N, 1);
    for (size_t i = 0; i < N; ++i) {
        if (!Active[i]) continue;
        for (size_t k = 0; k < N; ++k) {
            if (i == k || !Active[k]) continue;
            if (bitsetSubsetOf(Cover[i], Cover[k])) {
                if (Cover[k].count() >= Cover[i].count()) {
                    Active[i] = 0;
                    break;
                }
            }
        }
    }

    vector<BitsetDyn> Cover2;
    vector<int> Orig2;
    Cover2.reserve(N);
    Orig2.reserve(N);

    for (size_t i = 0; i < N; ++i) {
        if (Active[i]) {
            Cover2.push_back(Cover[i]);
            Orig2.push_back(OrigIdx[i]);
        }
    }

    const size_t N2 = Cover2.size();
    vector<vector<int>> CellToPolys2(M);
    for (size_t i = 0; i < N2; ++i) {
        for (size_t j = 0; j < M; ++j) {
            if (Cover2[i].test(j)) CellToPolys2[j].push_back((int)i);
        }
    }

    for (size_t j = 0; j < M; ++j) {
        if (CellToPolys2[j].empty()) {
            plhs[0] = mxCreateLogicalMatrix(0, 0);
            return;
        }
    }

    // Exact set cover search
    SearchData SD;
    SD.Cover = std::move(Cover2);
    SD.CellToPolys = std::move(CellToPolys2);
    SD.BestSize = (int)N2 + 1;

    BitsetDyn uncovered(M);
    for (size_t j = 0; j < M; ++j) uncovered.set(j);

    vector<int> chosen;
    dfsSetCover(SD, uncovered, chosen);

    if (SD.Best.empty()) {
        plhs[0] = mxCreateLogicalMatrix(0, 0);
        return;
    }

    plhs[0] = mxCreateLogicalMatrix(1, nCols2);
    mxLogical* out = mxGetLogicals(plhs[0]);
    for (mwSize i = 0; i < nCols2; ++i) out[i] = false;

    for (int idx : SD.Best) {
        out[(mwSize)Orig2[(size_t)idx]] = true;
    }
}

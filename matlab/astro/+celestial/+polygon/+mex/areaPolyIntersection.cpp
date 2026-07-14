/*
 * areaPolyIntersection.cpp
 *
 * MEX implementation of celestial.polygon.areaPolyIntersection.
 *
 * Compile from the +celestial/+polygon directory, for example:
 *
 *   mex -R2018a CXX=g++-9 CXXFLAGS="$CXXFLAGS -O3 -march=native" areaPolyIntersection.cpp
 *
 * Usage:
 *
 *   [Area, AreaRePoly] = celestial.polygon.areaPolyIntersection( ...
 *       PolyRefLon, PolyRefLat, PolysLon, PolysLat, ...
 *       'CooUnits','rad');
 *
 * Inputs:
 *   PolyRefLon, PolyRefLat : vectors defining one convex reference polygon.
 *   PolysLon, PolysLat     : vectors defining one polygon, or matrices in
 *                            which each column defines one polygon.
 *                            NaN-padded columns are supported.
 *
 * Key/value options:
 *   'CooUnits'      : 'rad' (default) or 'deg'.
 *   'TolInside'     : default depends on input precision.
 *   'TolParallel'   : default depends on input precision.
 *   'TolDuplicate'  : default depends on input precision.
 *
 * Outputs:
 *   Area       : 1-by-Npoly intersection areas.
 *   AreaRePoly : area of the reference polygon.
 *
 * In radian mode areas are in steradians. In degree mode areas are in
 * square degrees.
 *
 * All four numeric inputs must be real, full, and of the same class:
 * either single or double. Outputs have the same class as the inputs.
 */

#include "mex.h"

#include <algorithm>
#include <cmath>
#include <cctype>
#include <cstddef>
#include <limits>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

namespace {

template<typename T>
struct Vec3 {
    T x;
    T y;
    T z;
};

template<typename T>
inline Vec3<T> makeVec(const T x, const T y, const T z) {
    return Vec3<T>{x, y, z};
}

template<typename T>
inline Vec3<T> add(const Vec3<T>& a, const Vec3<T>& b) {
    return makeVec<T>(a.x + b.x, a.y + b.y, a.z + b.z);
}

template<typename T>
inline Vec3<T> sub(const Vec3<T>& a, const Vec3<T>& b) {
    return makeVec<T>(a.x - b.x, a.y - b.y, a.z - b.z);
}

template<typename T>
inline Vec3<T> mul(const Vec3<T>& a, const T s) {
    return makeVec<T>(a.x*s, a.y*s, a.z*s);
}

template<typename T>
inline T dot(const Vec3<T>& a, const Vec3<T>& b) {
    return a.x*b.x + a.y*b.y + a.z*b.z;
}

template<typename T>
inline Vec3<T> cross(const Vec3<T>& a, const Vec3<T>& b) {
    return makeVec<T>(a.y*b.z - a.z*b.y,
                      a.z*b.x - a.x*b.z,
                      a.x*b.y - a.y*b.x);
}

template<typename T>
inline T norm2(const Vec3<T>& a) {
    return dot(a, a);
}

template<typename T>
inline T norm(const Vec3<T>& a) {
    return std::sqrt(norm2(a));
}

template<typename T>
inline Vec3<T> normalize(const Vec3<T>& a) {
    const T n = norm(a);
    return mul(a, T(1)/n);
}

template<typename T>
inline bool finiteValue(const T x) {
    return std::isfinite(static_cast<double>(x));
}

inline std::string lowerString(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(),
                   [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return s;
}

inline std::string mxToStringChecked(const mxArray* a, const char* name) {
    if (!mxIsChar(a)) {
        mexErrMsgIdAndTxt("celestial:polygon:areaPolyIntersection:InvalidOption",
                          "%s must be a character vector.", name);
    }

    char* p = mxArrayToString(a);
    if (p == nullptr) {
        mexErrMsgIdAndTxt("celestial:polygon:areaPolyIntersection:StringConversion",
                          "Failed converting %s to a string.", name);
    }

    std::string s(p);
    mxFree(p);
    return s;
}

template<typename T>
T scalarOption(const mxArray* a, const char* name) {
    if (!mxIsNumeric(a) || mxIsComplex(a) || mxGetNumberOfElements(a) != 1) {
        mexErrMsgIdAndTxt("celestial:polygon:areaPolyIntersection:InvalidOption",
                          "%s must be a real numeric scalar.", name);
    }

    const double value = mxGetScalar(a);
    if (!std::isfinite(value) || value < 0.0) {
        mexErrMsgIdAndTxt("celestial:polygon:areaPolyIntersection:InvalidOption",
                          "%s must be finite and nonnegative.", name);
    }

    return static_cast<T>(value);
}

template<typename T>
struct Options {
    bool degrees = false;
    T tolInside;
    T tolParallel;
    T tolDuplicate;
};

template<typename T>
Options<T> defaultOptions() {
    Options<T> o;

    if (std::is_same<T,double>::value) {
        o.tolInside    = static_cast<T>(1e-12);
        o.tolParallel  = static_cast<T>(1e-14);
        o.tolDuplicate = static_cast<T>(1e-10);
    } else {
        o.tolInside    = static_cast<T>(2e-6);
        o.tolParallel  = static_cast<T>(2e-7);
        o.tolDuplicate = static_cast<T>(2e-5);
    }

    return o;
}

template<typename T>
Options<T> parseOptions(const int nrhs, const mxArray* prhs[]) {
    Options<T> o = defaultOptions<T>();

    if ((nrhs - 4) % 2 != 0) {
        mexErrMsgIdAndTxt("celestial:polygon:areaPolyIntersection:KeyValuePairs",
                          "Optional arguments must be supplied as key/value pairs.");
    }

    for (int i = 4; i < nrhs; i += 2) {
        const std::string key = lowerString(mxToStringChecked(prhs[i], "Option name"));

        if (key == "coounits") {
            const std::string value =
                lowerString(mxToStringChecked(prhs[i + 1], "CooUnits"));

            if (value == "rad") {
                o.degrees = false;
            } else if (value == "deg") {
                o.degrees = true;
            } else {
                mexErrMsgIdAndTxt(
                    "celestial:polygon:areaPolyIntersection:UnknownCooUnits",
                    "CooUnits must be either 'rad' or 'deg'.");
            }
        } else if (key == "tolinside") {
            o.tolInside = scalarOption<T>(prhs[i + 1], "TolInside");
        } else if (key == "tolparallel") {
            o.tolParallel = scalarOption<T>(prhs[i + 1], "TolParallel");
        } else if (key == "tolduplicate") {
            o.tolDuplicate = scalarOption<T>(prhs[i + 1], "TolDuplicate");
        } else {
            mexErrMsgIdAndTxt(
                "celestial:polygon:areaPolyIntersection:UnknownOption",
                "Unknown option '%s'.", key.c_str());
        }
    }

    return o;
}

inline bool isVector(const mxArray* a) {
    const mwSize m = mxGetM(a);
    const mwSize n = mxGetN(a);
    return m == 1 || n == 1;
}

inline void validateNumericInput(const mxArray* a,
                                 const mxClassID classId,
                                 const char* name) {
    if (!mxIsNumeric(a) || mxIsSparse(a) || mxIsComplex(a) ||
        mxGetClassID(a) != classId) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:InvalidInput",
            "%s must be a real, full array of the same class as PolyRefLon.",
            name);
    }
}

template<typename T>
std::vector<Vec3<T>> lonLatToVectors(const T* lon,
                                     const T* lat,
                                     const mwSize count,
                                     const mwSize stride,
                                     const T toRad,
                                     const T tolDuplicate,
                                     const char* polygonDescription) {
    std::vector<Vec3<T>> v;
    v.reserve(static_cast<std::size_t>(count));

    for (mwSize i = 0; i < count; ++i) {
        const T lonValue = lon[i*stride];
        const T latValue = lat[i*stride];

        if (!finiteValue(lonValue) || !finiteValue(latValue)) {
            continue;
        }

        const T lambda = lonValue*toRad;
        const T phi    = latValue*toRad;

        if (std::abs(phi) > T(0.5)*static_cast<T>(M_PI) +
                            T(100)*std::numeric_limits<T>::epsilon()) {
            mexErrMsgIdAndTxt(
                "celestial:polygon:areaPolyIntersection:InvalidLatitude",
                "%s contains a latitude outside the valid range.", polygonDescription);
        }

        const T cp = std::cos(phi);
        Vec3<T> p = makeVec<T>(cp*std::cos(lambda),
                               cp*std::sin(lambda),
                               std::sin(phi));

        if (!v.empty()) {
            const Vec3<T> d = sub(p, v.back());
            if (norm2(d) <= tolDuplicate*tolDuplicate) {
                continue;
            }
        }

        v.push_back(p);
    }

    if (v.size() > 1U) {
        const Vec3<T> d = sub(v.front(), v.back());
        if (norm2(d) <= tolDuplicate*tolDuplicate) {
            v.pop_back();
        }
    }

    return v;
}

template<typename T>
std::vector<Vec3<T>> polygonNormals(const std::vector<Vec3<T>>& vertices,
                                    const T tolParallel) {
    const std::size_t n = vertices.size();

    if (n < 3U) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:TooFewVertices",
            "A polygon must contain at least three distinct vertices.");
    }

    Vec3<T> center = makeVec<T>(T(0), T(0), T(0));
    for (const Vec3<T>& p : vertices) {
        center = add(center, p);
    }

    const T centerNorm = norm(center);
    if (!(centerNorm > tolParallel)) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:AmbiguousInterior",
            "Unable to determine the polygon interior.");
    }
    center = mul(center, T(1)/centerNorm);

    std::vector<Vec3<T>> normals(n);

    for (std::size_t i = 0; i < n; ++i) {
        const std::size_t j = (i + 1U == n) ? 0U : i + 1U;
        Vec3<T> edgeNormal = cross(vertices[i], vertices[j]);
        const T edgeNorm = norm(edgeNormal);

        if (!(edgeNorm > tolParallel)) {
            mexErrMsgIdAndTxt(
                "celestial:polygon:areaPolyIntersection:DegenerateEdge",
                "Two consecutive polygon vertices are identical or antipodal.");
        }

        edgeNormal = mul(edgeNormal, T(1)/edgeNorm);
        if (dot(edgeNormal, center) < T(0)) {
            edgeNormal = mul(edgeNormal, T(-1));
        }
        normals[i] = edgeNormal;
    }

    const T convexTolerance =
        std::max(static_cast<T>(1e-10),
                 T(20)*std::numeric_limits<T>::epsilon());

    for (const Vec3<T>& normalVec : normals) {
        for (const Vec3<T>& vertex : vertices) {
            if (dot(normalVec, vertex) < -convexTolerance) {
                mexErrMsgIdAndTxt(
                    "celestial:polygon:areaPolyIntersection:NonConvexPolygon",
                    "A polygon is nonconvex, unordered, or not contained in a hemisphere.");
            }
        }
    }

    return normals;
}

template<typename T>
inline bool insideAll(const Vec3<T>& p,
                      const std::vector<Vec3<T>>& normals,
                      const T tolInside) {
    for (const Vec3<T>& n : normals) {
        if (dot(n, p) < -tolInside) {
            return false;
        }
    }
    return true;
}

template<typename T>
void appendUnique(std::vector<Vec3<T>>& points,
                  const Vec3<T>& candidate,
                  const T tolDuplicate2) {
    for (const Vec3<T>& p : points) {
        if (norm2(sub(candidate, p)) <= tolDuplicate2) {
            return;
        }
    }
    points.push_back(candidate);
}

template<typename T>
void orderVertices(std::vector<Vec3<T>>& vertices, const T tolParallel) {
    Vec3<T> center = makeVec<T>(T(0), T(0), T(0));
    for (const Vec3<T>& p : vertices) {
        center = add(center, p);
    }

    const T centerNorm = norm(center);
    if (!(centerNorm > tolParallel)) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:OrderingFailure",
            "Unable to determine an interior direction for the intersection.");
    }
    center = mul(center, T(1)/centerNorm);

    std::size_t refIndex = 0U;
    T bestProjNorm2 = T(-1);

    for (std::size_t i = 0; i < vertices.size(); ++i) {
        const T dc = dot(vertices[i], center);
        const T projNorm2 = std::max(T(0), T(1) - dc*dc);
        if (projNorm2 > bestProjNorm2) {
            bestProjNorm2 = projNorm2;
            refIndex = i;
        }
    }

    Vec3<T> e1 = sub(vertices[refIndex],
                     mul(center, dot(vertices[refIndex], center)));
    const T e1Norm = norm(e1);

    if (!(e1Norm > tolParallel)) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:OrderingFailure",
            "Unable to construct a tangent-plane basis.");
    }

    e1 = mul(e1, T(1)/e1Norm);
    Vec3<T> e2 = normalize(cross(center, e1));

    std::vector<std::pair<T,Vec3<T>>> anglePoint;
    anglePoint.reserve(vertices.size());

    for (const Vec3<T>& p : vertices) {
        anglePoint.emplace_back(std::atan2(dot(p,e2), dot(p,e1)), p);
    }

    std::sort(anglePoint.begin(), anglePoint.end(),
              [](const std::pair<T,Vec3<T>>& a,
                 const std::pair<T,Vec3<T>>& b) {
                  return a.first < b.first;
              });

    for (std::size_t i = 0; i < vertices.size(); ++i) {
        vertices[i] = anglePoint[i].second;
    }

    T orientation = T(0);
    for (std::size_t i = 0; i < vertices.size(); ++i) {
        const std::size_t j = (i + 1U == vertices.size()) ? 0U : i + 1U;
        orientation += dot(center, cross(vertices[i], vertices[j]));
    }

    if (orientation < T(0)) {
        std::reverse(vertices.begin(), vertices.end());
    }
}

template<typename T>
T sphericalPolygonArea(std::vector<Vec3<T>> vertices,
                       const T tolParallel) {
    if (vertices.size() < 3U) {
        return T(0);
    }

    orderVertices(vertices, tolParallel);

    Vec3<T> center = makeVec<T>(T(0), T(0), T(0));
    for (const Vec3<T>& p : vertices) {
        center = add(center, p);
    }

    const T centerNorm = norm(center);
    if (!(centerNorm > tolParallel)) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:AreaFailure",
            "Unable to determine a triangulation point.");
    }
    center = mul(center, T(1)/centerNorm);

    T omega = T(0);

    for (std::size_t i = 0; i < vertices.size(); ++i) {
        const std::size_t j = (i + 1U == vertices.size()) ? 0U : i + 1U;
        const Vec3<T>& b = vertices[i];
        const Vec3<T>& c = vertices[j];

        const T numerator = dot(center, cross(b,c));
        const T denominator =
            T(1) + dot(center,b) + dot(b,c) + dot(c,center);

        omega += T(2)*std::atan2(numerator, denominator);
    }

    return std::abs(omega);
}

template<typename T>
T intersectionArea(const std::vector<Vec3<T>>& v1,
                   const std::vector<Vec3<T>>& n1,
                   const std::vector<Vec3<T>>& v2,
                   const std::vector<Vec3<T>>& n2,
                   const Options<T>& options) {
    std::vector<Vec3<T>> candidates;
    candidates.reserve(v1.size() + v2.size() + 2U*n1.size()*n2.size());

    const T tolDuplicate2 = options.tolDuplicate*options.tolDuplicate;

    for (const Vec3<T>& p : v1) {
        if (insideAll(p, n2, options.tolInside)) {
            appendUnique(candidates, p, tolDuplicate2);
        }
    }

    for (const Vec3<T>& p : v2) {
        if (insideAll(p, n1, options.tolInside)) {
            appendUnique(candidates, p, tolDuplicate2);
        }
    }

    for (const Vec3<T>& a : n1) {
        for (const Vec3<T>& b : n2) {
            Vec3<T> r = cross(a,b);
            const T rNorm = norm(r);

            if (!(rNorm > options.tolParallel)) {
                continue;
            }

            r = mul(r, T(1)/rNorm);

            if (insideAll(r,n1,options.tolInside) &&
                insideAll(r,n2,options.tolInside)) {
                appendUnique(candidates, r, tolDuplicate2);
            }

            const Vec3<T> minusR = mul(r,T(-1));
            if (insideAll(minusR,n1,options.tolInside) &&
                insideAll(minusR,n2,options.tolInside)) {
                appendUnique(candidates, minusR, tolDuplicate2);
            }
        }
    }

    if (candidates.size() < 3U) {
        return T(0);
    }

    const T area = sphericalPolygonArea(candidates, options.tolParallel);
    const T zeroTolerance =
        T(100)*std::numeric_limits<T>::epsilon();

    return area < zeroTolerance ? T(0) : area;
}

template<typename T>
void runTyped(const int nlhs, mxArray* plhs[],
              const int nrhs, const mxArray* prhs[],
              const mxClassID classId) {
    validateNumericInput(prhs[0], classId, "PolyRefLon");
    validateNumericInput(prhs[1], classId, "PolyRefLat");
    validateNumericInput(prhs[2], classId, "PolysLon");
    validateNumericInput(prhs[3], classId, "PolysLat");

    if (!isVector(prhs[0]) || !isVector(prhs[1])) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:ReferenceNotVector",
            "PolyRefLon and PolyRefLat must be vectors.");
    }

    if (mxGetNumberOfElements(prhs[0]) != mxGetNumberOfElements(prhs[1])) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:ReferenceSizeMismatch",
            "PolyRefLon and PolyRefLat must contain the same number of elements.");
    }

    if (mxGetM(prhs[2]) != mxGetM(prhs[3]) ||
        mxGetN(prhs[2]) != mxGetN(prhs[3])) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:PolygonSizeMismatch",
            "PolysLon and PolysLat must have identical sizes.");
    }

    const Options<T> options = parseOptions<T>(nrhs,prhs);

    const T toRad = options.degrees
                  ? static_cast<T>(M_PI/180.0)
                  : T(1);
    const T areaFactor = options.degrees
                       ? static_cast<T>((180.0/M_PI)*(180.0/M_PI))
                       : T(1);

    const T* refLon = static_cast<const T*>(mxGetData(prhs[0]));
    const T* refLat = static_cast<const T*>(mxGetData(prhs[1]));
    const T* polysLon = static_cast<const T*>(mxGetData(prhs[2]));
    const T* polysLat = static_cast<const T*>(mxGetData(prhs[3]));

    const mwSize nRefCount = mxGetNumberOfElements(prhs[0]);

    std::vector<Vec3<T>> vRef =
        lonLatToVectors(refLon,refLat,nRefCount,1,toRad,
                        options.tolDuplicate,"The reference polygon");

    if (vRef.size() < 3U) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:TooFewReferenceVertices",
            "The reference polygon must contain at least three valid vertices.");
    }

    const std::vector<Vec3<T>> nRef =
        polygonNormals(vRef,options.tolParallel);

    const T areaRef = sphericalPolygonArea(vRef,options.tolParallel)*areaFactor;

    mwSize nRows = mxGetM(prhs[2]);
    mwSize nCols = mxGetN(prhs[2]);

    // A row or column vector always represents one polygon.
    const bool polygonInputIsVector = isVector(prhs[2]);
    const mwSize nPolygons = polygonInputIsVector ? 1 : nCols;
    const mwSize verticesPerPolygon =
        polygonInputIsVector ? mxGetNumberOfElements(prhs[2]) : nRows;

    plhs[0] = mxCreateNumericMatrix(1,nPolygons,classId,mxREAL);
    T* outArea = static_cast<T*>(mxGetData(plhs[0]));

    if (nlhs > 1) {
        plhs[1] = mxCreateNumericMatrix(1,1,classId,mxREAL);
        *static_cast<T*>(mxGetData(plhs[1])) = areaRef;
    }

    for (mwSize column = 0; column < nPolygons; ++column) {
        const T* lonColumn;
        const T* latColumn;
        mwSize stride;

        if (polygonInputIsVector) {
            lonColumn = polysLon;
            latColumn = polysLat;
            stride = 1;
        } else {
            lonColumn = polysLon + column*nRows;
            latColumn = polysLat + column*nRows;
            stride = 1;
        }

        std::vector<Vec3<T>> vPoly =
            lonLatToVectors(lonColumn,latColumn,verticesPerPolygon,stride,toRad,
                            options.tolDuplicate,"A polygon");

        if (vPoly.size() < 3U) {
            outArea[column] = T(0);
            continue;
        }

        const std::vector<Vec3<T>> nPoly =
            polygonNormals(vPoly,options.tolParallel);

        outArea[column] =
            intersectionArea(vRef,nRef,vPoly,nPoly,options)*areaFactor;
    }
}

} // namespace

void mexFunction(const int nlhs, mxArray* plhs[],
                 const int nrhs, const mxArray* prhs[]) {
    if (nrhs < 4) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:NotEnoughInputs",
            "Four numeric inputs are required.");
    }

    if (nlhs > 2) {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:TooManyOutputs",
            "At most two outputs are supported.");
    }

    const mxClassID classId = mxGetClassID(prhs[0]);

    if (classId == mxDOUBLE_CLASS) {
        runTyped<double>(nlhs,plhs,nrhs,prhs,classId);
    } else if (classId == mxSINGLE_CLASS) {
        runTyped<float>(nlhs,plhs,nrhs,prhs,classId);
    } else {
        mexErrMsgIdAndTxt(
            "celestial:polygon:areaPolyIntersection:UnsupportedClass",
            "Inputs must be single or double precision.");
    }
}

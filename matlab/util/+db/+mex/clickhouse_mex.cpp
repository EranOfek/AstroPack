// src/clickhouse_mex.cpp
#include "mex.h"
#include <string>
#include <unordered_map>
#include <cstdint>
#include <cmath>
#include <limits>
#include <clickhouse/client.h>
#include <clickhouse/columns/numeric.h>
#include <clickhouse/columns/string.h>
#include <clickhouse/columns/array.h>
#include <clickhouse/columns/nullable.h>
#include <clickhouse/types/types.h>
#include <vector>
#include <unordered_set>

using namespace clickhouse;

static std::unordered_map<uint64_t, Client*> g_clients;
static uint64_t g_next_id = 1;
static bool g_exit_registered = false;

static void cleanup_all() {
    for (auto& p : g_clients) delete p.second;
    g_clients.clear();
}

static Client* get_client(const mxArray* h) {
    uint64_t id = *mxGetUint64s(h);
    auto it = g_clients.find(id);
    if (it == g_clients.end())
        mexErrMsgIdAndTxt("ClickHouse:invalidHandle", "Invalid or closed connection handle.");
    return it->second;
}

// ── connect ──────────────────────────────────────────────────────────────────
static void cmd_connect(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 5)
        mexErrMsgIdAndTxt("ClickHouse:badArgs", "connect requires host,port,user,pass,options");

    char* host_c = mxArrayToUTF8String(prhs[1]);
    if (!host_c) mexErrMsgIdAndTxt("ClickHouse:badArgs", "Failed to read host.");
    char* user_c = mxArrayToUTF8String(prhs[3]);
    if (!user_c) { mxFree(host_c); mexErrMsgIdAndTxt("ClickHouse:badArgs", "Failed to read user."); }
    char* pass_c = mxArrayToUTF8String(prhs[4]);
    if (!pass_c) { mxFree(host_c); mxFree(user_c); mexErrMsgIdAndTxt("ClickHouse:badArgs", "Failed to read password."); }

    // Copy into std::string immediately, then free MATLAB buffers before building opts
    std::string host(host_c), user(user_c), pass(pass_c);
    mxFree(host_c); mxFree(user_c); mxFree(pass_c);
    double port = mxGetScalar(prhs[2]);

    ClientOptions opts;
    opts.SetHost(host)
        .SetPort(static_cast<uint16_t>(port))
        .SetUser(user)
        .SetPassword(pass)
        .SetPingBeforeQuery(false)
        .SetConnectionRecvTimeout(std::chrono::seconds(30))
        .SetConnectionSendTimeout(std::chrono::seconds(30));

    // options struct (prhs[5])
    if (nrhs > 5 && !mxIsEmpty(prhs[5])) {
        const mxArray* opt = prhs[5];

        // TLS
        mxArray* tls = mxGetField(opt, 0, "tls");
        if (tls) {
            mxArray* enabled = mxGetField(tls, 0, "enabled");
            if (enabled && mxIsLogicalScalarTrue(enabled)) {
                ClientOptions::SSLOptions ssl;
                ssl.SetUseSNI(true).SetUseDefaultCALocations(true);

                mxArray* skip = mxGetField(tls, 0, "skip_verification");
                if (skip && mxIsLogicalScalarTrue(skip))
                    ssl.SetSkipVerification(true);

                mxArray* ca = mxGetField(tls, 0, "ca_file");
                if (ca && !mxIsEmpty(ca)) {
                    char* ca_str = mxArrayToUTF8String(ca);
                    if (ca_str) {
                        ssl.SetPathToCAFiles(std::vector<std::string>{ca_str});
                        mxFree(ca_str);
                    }
                }
                opts.SetSSLOptions(ssl);
            }
        }

        // useragent: SetClientName does not exist on ClientOptions — omitted.
        // settings:  SetSetting does not exist on ClientOptions — omitted.
    }

    try {
        Client* client = new Client(opts);
        uint64_t id = g_next_id++;
        g_clients[id] = client;
        plhs[0] = mxCreateNumericMatrix(1, 1, mxUINT64_CLASS, mxREAL);
        *mxGetUint64s(plhs[0]) = id;
    } catch (const std::exception& e) {
        mexErrMsgIdAndTxt("ClickHouse:connectionError", "%s", e.what());
    }
}

// ── ping ─────────────────────────────────────────────────────────────────────
static void cmd_ping(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 2) mexErrMsgIdAndTxt("ClickHouse:badArgs", "ping requires a connection handle.");
    Client* client = get_client(prhs[1]);
    try {
        client->Execute(Query("SELECT 1"));
        plhs[0] = mxCreateLogicalScalar(true);
    } catch (const std::exception& e) {
        mexErrMsgIdAndTxt("ClickHouse:pingError", "%s", e.what());
    }
}

// ── delete ───────────────────────────────────────────────────────────────────
static void cmd_delete(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 2) mexErrMsgIdAndTxt("ClickHouse:badArgs", "delete requires a connection handle.");
    uint64_t id = *mxGetUint64s(prhs[1]);
    auto it = g_clients.find(id);
    if (it != g_clients.end()) {
        delete it->second;
        g_clients.erase(it);
    }
}

// ── query ─────────────────────────────────────────────────────────────────────
static void cmd_query(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 3) mexErrMsgIdAndTxt("ClickHouse:badArgs", "query requires handle and sql string.");
    Client* client = get_client(prhs[1]);
    char* sql_c = mxArrayToUTF8String(prhs[2]);
    if (!sql_c) mexErrMsgIdAndTxt("ClickHouse:badArgs", "Failed to read SQL string.");
    std::string sql(sql_c);
    mxFree(sql_c);

    std::vector<Block> blocks;
    size_t total_rows = 0;

    Block schema_block;
    bool has_schema = false;

    try {
        client->Select(sql, [&](const Block& b) {
            if (!has_schema && b.GetColumnCount() > 0) {
                schema_block = b;
                has_schema = true;
            }
            if (b.GetRowCount() > 0) {
                blocks.push_back(b);
                total_rows += b.GetRowCount();
            }
        });
    } catch (const std::exception& e) {
        mexErrMsgIdAndTxt("ClickHouse:queryError", "%s", e.what());
    }

    // Determine which block to use for schema
    const Block& schema = has_schema ? schema_block : (blocks.empty() ? schema_block : blocks[0]);

    if (blocks.empty()) {
        // Return empty table with correct column names and types
        if (!has_schema || schema.GetColumnCount() == 0) {
            plhs[0] = mxCreateStructMatrix(1, 1, 0, nullptr);
            return;
        }
        size_t ncols = schema.GetColumnCount();
        std::vector<std::string> names;
        for (auto it = schema.begin(); it != schema.end(); ++it)
            names.push_back(it.Name());
        std::vector<const char*> name_ptrs;
        for (const auto& n : names) name_ptrs.push_back(n.c_str());
        plhs[0] = mxCreateStructMatrix(1, 1, static_cast<int>(ncols), name_ptrs.data());
        // Set each column to an appropriately-typed empty array
        size_t ci = 0;
        for (auto it = schema.begin(); it != schema.end(); ++it, ++ci) {
            Type::Code tc = it.Type()->GetCode();
            mxArray* empty = nullptr;
            switch (tc) {
            case Type::Float64: empty = mxCreateNumericMatrix(0,1,mxDOUBLE_CLASS, mxREAL); break;
            case Type::Float32: empty = mxCreateNumericMatrix(0,1,mxSINGLE_CLASS, mxREAL); break;
            case Type::Int8:    empty = mxCreateNumericMatrix(0,1,mxINT8_CLASS,   mxREAL); break;
            case Type::Int16:   empty = mxCreateNumericMatrix(0,1,mxINT16_CLASS,  mxREAL); break;
            case Type::Int32:   empty = mxCreateNumericMatrix(0,1,mxINT32_CLASS,  mxREAL); break;
            case Type::Int64:   empty = mxCreateNumericMatrix(0,1,mxINT64_CLASS,  mxREAL); break;
            case Type::UInt8:   empty = mxCreateNumericMatrix(0,1,mxUINT8_CLASS,  mxREAL); break;
            case Type::UInt16:  empty = mxCreateNumericMatrix(0,1,mxUINT16_CLASS, mxREAL); break;
            case Type::UInt32:  empty = mxCreateNumericMatrix(0,1,mxUINT32_CLASS, mxREAL); break;
            case Type::UInt64:  empty = mxCreateNumericMatrix(0,1,mxUINT64_CLASS, mxREAL); break;
            case Type::String:   empty = mxCreateCellMatrix(0,1); break;
            case Type::Array:    empty = mxCreateCellMatrix(0,1); break;
            case Type::Nullable: {
                // Use inner type to pick the right empty array
                auto inner_tc = it.Type()->As<NullableType>()->GetNestedType()->GetCode();
                if (inner_tc == Type::Float32)
                    empty = mxCreateNumericMatrix(0,1,mxSINGLE_CLASS,mxREAL);
                else if (inner_tc == Type::String)
                    empty = mxCreateCellMatrix(0,1);
                else
                    empty = mxCreateNumericMatrix(0,1,mxDOUBLE_CLASS,mxREAL);
                break;
            }
            default:             empty = mxCreateNumericMatrix(0,1,mxDOUBLE_CLASS, mxREAL); break;
            }
            mxSetField(plhs[0], 0, name_ptrs[ci], empty);
        }
        return;
    }

    const Block& first = blocks[0];  // used for type dispatch in main path
    size_t ncols = first.GetColumnCount();

    // Collect column names and types from first block's iterator
    std::vector<std::string> names;
    std::vector<Type::Code>  type_codes;
    for (auto it = first.begin(); it != first.end(); ++it) {
        names.push_back(it.Name());
        type_codes.push_back(it.Type()->GetCode());
    }

    std::vector<const char*> name_ptrs;
    for (const auto& n : names) name_ptrs.push_back(n.c_str());

    plhs[0] = mxCreateStructMatrix(1, 1, static_cast<int>(ncols), name_ptrs.data());

    for (size_t ci = 0; ci < ncols; ci++) {
        mxArray* col_arr = nullptr;

        switch (type_codes[ci]) {
        case Type::Float64: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
            double* dst = mxGetDoubles(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnFloat64>();
                for (size_t r = 0; r < col->Size(); r++) dst[off+r] = (*col)[r];
                off += col->Size();
            }
            break;
        }
        case Type::Float32: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxSINGLE_CLASS, mxREAL);
            float* dst = mxGetSingles(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnFloat32>();
                for (size_t r = 0; r < col->Size(); r++) dst[off+r] = (*col)[r];
                off += col->Size();
            }
            break;
        }
        case Type::Int8: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxINT8_CLASS, mxREAL);
            int8_T* dst = mxGetInt8s(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnInt8>();
                for (size_t r = 0; r < col->Size(); r++) dst[off+r] = (*col)[r];
                off += col->Size();
            }
            break;
        }
        case Type::Int16: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxINT16_CLASS, mxREAL);
            int16_T* dst = mxGetInt16s(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnInt16>();
                for (size_t r = 0; r < col->Size(); r++) dst[off+r] = (*col)[r];
                off += col->Size();
            }
            break;
        }
        case Type::Int32: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxINT32_CLASS, mxREAL);
            int32_T* dst = mxGetInt32s(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnInt32>();
                for (size_t r = 0; r < col->Size(); r++) dst[off+r] = (*col)[r];
                off += col->Size();
            }
            break;
        }
        case Type::Int64: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxINT64_CLASS, mxREAL);
            int64_T* dst = mxGetInt64s(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnInt64>();
                for (size_t r = 0; r < col->Size(); r++) dst[off+r] = (*col)[r];
                off += col->Size();
            }
            break;
        }
        case Type::UInt8: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxUINT8_CLASS, mxREAL);
            uint8_T* dst = mxGetUint8s(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnUInt8>();
                for (size_t r = 0; r < col->Size(); r++) dst[off+r] = (*col)[r];
                off += col->Size();
            }
            break;
        }
        case Type::UInt16: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxUINT16_CLASS, mxREAL);
            uint16_T* dst = mxGetUint16s(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnUInt16>();
                for (size_t r = 0; r < col->Size(); r++) dst[off+r] = (*col)[r];
                off += col->Size();
            }
            break;
        }
        case Type::UInt32: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxUINT32_CLASS, mxREAL);
            uint32_T* dst = mxGetUint32s(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnUInt32>();
                for (size_t r = 0; r < col->Size(); r++) dst[off+r] = (*col)[r];
                off += col->Size();
            }
            break;
        }
        case Type::UInt64: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxUINT64_CLASS, mxREAL);
            uint64_T* dst = mxGetUint64s(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnUInt64>();
                for (size_t r = 0; r < col->Size(); r++) dst[off+r] = (*col)[r];
                off += col->Size();
            }
            break;
        }
        case Type::String: {
            col_arr = mxCreateCellMatrix(total_rows, 1);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnString>();
                for (size_t r = 0; r < col->Size(); r++) {
                    std::string_view sv = col->At(r);
                    mxSetCell(col_arr, off + r,
                              mxCreateString(std::string(sv).c_str()));
                }
                off += col->Size();
            }
            break;
        }
        case Type::Array: {
            col_arr = mxCreateCellMatrix(total_rows, 1);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto arr_col = blk[ci]->As<ColumnArray>();
                for (size_t r = 0; r < arr_col->Size(); r++) {
                    auto inner = arr_col->GetAsColumn(r);
                    size_t inner_n = inner->Size();
                    Type::Code inner_code = inner->Type()->GetCode();
                    mxArray* row_arr = nullptr;
                    size_t arr_rows = (inner_n > 0) ? 1 : 0;

                    if (inner_code == Type::Float64) {
                        row_arr = mxCreateNumericMatrix(arr_rows, inner_n, mxDOUBLE_CLASS, mxREAL);
                        double* d = mxGetDoubles(row_arr);
                        auto ic = inner->As<ColumnFloat64>();
                        for (size_t j = 0; j < inner_n; j++) d[j] = (*ic)[j];
                    } else if (inner_code == Type::Float32) {
                        row_arr = mxCreateNumericMatrix(arr_rows, inner_n, mxSINGLE_CLASS, mxREAL);
                        float* d = mxGetSingles(row_arr);
                        auto ic = inner->As<ColumnFloat32>();
                        for (size_t j = 0; j < inner_n; j++) d[j] = (*ic)[j];
                    } else if (inner_code == Type::Int32) {
                        row_arr = mxCreateNumericMatrix(arr_rows, inner_n, mxINT32_CLASS, mxREAL);
                        int32_T* d = mxGetInt32s(row_arr);
                        auto ic = inner->As<ColumnInt32>();
                        for (size_t j = 0; j < inner_n; j++) d[j] = (*ic)[j];
                    } else if (inner_code == Type::Int64) {
                        row_arr = mxCreateNumericMatrix(arr_rows, inner_n, mxINT64_CLASS, mxREAL);
                        int64_T* d = mxGetInt64s(row_arr);
                        auto ic = inner->As<ColumnInt64>();
                        for (size_t j = 0; j < inner_n; j++) d[j] = (*ic)[j];
                    } else if (inner_code == Type::UInt32) {
                        row_arr = mxCreateNumericMatrix(arr_rows, inner_n, mxUINT32_CLASS, mxREAL);
                        uint32_T* d = mxGetUint32s(row_arr);
                        auto ic = inner->As<ColumnUInt32>();
                        for (size_t j = 0; j < inner_n; j++) d[j] = (*ic)[j];
                    } else if (inner_code == Type::UInt64) {
                        row_arr = mxCreateNumericMatrix(arr_rows, inner_n, mxUINT64_CLASS, mxREAL);
                        uint64_T* d = mxGetUint64s(row_arr);
                        auto ic = inner->As<ColumnUInt64>();
                        for (size_t j = 0; j < inner_n; j++) d[j] = (*ic)[j];
                    } else if (inner_code == Type::String) {
                        // Array(String): return cell array of char strings
                        row_arr = mxCreateCellMatrix(arr_rows, inner_n);
                        auto ic = inner->As<ColumnString>();
                        for (size_t j = 0; j < inner_n; j++) {
                            std::string_view sv = ic->At(j);
                            mxSetCell(row_arr, j, mxCreateString(std::string(sv).c_str()));
                        }
                    } else {
                        row_arr = mxCreateNumericMatrix(0, 0, mxDOUBLE_CLASS, mxREAL);
                    }

                    if (!row_arr) row_arr = mxCreateNumericMatrix(0, 0, mxDOUBLE_CLASS, mxREAL);
                    mxSetCell(col_arr, off + r, row_arr);
                }
                off += arr_col->Size();
            }
            break;
        }
        case Type::Nullable: {
            // Determine inner type from first block
            auto nc0 = blocks[0][ci]->As<ColumnNullable>();
            Type::Code inner_tc = nc0->Nested()->Type()->GetCode();
            static const double kNaN = std::numeric_limits<double>::quiet_NaN();
            static const float  kNaNf = std::numeric_limits<float>::quiet_NaN();

            if (inner_tc == Type::Float64) {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
                double* dst = mxGetDoubles(col_arr);
                for (size_t r = 0; r < total_rows; r++) dst[r] = kNaN;
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnFloat64>();
                    for (size_t r = 0; r < nc->Size(); r++)
                        if (!nc->IsNull(r)) dst[off+r] = (*ic)[r];
                    off += nc->Size();
                }
            } else if (inner_tc == Type::Float32) {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxSINGLE_CLASS, mxREAL);
                float* dst = mxGetSingles(col_arr);
                for (size_t r = 0; r < total_rows; r++) dst[r] = kNaNf;
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnFloat32>();
                    for (size_t r = 0; r < nc->Size(); r++)
                        if (!nc->IsNull(r)) dst[off+r] = (*ic)[r];
                    off += nc->Size();
                }
            } else if (inner_tc == Type::String) {
                // Nullable(String): cell of chars; null → [] (empty double sentinel)
                col_arr = mxCreateCellMatrix(total_rows, 1);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnString>();
                    for (size_t r = 0; r < nc->Size(); r++) {
                        if (nc->IsNull(r))
                            mxSetCell(col_arr, off+r, mxCreateDoubleMatrix(0, 0, mxREAL));
                        else {
                            std::string_view sv = ic->At(r);
                            mxSetCell(col_arr, off+r, mxCreateString(std::string(sv).c_str()));
                        }
                    }
                    off += nc->Size();
                }
            } else {
                // Nullable integer types → double with NaN for nulls
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
                double* dst = mxGetDoubles(col_arr);
                for (size_t r = 0; r < total_rows; r++) dst[r] = kNaN;
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto nested = nc->Nested();
                    for (size_t r = 0; r < nc->Size(); r++) {
                        if (!nc->IsNull(r)) {
                            switch (inner_tc) {
                            case Type::Int8:   dst[off+r] = (double)(*nested->As<ColumnInt8>())[r];   break;
                            case Type::Int16:  dst[off+r] = (double)(*nested->As<ColumnInt16>())[r];  break;
                            case Type::Int32:  dst[off+r] = (double)(*nested->As<ColumnInt32>())[r];  break;
                            case Type::Int64:  dst[off+r] = (double)(*nested->As<ColumnInt64>())[r];  break;
                            case Type::UInt8:  dst[off+r] = (double)(*nested->As<ColumnUInt8>())[r];  break;
                            case Type::UInt16: dst[off+r] = (double)(*nested->As<ColumnUInt16>())[r]; break;
                            case Type::UInt32: dst[off+r] = (double)(*nested->As<ColumnUInt32>())[r]; break;
                            case Type::UInt64: dst[off+r] = (double)(*nested->As<ColumnUInt64>())[r]; break;
                            default: break;
                            }
                        }
                    }
                    off += nc->Size();
                }
            }
            break;
        }
        default:
            mexErrMsgIdAndTxt("ClickHouse:unsupportedType",
                "Unsupported column type code %d for column '%s'.",
                (int)type_codes[ci], names[ci].c_str());
        }

        mxSetField(plhs[0], 0, name_ptrs[ci], col_arr);
    }
}

// ── insert ───────────────────────────────────────────────────────────────────
static void cmd_insert(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 4) mexErrMsgIdAndTxt("ClickHouse:badArgs", "insert requires handle, table name, and data struct.");
    Client* client = get_client(prhs[1]);

    char* table_c = mxArrayToUTF8String(prhs[2]);
    if (!table_c) mexErrMsgIdAndTxt("ClickHouse:badArgs", "Failed to read table name.");
    std::string table_name(table_c);
    mxFree(table_c);

    const mxArray* s = prhs[3];
    if (!mxIsStruct(s)) mexErrMsgIdAndTxt("ClickHouse:badArgs", "Data argument must be a struct.");
    int nfields = mxGetNumberOfFields(s);

    // Extract nullable column hints provided by the MATLAB layer (from DESCRIBE TABLE).
    // These are columns that must be inserted as ColumnNullable even when no NaN/sentinel present.
    std::unordered_set<std::string> nullable_cols;
    int hint_fi = mxGetFieldNumber(s, "ch_nullable_hint");
    if (hint_fi >= 0) {
        const mxArray* hint = mxGetFieldByNumber(s, 0, hint_fi);
        if (hint && mxIsCell(hint)) {
            size_t hn = mxGetNumberOfElements(hint);
            for (size_t i = 0; i < hn; i++) {
                const mxArray* elem = mxGetCell(hint, i);
                if (elem && mxIsChar(elem)) {
                    char* cname = mxArrayToUTF8String(elem);
                    if (cname) { nullable_cols.insert(cname); mxFree(cname); }
                }
            }
        }
    }

    Block block;

    for (int fi = 0; fi < nfields; fi++) {
        const char* fname  = mxGetFieldNameByNumber(s, fi);
        if (std::string(fname) == "ch_nullable_hint") continue;  // hint field, not a data column
        const mxArray* fd  = mxGetFieldByNumber(s, 0, fi);
        if (!fd) continue;
        mxClassID cid = mxGetClassID(fd);
        size_t n      = mxGetNumberOfElements(fd);

        switch (cid) {
        case mxDOUBLE_CLASS: {
            double* data = mxGetDoubles(fd);
            bool has_nan = false;
            for (size_t i = 0; i < n && !has_nan; i++) has_nan = std::isnan(data[i]);
            bool force_nullable = nullable_cols.count(fname) > 0;
            if (has_nan || force_nullable) {
                auto inner = std::make_shared<ColumnFloat64>();
                auto nulls = std::make_shared<ColumnUInt8>();
                for (size_t i = 0; i < n; i++) {
                    bool is_null = std::isnan(data[i]);
                    nulls->Append(is_null ? 1 : 0);
                    inner->Append(is_null ? 0.0 : data[i]);
                }
                block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
            } else {
                auto col = std::make_shared<ColumnFloat64>();
                for (size_t i = 0; i < n; i++) col->Append(data[i]);
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxSINGLE_CLASS: {
            float* data = mxGetSingles(fd);
            bool has_nan = false;
            for (size_t i = 0; i < n && !has_nan; i++) has_nan = std::isnan(data[i]);
            bool force_nullable = nullable_cols.count(fname) > 0;
            if (has_nan || force_nullable) {
                auto inner = std::make_shared<ColumnFloat32>();
                auto nulls = std::make_shared<ColumnUInt8>();
                for (size_t i = 0; i < n; i++) {
                    bool is_null = std::isnan(data[i]);
                    nulls->Append(is_null ? 1 : 0);
                    inner->Append(is_null ? 0.0f : data[i]);
                }
                block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
            } else {
                auto col = std::make_shared<ColumnFloat32>();
                for (size_t i = 0; i < n; i++) col->Append(data[i]);
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxINT8_CLASS: {
            auto col = std::make_shared<ColumnInt8>();
            int8_T* data = mxGetInt8s(fd);
            for (size_t i = 0; i < n; i++) col->Append(data[i]);
            block.AppendColumn(fname, col);
            break;
        }
        case mxINT16_CLASS: {
            auto col = std::make_shared<ColumnInt16>();
            int16_T* data = mxGetInt16s(fd);
            for (size_t i = 0; i < n; i++) col->Append(data[i]);
            block.AppendColumn(fname, col);
            break;
        }
        case mxINT32_CLASS: {
            auto col = std::make_shared<ColumnInt32>();
            int32_T* data = mxGetInt32s(fd);
            for (size_t i = 0; i < n; i++) col->Append(data[i]);
            block.AppendColumn(fname, col);
            break;
        }
        case mxINT64_CLASS: {
            auto col = std::make_shared<ColumnInt64>();
            int64_T* data = mxGetInt64s(fd);
            for (size_t i = 0; i < n; i++) col->Append(data[i]);
            block.AppendColumn(fname, col);
            break;
        }
        case mxUINT8_CLASS: {
            auto col = std::make_shared<ColumnUInt8>();
            uint8_T* data = mxGetUint8s(fd);
            for (size_t i = 0; i < n; i++) col->Append(data[i]);
            block.AppendColumn(fname, col);
            break;
        }
        case mxUINT16_CLASS: {
            auto col = std::make_shared<ColumnUInt16>();
            uint16_T* data = mxGetUint16s(fd);
            for (size_t i = 0; i < n; i++) col->Append(data[i]);
            block.AppendColumn(fname, col);
            break;
        }
        case mxUINT32_CLASS: {
            auto col = std::make_shared<ColumnUInt32>();
            uint32_T* data = mxGetUint32s(fd);
            for (size_t i = 0; i < n; i++) col->Append(data[i]);
            block.AppendColumn(fname, col);
            break;
        }
        case mxUINT64_CLASS: {
            auto col = std::make_shared<ColumnUInt64>();
            uint64_T* data = mxGetUint64s(fd);
            for (size_t i = 0; i < n; i++) col->Append(data[i]);
            block.AppendColumn(fname, col);
            break;
        }
        case mxCELL_CLASS: {
            // Detect whether this is a String column (cell of char) or Array(T) column
            bool is_string_col = false;
            bool is_arr_str_col = false;
            mxClassID inner_class = mxUNKNOWN_CLASS;
            mxClassID empty_hint = mxUNKNOWN_CLASS;
            for (size_t i = 0; i < n; i++) {
                const mxArray* cell = mxGetCell(fd, i);
                if (!cell) continue;
                if (!mxIsEmpty(cell)) {
                    is_string_col  = mxIsChar(cell);
                    is_arr_str_col = mxIsCell(cell);
                    inner_class    = mxGetClassID(cell);
                    break;
                } else if (empty_hint == mxUNKNOWN_CLASS) {
                    empty_hint = mxGetClassID(cell);
                }
            }
            // All cells were empty: use the class of the empty cells as a type hint.
            // {} (mxCELL_CLASS) → Array(String); double([]) (mxDOUBLE_CLASS) → Array(Float64).
            if (inner_class == mxUNKNOWN_CLASS && empty_hint != mxUNKNOWN_CLASS) {
                if (empty_hint == mxCELL_CLASS)
                    is_arr_str_col = true;
                else
                    inner_class = empty_hint;
            }

            if (is_string_col) {
                // Check for null sentinels: [] (empty double) = NULL → Nullable(String)
                bool has_null_sentinel = false;
                for (size_t i = 0; i < n && !has_null_sentinel; i++) {
                    const mxArray* cell = mxGetCell(fd, i);
                    if (cell && mxIsEmpty(cell) && !mxIsChar(cell) && !mxIsCell(cell))
                        has_null_sentinel = true;
                }
                bool force_nullable = nullable_cols.count(fname) > 0;
                if (has_null_sentinel || force_nullable) {
                    auto inner = std::make_shared<ColumnString>();
                    auto nulls = std::make_shared<ColumnUInt8>();
                    for (size_t i = 0; i < n; i++) {
                        const mxArray* cell = mxGetCell(fd, i);
                        bool is_null = !cell || (mxIsEmpty(cell) && !mxIsChar(cell) && !mxIsCell(cell));
                        nulls->Append(is_null ? 1 : 0);
                        if (is_null) { inner->Append(""); continue; }
                        char* str = mxArrayToUTF8String(cell);
                        if (str) { inner->Append(str); mxFree(str); }
                        else      { inner->Append(""); }
                    }
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else {
                // Plain String column: cell array of char
                auto col = std::make_shared<ColumnString>();
                for (size_t i = 0; i < n; i++) {
                    const mxArray* cell = mxGetCell(fd, i);
                    if (!cell || mxIsEmpty(cell)) { col->Append(""); continue; }
                    char* str = mxArrayToUTF8String(cell);
                    if (str) { col->Append(str); mxFree(str); }
                    else      { col->Append(""); }
                }
                block.AppendColumn(fname, col);
                }
            } else if (is_arr_str_col) {
                // Array(String) column: cell of cell-of-char
                auto arr_col = std::make_shared<ColumnArray>(std::make_shared<ColumnString>());
                for (size_t i = 0; i < n; i++) {
                    const mxArray* cell = mxGetCell(fd, i);
                    auto inner = std::make_shared<ColumnString>();
                    if (cell && !mxIsEmpty(cell)) {
                        size_t m = mxGetNumberOfElements(cell);
                        for (size_t j = 0; j < m; j++) {
                            const mxArray* elem = mxGetCell(cell, j);
                            if (!elem || mxIsEmpty(elem)) { inner->Append(""); continue; }
                            char* str = mxArrayToUTF8String(elem);
                            if (str) { inner->Append(str); mxFree(str); }
                            else      { inner->Append(""); }
                        }
                    }
                    arr_col->AppendAsColumn(inner);
                }
                block.AppendColumn(fname, arr_col);
            } else if (inner_class != mxUNKNOWN_CLASS) {
                // Array(T) numeric column
                auto make_inner_col = [&]() -> ColumnRef {
                    switch (inner_class) {
                    case mxDOUBLE_CLASS:  return std::make_shared<ColumnFloat64>();
                    case mxSINGLE_CLASS:  return std::make_shared<ColumnFloat32>();
                    case mxINT8_CLASS:    return std::make_shared<ColumnInt8>();
                    case mxINT16_CLASS:   return std::make_shared<ColumnInt16>();
                    case mxINT32_CLASS:   return std::make_shared<ColumnInt32>();
                    case mxINT64_CLASS:   return std::make_shared<ColumnInt64>();
                    case mxUINT8_CLASS:   return std::make_shared<ColumnUInt8>();
                    case mxUINT16_CLASS:  return std::make_shared<ColumnUInt16>();
                    case mxUINT32_CLASS:  return std::make_shared<ColumnUInt32>();
                    case mxUINT64_CLASS:  return std::make_shared<ColumnUInt64>();
                    default:
                        mexErrMsgIdAndTxt("ClickHouse:unsupportedType",
                            "Unsupported Array inner type %d for field '%s'.",
                            (int)inner_class, fname);
                        return nullptr;
                    }
                };

                auto arr_col = std::make_shared<ColumnArray>(make_inner_col());

                for (size_t i = 0; i < n; i++) {
                    const mxArray* cell = mxGetCell(fd, i);
                    auto inner = make_inner_col();
                    if (cell && !mxIsEmpty(cell)) {
                        size_t m = mxGetNumberOfElements(cell);
                        switch (inner_class) {
                        case mxDOUBLE_CLASS: { auto ic=inner->As<ColumnFloat64>(); double*   d=mxGetDoubles(cell);  for(size_t j=0;j<m;j++) ic->Append(d[j]); break; }
                        case mxSINGLE_CLASS: { auto ic=inner->As<ColumnFloat32>(); float*    d=mxGetSingles(cell); for(size_t j=0;j<m;j++) ic->Append(d[j]); break; }
                        case mxINT8_CLASS:   { auto ic=inner->As<ColumnInt8>();    int8_T*   d=mxGetInt8s(cell);   for(size_t j=0;j<m;j++) ic->Append(d[j]); break; }
                        case mxINT16_CLASS:  { auto ic=inner->As<ColumnInt16>();   int16_T*  d=mxGetInt16s(cell);  for(size_t j=0;j<m;j++) ic->Append(d[j]); break; }
                        case mxINT32_CLASS:  { auto ic=inner->As<ColumnInt32>();   int32_T*  d=mxGetInt32s(cell);  for(size_t j=0;j<m;j++) ic->Append(d[j]); break; }
                        case mxINT64_CLASS:  { auto ic=inner->As<ColumnInt64>();   int64_T*  d=mxGetInt64s(cell);  for(size_t j=0;j<m;j++) ic->Append(d[j]); break; }
                        case mxUINT8_CLASS:  { auto ic=inner->As<ColumnUInt8>();   uint8_T*  d=mxGetUint8s(cell);  for(size_t j=0;j<m;j++) ic->Append(d[j]); break; }
                        case mxUINT16_CLASS: { auto ic=inner->As<ColumnUInt16>();  uint16_T* d=mxGetUint16s(cell); for(size_t j=0;j<m;j++) ic->Append(d[j]); break; }
                        case mxUINT32_CLASS: { auto ic=inner->As<ColumnUInt32>();  uint32_T* d=mxGetUint32s(cell); for(size_t j=0;j<m;j++) ic->Append(d[j]); break; }
                        case mxUINT64_CLASS: { auto ic=inner->As<ColumnUInt64>();  uint64_T* d=mxGetUint64s(cell); for(size_t j=0;j<m;j++) ic->Append(d[j]); break; }
                        default: break;
                        }
                    }
                    arr_col->AppendAsColumn(inner);
                }
                block.AppendColumn(fname, arr_col);
            } else {
                // All cells are empty — insert as empty Float64 array column
                auto arr_col = std::make_shared<ColumnArray>(std::make_shared<ColumnFloat64>());
                for (size_t i = 0; i < n; i++) {
                    arr_col->AppendAsColumn(std::make_shared<ColumnFloat64>());
                }
                block.AppendColumn(fname, arr_col);
            }
            break;
        }
        default:
            mexErrMsgIdAndTxt("ClickHouse:unsupportedType",
                "Unsupported MATLAB class %d for field '%s'.", (int)cid, fname);
        }
    }

    try {
        client->Insert(table_name, block);
    } catch (const std::exception& e) {
        mexErrMsgIdAndTxt("ClickHouse:insertError", "%s", e.what());
    }
}

// ── dispatcher ───────────────────────────────────────────────────────────────
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 1 || !mxIsChar(prhs[0]))
        mexErrMsgIdAndTxt("ClickHouse:badArgs", "First argument must be a command string.");

    if (!g_exit_registered) {
        mexAtExit(cleanup_all);
        g_exit_registered = true;
    }

    char* cmd_c = mxArrayToUTF8String(prhs[0]);
    if (!cmd_c) mexErrMsgIdAndTxt("ClickHouse:badArgs", "Failed to read command string.");
    std::string cmd(cmd_c);
    mxFree(cmd_c);

    if      (cmd == "connect") cmd_connect(nlhs, plhs, nrhs, prhs);
    else if (cmd == "ping")    cmd_ping   (nlhs, plhs, nrhs, prhs);
    else if (cmd == "query")   cmd_query  (nlhs, plhs, nrhs, prhs);
    else if (cmd == "insert")  cmd_insert (nlhs, plhs, nrhs, prhs);
    else if (cmd == "delete")  cmd_delete (nlhs, plhs, nrhs, prhs);
    else
        mexErrMsgIdAndTxt("ClickHouse:unknownCommand", "Unknown command: %s", cmd.c_str());
}

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
#include <clickhouse/columns/date.h>
#include <clickhouse/columns/lowcardinality.h>
#include <clickhouse/columns/ip4.h>
#include <clickhouse/columns/ip6.h>
#include <clickhouse/columns/enum.h>
#include <clickhouse/columns/decimal.h>
#include <clickhouse/types/types.h>
#include <vector>
#include <unordered_set>
#include <cstring>

#ifndef CLICKHOUSE_MATLAB_VERSION
#error "CLICKHOUSE_MATLAB_VERSION not defined by build system — set via CMake"
#endif

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

    // maxRetries drives ping-before-query + protocol-level send retries.
    // 0 = fail-fast (no ping, single attempt). Default 3 matches the
    // ClickHouseClient.m default; ClickHouseClient always sets the field.
    int maxRetries = 3;
    if (nrhs > 5 && !mxIsEmpty(prhs[5])) {
        mxArray* mr = mxGetField(prhs[5], 0, "maxRetries");
        if (mr && !mxIsEmpty(mr)) {
            maxRetries = static_cast<int>(mxGetScalar(mr));
            if (maxRetries < 0) maxRetries = 0;
        }
    }

    ClientOptions opts;
    opts.SetHost(host)
        .SetPort(static_cast<uint16_t>(port))
        .SetUser(user)
        .SetPassword(pass)
        .SetPingBeforeQuery(maxRetries > 0)
        .SetSendRetries(maxRetries > 0 ? static_cast<unsigned>(maxRetries) : 1u)
        .SetRetryTimeout(std::chrono::seconds(2))
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

        // compression: client-level native-protocol block compression. The
        // MATLAB Compression enum is int8-backed with the same underlying
        // values as clickhouse-cpp's CompressionMethod, so this is a direct
        // cast. ClickHouseClient injects a default and validates the type, so
        // an out-of-range value never reaches here.
        mxArray* comp = mxGetField(opt, 0, "compression");
        if (comp && !mxIsEmpty(comp)) {
            int m = static_cast<int>(mxGetScalar(comp));
            opts.SetCompressionMethod(static_cast<CompressionMethod>(m));
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

// ── reconnect ────────────────────────────────────────────────────────────────
static void cmd_reconnect(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 2) mexErrMsgIdAndTxt("ClickHouse:badArgs", "reconnect requires a connection handle.");
    Client* client = get_client(prhs[1]);
    try {
        client->ResetConnection();
    } catch (const std::exception& e) {
        mexErrMsgIdAndTxt("ClickHouse:reconnectError", "%s", e.what());
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

// ── version ──────────────────────────────────────────────────────────────────
static void cmd_version(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    // No args required. The version is embedded at compile time via CMake.
    (void)nlhs; (void)nrhs; (void)prhs;
    plhs[0] = mxCreateString(CLICKHOUSE_MATLAB_VERSION);
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
            case Type::DateTime64: empty = mxCreateNumericMatrix(0,1,mxDOUBLE_CLASS,mxREAL); break;
            case Type::Date:     empty = mxCreateNumericMatrix(0,1,mxDOUBLE_CLASS,mxREAL); break;
            case Type::Date32:   empty = mxCreateNumericMatrix(0,1,mxDOUBLE_CLASS,mxREAL); break;
            case Type::DateTime: empty = mxCreateNumericMatrix(0,1,mxDOUBLE_CLASS,mxREAL); break;
            case Type::LowCardinality: {
                auto inner_tc2 = it.Type()->As<LowCardinalityType>()->GetNestedType()->GetCode();
                if (inner_tc2 == Type::Float32)
                    empty = mxCreateNumericMatrix(0,1,mxSINGLE_CLASS,mxREAL);
                else if (inner_tc2 == Type::String || inner_tc2 == Type::FixedString)
                    empty = mxCreateCellMatrix(0,1);
                else
                    empty = mxCreateNumericMatrix(0,1,mxDOUBLE_CLASS,mxREAL);
                break;
            }
            case Type::FixedString: empty = mxCreateCellMatrix(0,1); break;
            case Type::Enum8:       empty = mxCreateCellMatrix(0,1); break;
            case Type::Enum16:      empty = mxCreateCellMatrix(0,1); break;
            case Type::IPv4:        empty = mxCreateCellMatrix(0,1); break;
            case Type::IPv6:        empty = mxCreateCellMatrix(0,1); break;
            case Type::Decimal:
            case Type::Decimal32:
            case Type::Decimal64:
            case Type::Decimal128:  empty = mxCreateNumericMatrix(0,1,mxDOUBLE_CLASS,mxREAL); break;
            case Type::String:   empty = mxCreateCellMatrix(0,1); break;
            case Type::Array:    empty = mxCreateCellMatrix(0,1); break;
            case Type::Nullable: {
                // Use inner type to pick the right empty array
                auto inner_tc = it.Type()->As<NullableType>()->GetNestedType()->GetCode();
                if (inner_tc == Type::Float32)
                    empty = mxCreateNumericMatrix(0,1,mxSINGLE_CLASS,mxREAL);
                else if (inner_tc == Type::String || inner_tc == Type::FixedString ||
                         inner_tc == Type::IPv4 || inner_tc == Type::IPv6 ||
                         inner_tc == Type::Enum8 || inner_tc == Type::Enum16)
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
        case Type::Date: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
            double* dst = mxGetDoubles(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnDate>();
                for (size_t r = 0; r < col->Size(); r++)
                    dst[off+r] = (double)col->RawAt(r) * 86400.0;
                off += col->Size();
            }
            break;
        }
        case Type::Date32: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
            double* dst = mxGetDoubles(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnDate32>();
                for (size_t r = 0; r < col->Size(); r++)
                    dst[off+r] = (double)col->RawAt(r) * 86400.0;
                off += col->Size();
            }
            break;
        }
        case Type::DateTime: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
            double* dst = mxGetDoubles(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnDateTime>();
                for (size_t r = 0; r < col->Size(); r++)
                    dst[off+r] = (double)col->RawAt(r);
                off += col->Size();
            }
            break;
        }
        case Type::DateTime64: {
            // Return as double: POSIX seconds (ticks / 10^precision)
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
            double* dst = mxGetDoubles(col_arr);
            size_t prec = blocks[0][ci]->As<ColumnDateTime64>()->GetPrecision();
            double scale = std::pow(10.0, (double)prec);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnDateTime64>();
                for (size_t r = 0; r < col->Size(); r++)
                    dst[off+r] = (double)(*col)[r] / scale;
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
        case Type::FixedString: {
            col_arr = mxCreateCellMatrix(total_rows, 1);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnFixedString>();
                for (size_t r = 0; r < col->Size(); r++) {
                    auto sv = col->At(r);
                    mxSetCell(col_arr, off+r, mxCreateString(std::string(sv).c_str()));
                }
                off += col->Size();
            }
            break;
        }
        case Type::Enum8: {
            col_arr = mxCreateCellMatrix(total_rows, 1);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnEnum8>();
                for (size_t r = 0; r < col->Size(); r++) {
                    auto sv = col->NameAt(r);
                    mxSetCell(col_arr, off+r, mxCreateString(std::string(sv).c_str()));
                }
                off += col->Size();
            }
            break;
        }
        case Type::Enum16: {
            col_arr = mxCreateCellMatrix(total_rows, 1);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnEnum16>();
                for (size_t r = 0; r < col->Size(); r++) {
                    auto sv = col->NameAt(r);
                    mxSetCell(col_arr, off+r, mxCreateString(std::string(sv).c_str()));
                }
                off += col->Size();
            }
            break;
        }
        case Type::IPv4: {
            col_arr = mxCreateCellMatrix(total_rows, 1);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnIPv4>();
                for (size_t r = 0; r < col->Size(); r++)
                    mxSetCell(col_arr, off+r, mxCreateString(col->AsString(r).c_str()));
                off += col->Size();
            }
            break;
        }
        case Type::IPv6: {
            col_arr = mxCreateCellMatrix(total_rows, 1);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnIPv6>();
                for (size_t r = 0; r < col->Size(); r++)
                    mxSetCell(col_arr, off+r, mxCreateString(col->AsString(r).c_str()));
                off += col->Size();
            }
            break;
        }
        case Type::Decimal:
        case Type::Decimal32:
        case Type::Decimal64:
        case Type::Decimal128: {
            col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
            double* dst = mxGetDoubles(col_arr);
            size_t off = 0;
            for (const auto& blk : blocks) {
                auto col = blk[ci]->As<ColumnDecimal>();
                double scale_div = std::pow(10.0, (double)col->GetScale());
                for (size_t r = 0; r < col->Size(); r++)
                    dst[off+r] = static_cast<double>(col->At(r)) / scale_div;
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
                    } else if (inner_code == Type::Int8) {
                        row_arr = mxCreateNumericMatrix(arr_rows, inner_n, mxINT8_CLASS, mxREAL);
                        int8_T* d = mxGetInt8s(row_arr);
                        auto ic = inner->As<ColumnInt8>();
                        for (size_t j = 0; j < inner_n; j++) d[j] = (*ic)[j];
                    } else if (inner_code == Type::Int16) {
                        row_arr = mxCreateNumericMatrix(arr_rows, inner_n, mxINT16_CLASS, mxREAL);
                        int16_T* d = mxGetInt16s(row_arr);
                        auto ic = inner->As<ColumnInt16>();
                        for (size_t j = 0; j < inner_n; j++) d[j] = (*ic)[j];
                    } else if (inner_code == Type::UInt8) {
                        row_arr = mxCreateNumericMatrix(arr_rows, inner_n, mxUINT8_CLASS, mxREAL);
                        uint8_T* d = mxGetUint8s(row_arr);
                        auto ic = inner->As<ColumnUInt8>();
                        for (size_t j = 0; j < inner_n; j++) d[j] = (*ic)[j];
                    } else if (inner_code == Type::UInt16) {
                        row_arr = mxCreateNumericMatrix(arr_rows, inner_n, mxUINT16_CLASS, mxREAL);
                        uint16_T* d = mxGetUint16s(row_arr);
                        auto ic = inner->As<ColumnUInt16>();
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
        case Type::LowCardinality: {
            auto lc0 = blocks[0][ci]->As<ColumnLowCardinality>();
            Type::Code inner_lc = lc0->GetNestedType()->GetCode();
            switch (inner_lc) {
            case Type::String: {
                col_arr = mxCreateCellMatrix(total_rows, 1);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnString>>();
                    for (size_t r = 0; r < lc->Size(); r++) {
                        auto sv = lc->At(r);
                        mxSetCell(col_arr, off+r, mxCreateString(std::string(sv).c_str()));
                    }
                    off += lc->Size();
                }
                break;
            }
            case Type::FixedString: {
                col_arr = mxCreateCellMatrix(total_rows, 1);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnFixedString>>();
                    for (size_t r = 0; r < lc->Size(); r++) {
                        auto sv = lc->At(r);
                        mxSetCell(col_arr, off+r, mxCreateString(std::string(sv).c_str()));
                    }
                    off += lc->Size();
                }
                break;
            }
            case Type::Float64: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
                double* dst = mxGetDoubles(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnFloat64>>();
                    for (size_t r = 0; r < lc->Size(); r++) dst[off+r] = lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::Float32: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxSINGLE_CLASS, mxREAL);
                float* dst = mxGetSingles(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnFloat32>>();
                    for (size_t r = 0; r < lc->Size(); r++) dst[off+r] = lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::Int8: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxINT8_CLASS, mxREAL);
                int8_T* dst = mxGetInt8s(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnInt8>>();
                    for (size_t r = 0; r < lc->Size(); r++) dst[off+r] = lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::Int16: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxINT16_CLASS, mxREAL);
                int16_T* dst = mxGetInt16s(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnInt16>>();
                    for (size_t r = 0; r < lc->Size(); r++) dst[off+r] = lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::Int32: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxINT32_CLASS, mxREAL);
                int32_T* dst = mxGetInt32s(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnInt32>>();
                    for (size_t r = 0; r < lc->Size(); r++) dst[off+r] = lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::Int64: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxINT64_CLASS, mxREAL);
                int64_T* dst = mxGetInt64s(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnInt64>>();
                    for (size_t r = 0; r < lc->Size(); r++) dst[off+r] = lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::UInt8: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxUINT8_CLASS, mxREAL);
                uint8_T* dst = mxGetUint8s(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnUInt8>>();
                    for (size_t r = 0; r < lc->Size(); r++) dst[off+r] = lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::UInt16: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxUINT16_CLASS, mxREAL);
                uint16_T* dst = mxGetUint16s(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnUInt16>>();
                    for (size_t r = 0; r < lc->Size(); r++) dst[off+r] = lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::UInt32: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxUINT32_CLASS, mxREAL);
                uint32_T* dst = mxGetUint32s(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnUInt32>>();
                    for (size_t r = 0; r < lc->Size(); r++) dst[off+r] = lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::UInt64: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxUINT64_CLASS, mxREAL);
                uint64_T* dst = mxGetUint64s(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnUInt64>>();
                    for (size_t r = 0; r < lc->Size(); r++) dst[off+r] = lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::Date: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
                double* dst = mxGetDoubles(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnDate>>();
                    for (size_t r = 0; r < lc->Size(); r++)
                        dst[off+r] = (double)lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            case Type::DateTime: {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
                double* dst = mxGetDoubles(col_arr);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto lc = blk[ci]->As<ColumnLowCardinalityT<ColumnDateTime>>();
                    for (size_t r = 0; r < lc->Size(); r++)
                        dst[off+r] = (double)lc->At(r);
                    off += lc->Size();
                }
                break;
            }
            default:
                mexErrMsgIdAndTxt("ClickHouse:unsupportedType",
                    "Unsupported LowCardinality inner type %d for column '%s'.",
                    (int)inner_lc, names[ci].c_str());
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
                // Nullable(String): cell of chars; null → false (logical sentinel)
                col_arr = mxCreateCellMatrix(total_rows, 1);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnString>();
                    for (size_t r = 0; r < nc->Size(); r++) {
                        if (nc->IsNull(r))
                            mxSetCell(col_arr, off+r, mxCreateLogicalScalar(false));
                        else {
                            std::string_view sv = ic->At(r);
                            mxSetCell(col_arr, off+r, mxCreateString(std::string(sv).c_str()));
                        }
                    }
                    off += nc->Size();
                }
            } else if (inner_tc == Type::DateTime64) {
                // Nullable(DateTime64): double (POSIX seconds), NaN for nulls
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
                double* dst = mxGetDoubles(col_arr);
                for (size_t r = 0; r < total_rows; r++) dst[r] = kNaN;
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnDateTime64>();
                    size_t prec = ic->GetPrecision();
                    double scale = std::pow(10.0, (double)prec);
                    for (size_t r = 0; r < nc->Size(); r++)
                        if (!nc->IsNull(r)) dst[off+r] = (double)(*ic)[r] / scale;
                    off += nc->Size();
                }
            } else if (inner_tc == Type::Date) {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
                double* dst = mxGetDoubles(col_arr);
                for (size_t r = 0; r < total_rows; r++) dst[r] = kNaN;
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnDate>();
                    for (size_t r = 0; r < nc->Size(); r++)
                        if (!nc->IsNull(r)) dst[off+r] = (double)ic->RawAt(r) * 86400.0;
                    off += nc->Size();
                }
            } else if (inner_tc == Type::Date32) {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
                double* dst = mxGetDoubles(col_arr);
                for (size_t r = 0; r < total_rows; r++) dst[r] = kNaN;
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnDate32>();
                    for (size_t r = 0; r < nc->Size(); r++)
                        if (!nc->IsNull(r)) dst[off+r] = (double)ic->RawAt(r) * 86400.0;
                    off += nc->Size();
                }
            } else if (inner_tc == Type::DateTime) {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
                double* dst = mxGetDoubles(col_arr);
                for (size_t r = 0; r < total_rows; r++) dst[r] = kNaN;
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnDateTime>();
                    for (size_t r = 0; r < nc->Size(); r++)
                        if (!nc->IsNull(r)) dst[off+r] = (double)ic->RawAt(r);
                    off += nc->Size();
                }
            } else if (inner_tc == Type::FixedString) {
                col_arr = mxCreateCellMatrix(total_rows, 1);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnFixedString>();
                    for (size_t r = 0; r < nc->Size(); r++) {
                        if (nc->IsNull(r))
                            mxSetCell(col_arr, off+r, mxCreateLogicalScalar(false));
                        else {
                            auto sv = ic->At(r);
                            mxSetCell(col_arr, off+r, mxCreateString(std::string(sv).c_str()));
                        }
                    }
                    off += nc->Size();
                }
            } else if (inner_tc == Type::Enum8) {
                col_arr = mxCreateCellMatrix(total_rows, 1);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnEnum8>();
                    for (size_t r = 0; r < nc->Size(); r++) {
                        if (nc->IsNull(r))
                            mxSetCell(col_arr, off+r, mxCreateLogicalScalar(false));
                        else {
                            auto sv = ic->NameAt(r);
                            mxSetCell(col_arr, off+r, mxCreateString(std::string(sv).c_str()));
                        }
                    }
                    off += nc->Size();
                }
            } else if (inner_tc == Type::Enum16) {
                col_arr = mxCreateCellMatrix(total_rows, 1);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnEnum16>();
                    for (size_t r = 0; r < nc->Size(); r++) {
                        if (nc->IsNull(r))
                            mxSetCell(col_arr, off+r, mxCreateLogicalScalar(false));
                        else {
                            auto sv = ic->NameAt(r);
                            mxSetCell(col_arr, off+r, mxCreateString(std::string(sv).c_str()));
                        }
                    }
                    off += nc->Size();
                }
            } else if (inner_tc == Type::IPv4) {
                col_arr = mxCreateCellMatrix(total_rows, 1);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnIPv4>();
                    for (size_t r = 0; r < nc->Size(); r++) {
                        if (nc->IsNull(r))
                            mxSetCell(col_arr, off+r, mxCreateLogicalScalar(false));
                        else
                            mxSetCell(col_arr, off+r, mxCreateString(ic->AsString(r).c_str()));
                    }
                    off += nc->Size();
                }
            } else if (inner_tc == Type::IPv6) {
                col_arr = mxCreateCellMatrix(total_rows, 1);
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnIPv6>();
                    for (size_t r = 0; r < nc->Size(); r++) {
                        if (nc->IsNull(r))
                            mxSetCell(col_arr, off+r, mxCreateLogicalScalar(false));
                        else
                            mxSetCell(col_arr, off+r, mxCreateString(ic->AsString(r).c_str()));
                    }
                    off += nc->Size();
                }
            } else if (inner_tc == Type::Decimal || inner_tc == Type::Decimal32 ||
                       inner_tc == Type::Decimal64 || inner_tc == Type::Decimal128) {
                col_arr = mxCreateNumericMatrix(total_rows, 1, mxDOUBLE_CLASS, mxREAL);
                double* dst = mxGetDoubles(col_arr);
                for (size_t r = 0; r < total_rows; r++) dst[r] = kNaN;
                size_t off = 0;
                for (const auto& blk : blocks) {
                    auto nc = blk[ci]->As<ColumnNullable>();
                    auto ic = nc->Nested()->As<ColumnDecimal>();
                    double scale_div = std::pow(10.0, (double)ic->GetScale());
                    for (size_t r = 0; r < nc->Size(); r++)
                        if (!nc->IsNull(r)) dst[off+r] = static_cast<double>((*ic)[r]) / scale_div;
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
    // Strict nullability (set by ClickHouseClient.insert when it has the table schema):
    // a NaN then makes a float column Nullable only if the schema says it is Nullable
    // (ch_nullable_hint); in a plain Float32/Float64 column NaN is stored as a value.
    bool strict_nullable = false;
    int strict_fi = mxGetFieldNumber(s, "ch_strict_nullable");
    if (strict_fi >= 0) {
        const mxArray* sv = mxGetFieldByNumber(s, 0, strict_fi);
        strict_nullable = sv && (mxIsLogicalScalarTrue(sv) || (mxIsNumeric(sv) && mxGetNumberOfElements(sv) == 1 && mxGetScalar(sv) != 0));
    }
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

    // Extract DateTime64 precision hints: struct mapping col_name → precision.
    // Populated by ClickHouseClient.m from DESCRIBE TABLE for DateTime64 columns.
    std::unordered_map<std::string, int> datetime64_cols;
    int dt64_hint_fi = mxGetFieldNumber(s, "ch_datetime64_hint");
    if (dt64_hint_fi >= 0) {
        const mxArray* dt64_hint = mxGetFieldByNumber(s, 0, dt64_hint_fi);
        if (dt64_hint && mxIsStruct(dt64_hint)) {
            int nf_h = mxGetNumberOfFields(dt64_hint);
            for (int i = 0; i < nf_h; i++) {
                const char* fn_h = mxGetFieldNameByNumber(dt64_hint, i);
                const mxArray* val = mxGetFieldByNumber(dt64_hint, 0, i);
                if (fn_h && val && mxIsNumeric(val))
                    datetime64_cols[fn_h] = (int)mxGetScalar(val);
            }
        }
    }

    // Date columns hint: col_name → "Date" | "Date32" | "DateTime"
    // Used to insert MATLAB double (POSIX seconds) into Date/DateTime columns.
    std::unordered_map<std::string, std::string> date_type_cols;
    int date_hint_fi = mxGetFieldNumber(s, "ch_date_type_hint");
    if (date_hint_fi >= 0) {
        const mxArray* dh = mxGetFieldByNumber(s, 0, date_hint_fi);
        if (dh && mxIsStruct(dh)) {
            int nf_d = mxGetNumberOfFields(dh);
            for (int i = 0; i < nf_d; i++) {
                const char* fn_d = mxGetFieldNameByNumber(dh, i);
                const mxArray* val = mxGetFieldByNumber(dh, 0, i);
                if (fn_d && val && mxIsChar(val)) {
                    char* type_str = mxArrayToUTF8String(val);
                    if (type_str) { date_type_cols[fn_d] = type_str; mxFree(type_str); }
                }
            }
        }
    }

    // Nullable integer hint: col_name → int type string (e.g. "Int32", "UInt64")
    // When MATLAB sends double (with NaN), insert as Nullable(IntType).
    std::unordered_map<std::string, std::string> nullable_int_cols;
    int ni_hint_fi = mxGetFieldNumber(s, "ch_nullable_int_hint");
    if (ni_hint_fi >= 0) {
        const mxArray* nih = mxGetFieldByNumber(s, 0, ni_hint_fi);
        if (nih && mxIsStruct(nih)) {
            int nf_ni = mxGetNumberOfFields(nih);
            for (int i = 0; i < nf_ni; i++) {
                const char* fn_ni = mxGetFieldNameByNumber(nih, i);
                const mxArray* val = mxGetFieldByNumber(nih, 0, i);
                if (fn_ni && val && mxIsChar(val)) {
                    char* type_str = mxArrayToUTF8String(val);
                    if (type_str) { nullable_int_cols[fn_ni] = type_str; mxFree(type_str); }
                }
            }
        }
    }

    // LowCardinality hint: col_name → inner type string (e.g. "String", "UInt32")
    std::unordered_map<std::string, std::string> lc_cols;
    int lc_hint_fi = mxGetFieldNumber(s, "ch_lc_hint");
    if (lc_hint_fi >= 0) {
        const mxArray* lch = mxGetFieldByNumber(s, 0, lc_hint_fi);
        if (lch && mxIsStruct(lch)) {
            int nf_lc = mxGetNumberOfFields(lch);
            for (int i = 0; i < nf_lc; i++) {
                const char* fn_lc = mxGetFieldNameByNumber(lch, i);
                const mxArray* val = mxGetFieldByNumber(lch, 0, i);
                if (fn_lc && val && mxIsChar(val)) {
                    char* type_str = mxArrayToUTF8String(val);
                    if (type_str) { lc_cols[fn_lc] = type_str; mxFree(type_str); }
                }
            }
        }
    }

    // IPv4/IPv6 hints: cell arrays of column names
    std::unordered_set<std::string> ipv4_cols;
    int ipv4_hint_fi = mxGetFieldNumber(s, "ch_ipv4_hint");
    if (ipv4_hint_fi >= 0) {
        const mxArray* hint = mxGetFieldByNumber(s, 0, ipv4_hint_fi);
        if (hint && mxIsCell(hint)) {
            for (size_t i = 0; i < mxGetNumberOfElements(hint); i++) {
                const mxArray* e = mxGetCell(hint, i);
                if (e && mxIsChar(e)) {
                    char* c = mxArrayToUTF8String(e);
                    if (c) { ipv4_cols.insert(c); mxFree(c); }
                }
            }
        }
    }
    std::unordered_set<std::string> ipv6_cols;
    int ipv6_hint_fi = mxGetFieldNumber(s, "ch_ipv6_hint");
    if (ipv6_hint_fi >= 0) {
        const mxArray* hint = mxGetFieldByNumber(s, 0, ipv6_hint_fi);
        if (hint && mxIsCell(hint)) {
            for (size_t i = 0; i < mxGetNumberOfElements(hint); i++) {
                const mxArray* e = mxGetCell(hint, i);
                if (e && mxIsChar(e)) {
                    char* c = mxArrayToUTF8String(e);
                    if (c) { ipv6_cols.insert(c); mxFree(c); }
                }
            }
        }
    }
    // Enum hint: col_name → full type string e.g. "Enum8('a'=1,'b'=2)"
    std::unordered_map<std::string, std::string> enum_type_cols;
    int enum_hint_fi = mxGetFieldNumber(s, "ch_enum_hint");
    if (enum_hint_fi >= 0) {
        const mxArray* eh = mxGetFieldByNumber(s, 0, enum_hint_fi);
        if (eh && mxIsStruct(eh)) {
            for (int i = 0; i < mxGetNumberOfFields(eh); i++) {
                const char* fn = mxGetFieldNameByNumber(eh, i);
                const mxArray* val = mxGetFieldByNumber(eh, 0, i);
                if (fn && val && mxIsChar(val)) {
                    char* ts = mxArrayToUTF8String(val);
                    if (ts) { enum_type_cols[fn] = ts; mxFree(ts); }
                }
            }
        }
    }
    // FixedString hint: col_name → N (fixed length)
    std::unordered_map<std::string, size_t> fixedstring_cols;
    int fs_hint_fi = mxGetFieldNumber(s, "ch_fixedstring_hint");
    if (fs_hint_fi >= 0) {
        const mxArray* fsh = mxGetFieldByNumber(s, 0, fs_hint_fi);
        if (fsh && mxIsStruct(fsh)) {
            for (int i = 0; i < mxGetNumberOfFields(fsh); i++) {
                const char* fn = mxGetFieldNameByNumber(fsh, i);
                const mxArray* val = mxGetFieldByNumber(fsh, 0, i);
                if (fn && val && mxIsNumeric(val))
                    fixedstring_cols[fn] = (size_t)mxGetScalar(val);
            }
        }
    }
    // Decimal hint: col_name → [precision, scale] stored as 2-element double array
    std::unordered_map<std::string, std::pair<int,int>> decimal_cols_hint;
    int dec_hint_fi = mxGetFieldNumber(s, "ch_decimal_hint");
    if (dec_hint_fi >= 0) {
        const mxArray* dh = mxGetFieldByNumber(s, 0, dec_hint_fi);
        if (dh && mxIsStruct(dh)) {
            for (int i = 0; i < mxGetNumberOfFields(dh); i++) {
                const char* fn = mxGetFieldNameByNumber(dh, i);
                const mxArray* val = mxGetFieldByNumber(dh, 0, i);
                if (fn && val && mxIsNumeric(val) && mxGetNumberOfElements(val) >= 2) {
                    double* v = mxGetDoubles(val);
                    decimal_cols_hint[fn] = {(int)v[0], (int)v[1]};
                }
            }
        }
    }

    Block block;

    for (int fi = 0; fi < nfields; fi++) {
        const char* fname  = mxGetFieldNameByNumber(s, fi);
        if (std::string(fname) == "ch_nullable_hint" ||
            std::string(fname) == "ch_datetime64_hint" ||
            std::string(fname) == "ch_date_type_hint" ||
            std::string(fname) == "ch_nullable_int_hint" ||
            std::string(fname) == "ch_lc_hint" ||
            std::string(fname) == "ch_ipv4_hint" ||
            std::string(fname) == "ch_ipv6_hint" ||
            std::string(fname) == "ch_enum_hint" ||
            std::string(fname) == "ch_fixedstring_hint" ||
            std::string(fname) == "ch_decimal_hint" ||
            std::string(fname) == "ch_strict_nullable") continue;
        const mxArray* fd  = mxGetFieldByNumber(s, 0, fi);
        if (!fd) continue;
        mxClassID cid = mxGetClassID(fd);
        size_t n      = mxGetNumberOfElements(fd);

        switch (cid) {
        case mxDOUBLE_CLASS: {
            double* data = mxGetDoubles(fd);
            // LowCardinality(Float64): check before NaN scan
            if (lc_cols.count(fname) && lc_cols.at(fname) == "Float64") {
                auto lc = std::make_shared<ColumnLowCardinalityT<ColumnFloat64>>();
                lc->Reserve(n);
                for (size_t i = 0; i < n; i++) lc->Append(data[i]);
                block.AppendColumn(fname, lc);
                break;
            }
            bool has_nan = false;
            for (size_t i = 0; i < n && !has_nan; i++) has_nan = std::isnan(data[i]);
            bool force_nullable = nullable_cols.count(fname) > 0;
            auto dt64_it = datetime64_cols.find(fname);
            auto date_it = date_type_cols.find(fname);
            auto ni_it   = nullable_int_cols.find(fname);
            if (dt64_it != datetime64_cols.end()) {
                // DateTime64: double (POSIX seconds) → int64 ticks
                int prec = dt64_it->second;
                double scale = std::pow(10.0, prec);
                if (has_nan || force_nullable) {
                    auto inner = std::make_shared<ColumnDateTime64>(prec);
                    auto nulls = std::make_shared<ColumnUInt8>();
                    inner->Reserve(n);
                    nulls->Reserve(n);
                    for (size_t i = 0; i < n; i++) {
                        bool is_null = std::isnan(data[i]);
                        nulls->Append(is_null ? 1 : 0);
                        inner->Append(is_null ? 0LL : (int64_t)std::round(data[i] * scale));
                    }
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else {
                    auto col = std::make_shared<ColumnDateTime64>(prec);
                    col->Reserve(n);
                    for (size_t i = 0; i < n; i++)
                        col->Append((int64_t)std::round(data[i] * scale));
                    block.AppendColumn(fname, col);
                }
            } else if (date_it != date_type_cols.end()) {
                const std::string& dt = date_it->second;
                if (dt == "Date") {
                    if (has_nan || force_nullable) {
                        auto inner = std::make_shared<ColumnDate>();
                        auto nulls = std::make_shared<ColumnUInt8>();
                        inner->Reserve(n);
                        nulls->Reserve(n);
                        for (size_t i = 0; i < n; i++) {
                            bool is_null = std::isnan(data[i]);
                            nulls->Append(is_null ? 1 : 0);
                            inner->AppendRaw(is_null ? 0 : (uint16_t)(data[i] / 86400.0));
                        }
                        block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                    } else {
                        auto col = std::make_shared<ColumnDate>();
                        col->Reserve(n);
                        for (size_t i = 0; i < n; i++)
                            col->AppendRaw((uint16_t)(data[i] / 86400.0));
                        block.AppendColumn(fname, col);
                    }
                } else if (dt == "Date32") {
                    if (has_nan || force_nullable) {
                        auto inner = std::make_shared<ColumnDate32>();
                        auto nulls = std::make_shared<ColumnUInt8>();
                        inner->Reserve(n);
                        nulls->Reserve(n);
                        for (size_t i = 0; i < n; i++) {
                            bool is_null = std::isnan(data[i]);
                            nulls->Append(is_null ? 1 : 0);
                            inner->AppendRaw(is_null ? 0 : (int32_t)(data[i] / 86400.0));
                        }
                        block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                    } else {
                        auto col = std::make_shared<ColumnDate32>();
                        col->Reserve(n);
                        for (size_t i = 0; i < n; i++)
                            col->AppendRaw((int32_t)(data[i] / 86400.0));
                        block.AppendColumn(fname, col);
                    }
                } else { // DateTime
                    if (has_nan || force_nullable) {
                        auto inner = std::make_shared<ColumnDateTime>();
                        auto nulls = std::make_shared<ColumnUInt8>();
                        inner->Reserve(n);
                        nulls->Reserve(n);
                        for (size_t i = 0; i < n; i++) {
                            bool is_null = std::isnan(data[i]);
                            nulls->Append(is_null ? 1 : 0);
                            inner->Append(is_null ? 0 : (uint32_t)data[i]);
                        }
                        block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                    } else {
                        auto col = std::make_shared<ColumnDateTime>();
                        col->Reserve(n);
                        for (size_t i = 0; i < n; i++)
                            col->Append((uint32_t)data[i]);
                        block.AppendColumn(fname, col);
                    }
                }
            } else if (decimal_cols_hint.count(fname)) {
                auto [prec, scale] = decimal_cols_hint.at(fname);
                double scale_factor = std::pow(10.0, scale);
                if (has_nan || force_nullable) {
                    auto inner = std::make_shared<ColumnDecimal>(prec, scale);
                    auto nulls = std::make_shared<ColumnUInt8>();
                    inner->Reserve(n);
                    nulls->Reserve(n);
                    for (size_t i = 0; i < n; i++) {
                        bool is_null = std::isnan(data[i]);
                        nulls->Append(is_null ? 1 : 0);
                        inner->Append(is_null ? Int128(0) : static_cast<Int128>(std::round(data[i] * scale_factor)));
                    }
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else {
                    auto col = std::make_shared<ColumnDecimal>(prec, scale);
                    col->Reserve(n);
                    for (size_t i = 0; i < n; i++)
                        col->Append(static_cast<Int128>(std::round(data[i] * scale_factor)));
                    block.AppendColumn(fname, col);
                }
            } else if (ni_it != nullable_int_cols.end()) {
                // Nullable integer: MATLAB double (NaN=NULL) → Nullable(IntType)
                const std::string& int_type = ni_it->second;
                auto nulls = std::make_shared<ColumnUInt8>();
                nulls->Reserve(n);
                for (size_t i = 0; i < n; i++) nulls->Append(std::isnan(data[i]) ? 1 : 0);
                if (int_type == "Int8") {
                    auto inner = std::make_shared<ColumnInt8>();
                    inner->Reserve(n);
                    for (size_t i = 0; i < n; i++) inner->Append(std::isnan(data[i]) ? 0 : (int8_t)data[i]);
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else if (int_type == "Int16") {
                    auto inner = std::make_shared<ColumnInt16>();
                    inner->Reserve(n);
                    for (size_t i = 0; i < n; i++) inner->Append(std::isnan(data[i]) ? 0 : (int16_t)data[i]);
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else if (int_type == "Int32") {
                    auto inner = std::make_shared<ColumnInt32>();
                    inner->Reserve(n);
                    for (size_t i = 0; i < n; i++) inner->Append(std::isnan(data[i]) ? 0 : (int32_t)data[i]);
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else if (int_type == "Int64") {
                    auto inner = std::make_shared<ColumnInt64>();
                    inner->Reserve(n);
                    for (size_t i = 0; i < n; i++) inner->Append(std::isnan(data[i]) ? 0LL : (int64_t)data[i]);
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else if (int_type == "UInt8") {
                    auto inner = std::make_shared<ColumnUInt8>();
                    inner->Reserve(n);
                    for (size_t i = 0; i < n; i++) inner->Append(std::isnan(data[i]) ? 0 : (uint8_t)data[i]);
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else if (int_type == "UInt16") {
                    auto inner = std::make_shared<ColumnUInt16>();
                    inner->Reserve(n);
                    for (size_t i = 0; i < n; i++) inner->Append(std::isnan(data[i]) ? 0 : (uint16_t)data[i]);
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else if (int_type == "UInt32") {
                    auto inner = std::make_shared<ColumnUInt32>();
                    inner->Reserve(n);
                    for (size_t i = 0; i < n; i++) inner->Append(std::isnan(data[i]) ? 0u : (uint32_t)data[i]);
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else if (int_type == "UInt64") {
                    auto inner = std::make_shared<ColumnUInt64>();
                    inner->Reserve(n);
                    for (size_t i = 0; i < n; i++) inner->Append(std::isnan(data[i]) ? 0ULL : (uint64_t)data[i]);
                    block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
                } else {
                    // fallback to Float64
                    auto col = std::make_shared<ColumnFloat64>();
                    auto& v = col->GetWritableData();
                    v.resize(n);
                    if (n) std::memcpy(v.data(), data, n * sizeof(double));
                    block.AppendColumn(fname, col);
                }
            } else if ((has_nan && !strict_nullable) || force_nullable) {
                auto inner = std::make_shared<ColumnFloat64>();
                auto nulls = std::make_shared<ColumnUInt8>();
                inner->Reserve(n);
                nulls->Reserve(n);
                for (size_t i = 0; i < n; i++) {
                    bool is_null = std::isnan(data[i]);
                    nulls->Append(is_null ? 1 : 0);
                    inner->Append(is_null ? 0.0 : data[i]);
                }
                block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
            } else {
                auto col = std::make_shared<ColumnFloat64>();
                auto& v = col->GetWritableData();
                v.resize(n);
                if (n) std::memcpy(v.data(), data, n * sizeof(double));
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxSINGLE_CLASS: {
            float* data = mxGetSingles(fd);
            bool has_nan = false;
            for (size_t i = 0; i < n && !has_nan; i++) has_nan = std::isnan(data[i]);
            bool force_nullable = nullable_cols.count(fname) > 0;
            if (lc_cols.count(fname)) {
                auto lc = std::make_shared<ColumnLowCardinalityT<ColumnFloat32>>();
                lc->Reserve(n);
                for (size_t i = 0; i < n; i++) lc->Append(data[i]);
                block.AppendColumn(fname, lc);
            } else if ((has_nan && !strict_nullable) || force_nullable) {
                auto inner = std::make_shared<ColumnFloat32>();
                auto nulls = std::make_shared<ColumnUInt8>();
                inner->Reserve(n);
                nulls->Reserve(n);
                for (size_t i = 0; i < n; i++) {
                    bool is_null = std::isnan(data[i]);
                    nulls->Append(is_null ? 1 : 0);
                    inner->Append(is_null ? 0.0f : data[i]);
                }
                block.AppendColumn(fname, std::make_shared<ColumnNullable>(inner, nulls));
            } else {
                auto col = std::make_shared<ColumnFloat32>();
                auto& v = col->GetWritableData();
                v.resize(n);
                if (n) std::memcpy(v.data(), data, n * sizeof(float));
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxINT8_CLASS: {
            int8_T* data = mxGetInt8s(fd);
            if (lc_cols.count(fname)) {
                auto lc = std::make_shared<ColumnLowCardinalityT<ColumnInt8>>();
                lc->Reserve(n);
                for (size_t i = 0; i < n; i++) lc->Append(data[i]);
                block.AppendColumn(fname, lc);
            } else {
                auto col = std::make_shared<ColumnInt8>();
                auto& v = col->GetWritableData();
                v.resize(n);
                if (n) std::memcpy(v.data(), data, n * sizeof(int8_t));
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxINT16_CLASS: {
            int16_T* data = mxGetInt16s(fd);
            if (lc_cols.count(fname)) {
                auto lc = std::make_shared<ColumnLowCardinalityT<ColumnInt16>>();
                lc->Reserve(n);
                for (size_t i = 0; i < n; i++) lc->Append(data[i]);
                block.AppendColumn(fname, lc);
            } else {
                auto col = std::make_shared<ColumnInt16>();
                auto& v = col->GetWritableData();
                v.resize(n);
                if (n) std::memcpy(v.data(), data, n * sizeof(int16_t));
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxINT32_CLASS: {
            int32_T* data = mxGetInt32s(fd);
            if (lc_cols.count(fname)) {
                auto lc = std::make_shared<ColumnLowCardinalityT<ColumnInt32>>();
                lc->Reserve(n);
                for (size_t i = 0; i < n; i++) lc->Append(data[i]);
                block.AppendColumn(fname, lc);
            } else {
                auto col = std::make_shared<ColumnInt32>();
                auto& v = col->GetWritableData();
                v.resize(n);
                if (n) std::memcpy(v.data(), data, n * sizeof(int32_t));
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxINT64_CLASS: {
            int64_T* data = mxGetInt64s(fd);
            if (lc_cols.count(fname)) {
                auto lc = std::make_shared<ColumnLowCardinalityT<ColumnInt64>>();
                lc->Reserve(n);
                for (size_t i = 0; i < n; i++) lc->Append(data[i]);
                block.AppendColumn(fname, lc);
            } else {
                auto col = std::make_shared<ColumnInt64>();
                auto& v = col->GetWritableData();
                v.resize(n);
                if (n) std::memcpy(v.data(), data, n * sizeof(int64_t));
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxLOGICAL_CLASS: {
            // MATLAB logical → ClickHouse Bool (or UInt8). Bool is stored as UInt8
            // on the wire; clickhouse-cpp has no separate Bool type code.
            auto col = std::make_shared<ColumnUInt8>();
            mxLogical* data = mxGetLogicals(fd);
            col->Reserve(n);
            for (size_t i = 0; i < n; i++) col->Append(data[i] ? 1 : 0);
            block.AppendColumn(fname, col);
            break;
        }
        case mxUINT8_CLASS: {
            uint8_T* data = mxGetUint8s(fd);
            if (lc_cols.count(fname)) {
                auto lc = std::make_shared<ColumnLowCardinalityT<ColumnUInt8>>();
                lc->Reserve(n);
                for (size_t i = 0; i < n; i++) lc->Append(data[i]);
                block.AppendColumn(fname, lc);
            } else {
                auto col = std::make_shared<ColumnUInt8>();
                auto& v = col->GetWritableData();
                v.resize(n);
                if (n) std::memcpy(v.data(), data, n * sizeof(uint8_t));
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxUINT16_CLASS: {
            uint16_T* data = mxGetUint16s(fd);
            if (lc_cols.count(fname)) {
                auto lc = std::make_shared<ColumnLowCardinalityT<ColumnUInt16>>();
                lc->Reserve(n);
                for (size_t i = 0; i < n; i++) lc->Append(data[i]);
                block.AppendColumn(fname, lc);
            } else {
                auto col = std::make_shared<ColumnUInt16>();
                auto& v = col->GetWritableData();
                v.resize(n);
                if (n) std::memcpy(v.data(), data, n * sizeof(uint16_t));
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxUINT32_CLASS: {
            uint32_T* data = mxGetUint32s(fd);
            if (lc_cols.count(fname)) {
                auto lc = std::make_shared<ColumnLowCardinalityT<ColumnUInt32>>();
                lc->Reserve(n);
                for (size_t i = 0; i < n; i++) lc->Append(data[i]);
                block.AppendColumn(fname, lc);
            } else {
                auto col = std::make_shared<ColumnUInt32>();
                auto& v = col->GetWritableData();
                v.resize(n);
                if (n) std::memcpy(v.data(), data, n * sizeof(uint32_t));
                block.AppendColumn(fname, col);
            }
            break;
        }
        case mxUINT64_CLASS: {
            uint64_T* data = mxGetUint64s(fd);
            if (lc_cols.count(fname)) {
                auto lc = std::make_shared<ColumnLowCardinalityT<ColumnUInt64>>();
                lc->Reserve(n);
                for (size_t i = 0; i < n; i++) lc->Append(data[i]);
                block.AppendColumn(fname, lc);
            } else {
                auto col = std::make_shared<ColumnUInt64>();
                auto& v = col->GetWritableData();
                v.resize(n);
                if (n) std::memcpy(v.data(), data, n * sizeof(uint64_t));
                block.AppendColumn(fname, col);
            }
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
            // If all cells are null sentinels (empty double) but the column is a known
            // string-like type (Enum, IPv4, IPv6, FixedString), force string-col path so
            // the Nullable handler below can create the proper all-null column.
            if (!is_string_col && !is_arr_str_col &&
                (enum_type_cols.count(fname) || ipv4_cols.count(fname) ||
                 ipv6_cols.count(fname) || fixedstring_cols.count(fname))) {
                is_string_col = true;
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
                    inner->Reserve(n);
                    nulls->Reserve(n);
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
                } else if (lc_cols.count(fname) && lc_cols.at(fname) == "String") {
                    auto lc = std::make_shared<ColumnLowCardinalityT<ColumnString>>();
                    lc->Reserve(n);
                    for (size_t i = 0; i < n; i++) {
                        const mxArray* cell = mxGetCell(fd, i);
                        if (!cell || mxIsEmpty(cell)) { lc->Append(""); continue; }
                        char* str = mxArrayToUTF8String(cell);
                        if (str) { lc->Append(str); mxFree(str); }
                        else      { lc->Append(""); }
                    }
                    block.AppendColumn(fname, lc);
                } else if (ipv4_cols.count(fname)) {
                    auto col = std::make_shared<ColumnIPv4>();
                    col->Reserve(n);
                    for (size_t i = 0; i < n; i++) {
                        const mxArray* cell = mxGetCell(fd, i);
                        if (!cell || mxIsEmpty(cell)) { col->Append(uint32_t(0)); continue; }
                        char* str = mxArrayToUTF8String(cell);
                        if (str) { col->Append(std::string(str)); mxFree(str); }
                        else      { col->Append(uint32_t(0)); }
                    }
                    block.AppendColumn(fname, col);
                } else if (ipv6_cols.count(fname)) {
                    auto col = std::make_shared<ColumnIPv6>();
                    col->Reserve(n);
                    for (size_t i = 0; i < n; i++) {
                        const mxArray* cell = mxGetCell(fd, i);
                        if (!cell || mxIsEmpty(cell)) { col->Append(std::string_view("::")); continue; }
                        char* str = mxArrayToUTF8String(cell);
                        if (str) { col->Append(std::string_view(str)); mxFree(str); }
                        else      { col->Append(std::string_view("::")); }
                    }
                    block.AppendColumn(fname, col);
                } else if (enum_type_cols.count(fname)) {
                    // Parse enum type string and create typed column
                    const std::string& ets = enum_type_cols.at(fname);
                    bool is8 = (ets.size() >= 5 && ets.substr(0,5) == "Enum8");
                    std::vector<Type::EnumItem> items;
                    size_t p = ets.find('(');
                    size_t pe = ets.rfind(')');
                    if (p != std::string::npos && pe != std::string::npos) {
                        std::string content = ets.substr(p+1, pe-p-1);
                        size_t pos = 0;
                        while (pos < content.size()) {
                            while (pos < content.size() && content[pos] != '\'') pos++;
                            if (pos >= content.size()) break;
                            pos++;
                            size_t ns = pos;
                            while (pos < content.size() && content[pos] != '\'') pos++;
                            std::string name = content.substr(ns, pos - ns);
                            pos++;
                            while (pos < content.size() && (content[pos]==' '||content[pos]=='=')) pos++;
                            size_t vs = pos;
                            if (pos < content.size() && content[pos]=='-') pos++;
                            while (pos < content.size() && std::isdigit((unsigned char)content[pos])) pos++;
                            int16_t val = (int16_t)std::stoi(content.substr(vs, pos-vs));
                            items.push_back({name, val});
                            while (pos < content.size() && (content[pos]==' '||content[pos]==',')) pos++;
                        }
                    }
                    TypeRef etype = is8 ? Type::CreateEnum8(items) : Type::CreateEnum16(items);
                    if (is8) {
                        auto col = std::make_shared<ColumnEnum8>(etype);
                        col->Reserve(n);
                        for (size_t i = 0; i < n; i++) {
                            const mxArray* cell = mxGetCell(fd, i);
                            if (!cell || mxIsEmpty(cell)) { col->Append(int8_t(0)); continue; }
                            char* str = mxArrayToUTF8String(cell);
                            if (str) { col->Append(std::string(str)); mxFree(str); }
                            else col->Append(int8_t(0));
                        }
                        block.AppendColumn(fname, col);
                    } else {
                        auto col = std::make_shared<ColumnEnum16>(etype);
                        col->Reserve(n);
                        for (size_t i = 0; i < n; i++) {
                            const mxArray* cell = mxGetCell(fd, i);
                            if (!cell || mxIsEmpty(cell)) { col->Append(int16_t(0)); continue; }
                            char* str = mxArrayToUTF8String(cell);
                            if (str) { col->Append(std::string(str)); mxFree(str); }
                            else col->Append(int16_t(0));
                        }
                        block.AppendColumn(fname, col);
                    }
                } else if (fixedstring_cols.count(fname)) {
                    size_t fsn = fixedstring_cols.at(fname);
                    auto col = std::make_shared<ColumnFixedString>(fsn);
                    col->Reserve(n);
                    for (size_t i = 0; i < n; i++) {
                        const mxArray* cell = mxGetCell(fd, i);
                        if (!cell || mxIsEmpty(cell)) { col->Append(std::string(fsn, '\0')); continue; }
                        char* str = mxArrayToUTF8String(cell);
                        if (str) { col->Append(str); mxFree(str); }
                        else      { col->Append(std::string(fsn, '\0')); }
                    }
                    block.AppendColumn(fname, col);
                } else {
                    // Plain String column
                    auto col = std::make_shared<ColumnString>();
                    col->Reserve(n);
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
                arr_col->Reserve(n);
                for (size_t i = 0; i < n; i++) {
                    const mxArray* cell = mxGetCell(fd, i);
                    auto inner = std::make_shared<ColumnString>();
                    if (cell && !mxIsEmpty(cell)) {
                        size_t m = mxGetNumberOfElements(cell);
                        inner->Reserve(m);
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
                arr_col->Reserve(n);

                for (size_t i = 0; i < n; i++) {
                    const mxArray* cell = mxGetCell(fd, i);
                    auto inner = make_inner_col();
                    if (cell && !mxIsEmpty(cell)) {
                        size_t m = mxGetNumberOfElements(cell);
                        switch (inner_class) {
                        case mxDOUBLE_CLASS: { auto ic=inner->As<ColumnFloat64>(); double*   d=mxGetDoubles(cell);  auto& v=ic->GetWritableData(); v.resize(m); if(m) std::memcpy(v.data(),d,m*sizeof(double));   break; }
                        case mxSINGLE_CLASS: { auto ic=inner->As<ColumnFloat32>(); float*    d=mxGetSingles(cell); auto& v=ic->GetWritableData(); v.resize(m); if(m) std::memcpy(v.data(),d,m*sizeof(float));    break; }
                        case mxINT8_CLASS:   { auto ic=inner->As<ColumnInt8>();    int8_T*   d=mxGetInt8s(cell);   auto& v=ic->GetWritableData(); v.resize(m); if(m) std::memcpy(v.data(),d,m*sizeof(int8_t));   break; }
                        case mxINT16_CLASS:  { auto ic=inner->As<ColumnInt16>();   int16_T*  d=mxGetInt16s(cell);  auto& v=ic->GetWritableData(); v.resize(m); if(m) std::memcpy(v.data(),d,m*sizeof(int16_t));  break; }
                        case mxINT32_CLASS:  { auto ic=inner->As<ColumnInt32>();   int32_T*  d=mxGetInt32s(cell);  auto& v=ic->GetWritableData(); v.resize(m); if(m) std::memcpy(v.data(),d,m*sizeof(int32_t));  break; }
                        case mxINT64_CLASS:  { auto ic=inner->As<ColumnInt64>();   int64_T*  d=mxGetInt64s(cell);  auto& v=ic->GetWritableData(); v.resize(m); if(m) std::memcpy(v.data(),d,m*sizeof(int64_t));  break; }
                        case mxUINT8_CLASS:  { auto ic=inner->As<ColumnUInt8>();   uint8_T*  d=mxGetUint8s(cell);  auto& v=ic->GetWritableData(); v.resize(m); if(m) std::memcpy(v.data(),d,m*sizeof(uint8_t));  break; }
                        case mxUINT16_CLASS: { auto ic=inner->As<ColumnUInt16>();  uint16_T* d=mxGetUint16s(cell); auto& v=ic->GetWritableData(); v.resize(m); if(m) std::memcpy(v.data(),d,m*sizeof(uint16_t)); break; }
                        case mxUINT32_CLASS: { auto ic=inner->As<ColumnUInt32>();  uint32_T* d=mxGetUint32s(cell); auto& v=ic->GetWritableData(); v.resize(m); if(m) std::memcpy(v.data(),d,m*sizeof(uint32_t)); break; }
                        case mxUINT64_CLASS: { auto ic=inner->As<ColumnUInt64>();  uint64_T* d=mxGetUint64s(cell); auto& v=ic->GetWritableData(); v.resize(m); if(m) std::memcpy(v.data(),d,m*sizeof(uint64_t)); break; }
                        default: break;
                        }
                    }
                    arr_col->AppendAsColumn(inner);
                }
                block.AppendColumn(fname, arr_col);
            } else {
                // All cells are empty — insert as empty Float64 array column
                auto arr_col = std::make_shared<ColumnArray>(std::make_shared<ColumnFloat64>());
                arr_col->Reserve(n);
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
        // Reset the connection so the inserting_ flag is cleared. Without this,
        // a failed insert leaves the client in a stuck state where every subsequent
        // Insert() call throws "cannot execute query while inserting".
        try { client->ResetConnection(); } catch (...) {}
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
    else if (cmd == "reconnect") cmd_reconnect(nlhs, plhs, nrhs, prhs);
    else if (cmd == "delete")  cmd_delete (nlhs, plhs, nrhs, prhs);
    else if (cmd == "version") cmd_version(nlhs, plhs, nrhs, prhs);
    else
        mexErrMsgIdAndTxt("ClickHouse:unknownCommand", "Unknown command: %s", cmd.c_str());
}

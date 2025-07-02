#include "mex.h"
#include <clickhouse/client.h>
#include <future>
#include <string>
#include <atomic>

using namespace clickhouse;


void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 1 || !mxIsChar(prhs[0])) {
        mexErrMsgTxt("First argument must be a SQL query string.");
    }

    char* query = mxArrayToString(prhs[0]);

    try {
        Client client(ClientOptions()
            .SetHost("10.150.28.18")
            .SetPort(9000)
            .SetUser("last_user")
            .SetPassword("physics")
            .SetDefaultDatabase("last"));

        std::promise<Block> prom;
        std::future<Block> fut = prom.get_future();

        std::atomic<bool> promise_set(false);

        client.Select(query,
            [&prom, &promise_set](const Block& block) {
                if (!promise_set.exchange(true)) {
                    prom.set_value(block);  // set block only once
                }
            });

        // wait synchronously for query result
        Block block = fut.get();

        if (block.GetRowCount() == 0 || block.GetColumnCount() == 0) {
            plhs[0] = mxCreateDoubleScalar(NAN);
        } else {
            auto col = block[0];

            if (auto col_uint64 = col->As<ColumnUInt64>()) {
                uint64_t val = col_uint64->At(0);
                plhs[0] = mxCreateDoubleScalar(static_cast<double>(val));
            }
            else if (auto col_uint32 = col->As<ColumnUInt32>()) {
                uint32_t val = col_uint32->At(0);
                plhs[0] = mxCreateDoubleScalar(static_cast<double>(val));
            }
            else if (auto col_uint8 = col->As<ColumnUInt8>()) {
                uint8_t val = col_uint8->At(0);
                plhs[0] = mxCreateDoubleScalar(static_cast<double>(val));
            }
            else if (auto col_float64 = col->As<ColumnFloat64>()) {
                double val = col_float64->At(0);
                plhs[0] = mxCreateDoubleScalar(val);
            }
            else if (auto col_string = col->As<ColumnString>()) {
                std::string_view sv = col_string->At(0);
                std::string str(sv);  // explicit conversion
                plhs[0] = mxCreateString(str.c_str());
            }
            else {
                mexErrMsgTxt("Unsupported column type or empty result.");
            }
        }
    } catch (const std::exception& e) {
        mexErrMsgTxt(e.what());
    }

    mxFree(query);
}

#include "mex.h"
#include <clickhouse/client.h>
#include <string>
#include <future>
#include "block2struct.cpp"  // include the helper here

using namespace clickhouse;

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 1 || !mxIsChar(prhs[0])) {
        mexErrMsgTxt("First input must be SQL query string.");
    }

    char* query = mxArrayToString(prhs[0]);

    try {
        Client client(ClientOptions()
            .SetHost("10.150.28.18")
            .SetPort(9000)
            .SetUser("last_user")
            .SetPassword("physics")
            .SetDefaultDatabase("last"));

        // Block result;
        // std::promise<void> done;
        // std::mutex mtx;
        // 
        // client.Select(query, [&](const Block& b) {
        //     std::lock_guard<std::mutex> lock(mtx);
        //     if (b.GetRowCount() > 0)
        //         result = b;
        // });
        // 
        // // wait to ensure all callbacks complete
        // std::this_thread::sleep_for(std::chrono::milliseconds(50));

        // plhs[0] = convert_block_to_struct_array(result);

        std::vector<clickhouse::Block> allBlocks;

        client.Select(query, [&](const clickhouse::Block& block) {
            if (block.GetRowCount() > 0) {
                allBlocks.push_back(block);
            }
        });      

        plhs[0] = convert_blocks_to_struct_array(allBlocks);

    } catch (const std::exception& e) {
        mexErrMsgTxt(e.what());
    }

    mxFree(query);
}


  

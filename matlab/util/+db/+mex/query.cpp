#include "mex.h"
#include <clickhouse/client.h>
#include <string>
#include <future>
#include "block2struct.cpp"  // include the helper here

using namespace clickhouse;

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 1 || !mxIsChar(prhs[0])) {
        mexErrMsgTxt("The first input must be an SQL query string \n"
        "The next optional inputs are: host, user name, password, DB name");
    }

    char* query = mxArrayToString(prhs[0]);  

    // Defaults
    std::string host = "10.150.28.18";
    int port = 9000;
    std::string user = "last_user";
    std::string password = "physics";
    std::string database = "last";

    // Optional: [host, user, password, database]
    if (nrhs >= 2 && mxIsChar(prhs[1])) {
        host = mxArrayToString(prhs[1]);
    }
    if (nrhs >= 3 && mxIsChar(prhs[2])) {
        user = mxArrayToString(prhs[2]);
    }
    if (nrhs >= 4 && mxIsChar(prhs[3])) {
        password = mxArrayToString(prhs[3]);
    }
    if (nrhs >= 5 && mxIsChar(prhs[4])) {
        database = mxArrayToString(prhs[4]);
    }

    try {
        Client client(ClientOptions()
            .SetHost(host)
            .SetPort(port)
            .SetUser(user)
            .SetPassword(password)
            .SetDefaultDatabase(database));
   
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


  

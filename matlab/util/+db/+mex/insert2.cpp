#include "mex.h"
#include "matrix.h"
#include <clickhouse/client.h> 
#include <memory>
#include <vector>
#include <string>
#include <algorithm>
#include <iostream>

using namespace std;

static constexpr size_t BATCH_SIZE = 100000;

/* ============================================================
 * Safe string conversion
 * ============================================================ */
static std::string mxToString(const mxArray* a, const char* name)
{
    if (!a || !mxIsChar(a))
        mexErrMsgIdAndTxt("ClickHouse:TypeError", "%s must be char", name);

    char* s = mxArrayToUTF8String(a);
    if (!s)
        mexErrMsgIdAndTxt("ClickHouse:TypeError", "Failed to convert %s", name);

    std::string out(s);
    mxFree(s);
    return out;
}

/* ============================================================
 * Get VariableNames via subsref
 * ============================================================ */
static std::vector<std::string> getColumnNames(const mxArray* table)
{
    const char* fields[] = {"type","subs"};

    mxArray* rhs[2];
    rhs[0] = const_cast<mxArray*>(table);
    rhs[1] = mxCreateStructMatrix(1,1,2,fields);
    mxSetField(rhs[1],0,"type",mxCreateString("."));
    mxSetField(rhs[1],0,"subs",mxCreateString("Properties"));

    mxArray* props = nullptr;
    if (mexCallMATLAB(1,&props,2,rhs,"subsref") != 0 || !props)
        mexErrMsgIdAndTxt("ClickHouse:Error","Cannot access table.Properties");
    mxDestroyArray(rhs[1]);

    rhs[0] = props;
    rhs[1] = mxCreateStructMatrix(1,1,2,fields);
    mxSetField(rhs[1],0,"type",mxCreateString("."));
    mxSetField(rhs[1],0,"subs",mxCreateString("VariableNames"));

    mxArray* names = nullptr;
    if (mexCallMATLAB(1,&names,2,rhs,"subsref") != 0 || !names)
        mexErrMsgIdAndTxt("ClickHouse:Error","Cannot access VariableNames");

    mxDestroyArray(props); 
    mxDestroyArray(rhs[1]); 

    size_t n = mxGetNumberOfElements(names);
    std::vector<std::string> out(n);

    for (size_t i=0;i<n;++i) {
        mxArray* c = mxGetCell(names,i);
        char* s = mxArrayToUTF8String(c);
        out[i] = s;
        mxFree(s);
    }
    mxDestroyArray(names);
    return out;
}

/* ============================================================
 * Extract T{rows, col}
 * ============================================================ */
static mxArray* getTableColumn(
    const mxArray* table,
    size_t col,
    size_t row0,
    size_t nrows)
{
    const char* fields[] = {"type","subs"};

    mxArray* rows = mxCreateDoubleMatrix(1,nrows,mxREAL);
    double* pr = mxGetPr(rows);
    for (size_t i=0;i<nrows;++i)
        pr[i] = double(row0 + i + 1);

    mxArray* cols = mxCreateDoubleScalar(double(col+1));

    mxArray* subs = mxCreateCellMatrix(1,2);
    mxSetCell(subs,0,rows);
    mxSetCell(subs,1,cols);

    mxArray* s = mxCreateStructMatrix(1,1,2,fields);
    mxSetField(s,0,"type",mxCreateString("{}"));
    mxSetField(s,0,"subs",subs);

    mxArray* rhs[2] = { const_cast<mxArray*>(table), s };
    mxArray* lhs = nullptr;

    if (mexCallMATLAB(1,&lhs,2,rhs,"subsref") != 0)
        mexErrMsgIdAndTxt("ClickHouse:IndexError","table indexing failed");

    mxDestroyArray(s);
    return lhs;
}

/* ============================================================
 * Main MEX
 * ============================================================ */
void mexFunction(int, mxArray**, int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2)
        mexErrMsgIdAndTxt("ClickHouse:Usage",
            "insert1(table, tableName, [host,user,password,database])");

    if (!mxIsClass(prhs[0],"table"))
        mexErrMsgIdAndTxt("ClickHouse:TypeError","First arg must be table");

    std::string tableName = mxToString(prhs[1],"tableName");

    std::string host="localhost", user="default", password="", database="default";
    int port = 9000;

    if (nrhs>2) host = mxToString(prhs[2],"host");
    if (nrhs>3) user = mxToString(prhs[3],"user");
    if (nrhs>4) password = mxToString(prhs[4],"password");
    if (nrhs>5) database = mxToString(prhs[5],"database");

    std::vector<std::string> colNames = getColumnNames(prhs[0]);
    size_t ncols = colNames.size();

//    mexPrintf("%s \n", "Starting");
       
    mxArray* height = nullptr;
    mxArray* err = mexCallMATLABWithTrap(1,&height,1,
        const_cast<mxArray**>(&prhs[0]),"height");
       
    size_t nrows = static_cast<size_t>(mxGetScalar(height));

    mxDestroyArray(height);

    clickhouse::ClientOptions opts;
    opts.SetHost(host).SetPort(port).SetUser(user)
        .SetPassword(password).SetDefaultDatabase(database);
    clickhouse::Client client(opts);

//  mexPrintf("%s %d\n", "Connected", nrows);

    for (size_t off=0; off<nrows; off+=BATCH_SIZE) {
        
        size_t rows = std::min(BATCH_SIZE, nrows-off);
        clickhouse::Block block;

        for (size_t c=0;c<ncols;++c) {
            mxArray* col = getTableColumn(prhs[0],c,off,rows);
            if (mxIsDouble(col)) {
                auto data = mxGetPr(col);
                auto nested = std::make_shared<clickhouse::ColumnFloat64>();
                auto nulls  = std::make_shared<clickhouse::ColumnUInt8>();

                for (size_t i=0;i<rows;++i) {
                    if (mxIsNaN(data[i])) {
                        nested->Append(0);
                        nulls->Append(1);
                    } else {
                        nested->Append(data[i]);
                        nulls->Append(0);
                    }
                }
                block.AppendColumn(
                    colNames[c],
                    std::make_shared<clickhouse::ColumnNullable>(nested,nulls));
            }
            else if (mxIsLogical(col)) {
                auto data = mxGetLogicals(col);
                auto ch = std::make_shared<clickhouse::ColumnUInt8>();
                for (size_t i=0;i<rows;++i) ch->Append(data[i]);
                block.AppendColumn(colNames[c], ch);
            }
            else if (mxIsChar(col) || mxIsClass(col,"string")) {
                auto ch = std::make_shared<clickhouse::ColumnString>();
                for (size_t i=0;i<rows;++i) {
                    mxArray* s = mxGetCell(col,i);
                    char* buf = mxArrayToUTF8String(s);
                    ch->Append(buf ? buf : "");
                    mxFree(buf);
                }
                block.AppendColumn(colNames[c], ch);
            }
            else {
                mexErrMsgIdAndTxt("ClickHouse:TypeError",
                    "Unsupported MATLAB column type");
            }
            mxDestroyArray(col);
        }
        client.Insert(tableName, block);
    }
}


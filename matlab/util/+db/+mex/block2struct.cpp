#include <clickhouse/client.h>
#include <mex.h>
#include <string>
#include <vector>

using namespace clickhouse;

mxArray* convert_blocks_to_struct_array(const std::vector<Block>& blocks) {
    if (blocks.empty()) {
        return mxCreateDoubleMatrix(0, 0, mxREAL);
    }

    size_t nCols = blocks[0].GetColumnCount();

    // Calculate total rows
    size_t totalRows = 0;
    for (const auto& block : blocks) {
        totalRows += block.GetRowCount();
    }
    if (totalRows == 0) {
        return mxCreateDoubleMatrix(0, 0, mxREAL);
    }

    // Get column names from first block
    std::vector<std::string> fieldNames;
    for (size_t c = 0; c < nCols; ++c) {
        std::string name = blocks[0].GetColumnName(c);
        if (name.empty()) {
            name = "col" + std::to_string(c);
        }
        fieldNames.push_back(name);
    }

    std::vector<const char*> c_fieldNames;
    for (auto& name : fieldNames) {
        c_fieldNames.push_back(name.c_str());
    }

    // Create struct array with totalRows rows, 1 col
    mxArray* result = mxCreateStructMatrix(totalRows, 1, nCols, c_fieldNames.data());

    size_t destRow = 0;

    // Loop through all blocks
    for (const auto& block : blocks) {
        size_t nRows = block.GetRowCount();

        for (size_t c = 0; c < nCols; ++c) {
            auto col = block[c];

            // For each column type handle conversion and fill struct fields            

            mxArray* arr = nullptr;

            if (auto col_uint64 = col->As<ColumnUInt64>()) {
                arr = mxCreateNumericMatrix(1, nRows, mxUINT64_CLASS, mxREAL);
                auto* data = static_cast<uint64_t*>(mxGetData(arr));
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = col_uint64->At(r);
                }
            } else if (auto col_uint32 = col->As<ColumnUInt32>()) {
                arr = mxCreateNumericMatrix(1, nRows, mxUINT32_CLASS, mxREAL);
                auto* data = static_cast<uint32_t*>(mxGetData(arr));
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = col_uint32->At(r);
                }
            } else if (auto col_uint16 = col->As<ColumnUInt16>()) {
                arr = mxCreateNumericMatrix(1, nRows, mxUINT16_CLASS, mxREAL);
                auto* data = static_cast<uint16_t*>(mxGetData(arr));
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = col_uint16->At(r);
                }
            } else if (auto col_uint8 = col->As<ColumnUInt8>()) {
                arr = mxCreateNumericMatrix(1, nRows, mxUINT8_CLASS, mxREAL);
                auto* data = static_cast<uint8_t*>(mxGetData(arr));
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = col_uint8->At(r);
                }
            } else if (auto col_int64 = col->As<ColumnInt64>()) {
                arr = mxCreateNumericMatrix(1, nRows, mxINT64_CLASS, mxREAL);
                auto* data = static_cast<int64_t*>(mxGetData(arr));
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = col_int64->At(r);
                }
            } else if (auto col_int32 = col->As<ColumnInt32>()) {
                arr = mxCreateNumericMatrix(1, nRows, mxINT32_CLASS, mxREAL);
                auto* data = static_cast<int32_t*>(mxGetData(arr));
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = col_int32->At(r);
                }
            } else if (auto col_int16 = col->As<ColumnInt16>()) {
                arr = mxCreateNumericMatrix(1, nRows, mxINT16_CLASS, mxREAL);
                auto* data = static_cast<int16_t*>(mxGetData(arr));
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = col_int16->At(r);
                }
            } else if (auto col_int8 = col->As<ColumnInt8>()) {
                arr = mxCreateNumericMatrix(1, nRows, mxINT8_CLASS, mxREAL);
                auto* data = static_cast<int8_t*>(mxGetData(arr));
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = col_int8->At(r);
                }
            } else if (auto col_float64 = col->As<ColumnFloat64>()) {
                arr = mxCreateDoubleMatrix(1, nRows, mxREAL);
                double* data = mxGetPr(arr);
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = col_float64->At(r);
                }
            } else if (auto col_float32 = col->As<ColumnFloat32>()) {
                arr = mxCreateNumericMatrix(1, nRows, mxSINGLE_CLASS, mxREAL);
                float* data = static_cast<float*>(mxGetData(arr));
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = col_float32->At(r);
                }
            } else if (auto col_string = col->As<ColumnString>()) {
                arr = mxCreateCellMatrix(1, nRows);
                for (size_t r = 0; r < nRows; ++r) {
                    std::string str(col_string->At(r));
                    mxSetCell(arr, r, mxCreateString(str.c_str()));
                }
            } else if (auto col_datetime64 = col->As<ColumnDateTime64>()) {
                arr = mxCreateDoubleMatrix(1, nRows, mxREAL);
                double* data = mxGetPr(arr);
                for (size_t r = 0; r < nRows; ++r) {
                    int64_t raw = col_datetime64->At(r);
                    data[r] = static_cast<double>(raw) / 1000.0; // adjust divisor to your scale (here milliseconds → seconds)
                }
            } else {
                // Default: fill with NaNs
                arr = mxCreateDoubleMatrix(1, nRows, mxREAL);
                double* data = mxGetPr(arr);
                for (size_t r = 0; r < nRows; ++r) {
                    data[r] = NAN;
                }
            }
               
            // Now copy each element of arr into the corresponding struct field in result
            for (size_t r = 0; r < nRows; ++r) {
                mxArray* val;
                if (mxIsCell(arr)) {
                    val = mxGetCell(arr, r);
                    // Increase ref count because mxSetField takes ownership

                    // Following the advice of Mark Zitnik, this line:
                    // mxSetField(result, destRow + r, fieldNames[c].c_str(), val);
                    // was changed for these two lines:
                    mxArray* valCopy = mxDuplicateArray(val); // Create a copy! 
                    mxSetField(result, destRow + r, fieldNames[c].c_str(), valCopy); 

                    // Do NOT destroy val, it's now owned by result
                } else {                    
                    if (mxGetNumberOfElements(arr) == nRows) {
                        if (mxIsDouble(arr)) {
                            val = mxCreateDoubleScalar(mxGetPr(arr)[r]);
                        }
                        else {
                            mxClassID classId = mxGetClassID(arr);
                            switch (classId) {
                                case mxUINT64_CLASS: {
                                    val = mxCreateNumericMatrix(1, 1, mxUINT64_CLASS, mxREAL);
                                    *static_cast<uint64_t*>(mxGetData(val)) = static_cast<uint64_t*>(mxGetData(arr))[r];
                                    break;
                                }
                                case mxUINT32_CLASS: {
                                    val = mxCreateNumericMatrix(1, 1, mxUINT32_CLASS, mxREAL);
                                    *static_cast<uint32_t*>(mxGetData(val)) = static_cast<uint32_t*>(mxGetData(arr))[r];
                                    break;
                                }
                                case mxUINT16_CLASS: {
                                    val = mxCreateNumericMatrix(1, 1, mxUINT16_CLASS, mxREAL);
                                    *static_cast<uint16_t*>(mxGetData(val)) = static_cast<uint16_t*>(mxGetData(arr))[r];
                                    break;
                                }
                                case mxUINT8_CLASS: {
                                    val = mxCreateNumericMatrix(1, 1, mxUINT8_CLASS, mxREAL);
                                    *static_cast<uint8_t*>(mxGetData(val)) = static_cast<uint8_t*>(mxGetData(arr))[r];
                                    break;
                                }
                                case mxINT64_CLASS: {
                                    val = mxCreateNumericMatrix(1, 1, mxINT64_CLASS, mxREAL);
                                    *static_cast<int64_t*>(mxGetData(val)) = static_cast<int64_t*>(mxGetData(arr))[r];
                                    break;
                                }
                                case mxINT32_CLASS: {
                                    val = mxCreateNumericMatrix(1, 1, mxINT32_CLASS, mxREAL);
                                    *static_cast<int32_t*>(mxGetData(val)) = static_cast<int32_t*>(mxGetData(arr))[r];
                                    break;
                                }
                                case mxINT16_CLASS: {
                                    val = mxCreateNumericMatrix(1, 1, mxINT16_CLASS, mxREAL);
                                    *static_cast<int16_t*>(mxGetData(val)) = static_cast<int16_t*>(mxGetData(arr))[r];
                                    break;
                                }
                                case mxINT8_CLASS: {
                                    val = mxCreateNumericMatrix(1, 1, mxINT8_CLASS, mxREAL);
                                    *static_cast<int8_t*>(mxGetData(val)) = static_cast<int8_t*>(mxGetData(arr))[r];
                                    break;
                                }
                                case mxSINGLE_CLASS: {
                                    val = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);
                                    *static_cast<float*>(mxGetData(val)) = static_cast<float*>(mxGetData(arr))[r];
                                    break;
                                }
                                default: {
                                    // fallback
                                    val = mxCreateDoubleScalar(mxGetPr(arr)[r]);
                                    break;
                                }
                            }
                        }
                        mxSetField(result, destRow + r, fieldNames[c].c_str(), val);
                    }

                }
            }

            mxDestroyArray(arr);
        }
        destRow += nRows;
    }

    return result;
}

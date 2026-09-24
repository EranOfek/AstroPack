function Result = updateMatrixInplace(A, B, x1_A, y1_A, x1_B, y1_B, width_B, height_B)
% Update inplace matrix A with piece of matrix B.
% Input  : - A: 2D matrix of values
%          - B: 2D matrix of values
%          - x1_A:
%          - y1_A:
%          - x1_B:
%          - y1_B:
%          - width_B:
%          - height_B:
% Output : - None
% Author : Chen Tishler (Apr 2024)
% Example:
%          A = [3 6 9; 4 7 11];
%          B = [1 1; 1 1];
%          tools.array.updateMatrixInplace(A, B, 0, 0, 0, 0, 2, 2);
%----------------------------------------------------------------------

    % MEX implementation
    % Call function according to input data type
    C = lower(class(A));
    switch C
        case {'single'}
            tools.array.mex.mex_update_matrix_inplace_single(A, B, x1_A, int32(x1_A), int32(y1_A), int32(x1_B), int32(y1_B), int32(width_B), int32(height_B))
        case {'double'}
            tools.array.mex.mex_update_matrix_inplace_double(A, B, int32(x1_A), int32(y1_A), int32(x1_B), int32(y1_B), int32(width_B), int32(height_B))
        case {'uint32','int32'}
            tools.array.mex.mex_update_matrix_inplace_int8(A, B, int32(x1_A), int32(y1_A), int32(x1_B), int32(y1_B), int32(width_B), int32(height_B))

        case {'uint8','int8'}
            tools.array.mex.mex_update_matrix_inplace_int8(A, B, int32(x1_A), int32(y1_A), int32(x1_B), int32(y1_B), int32(width_B), int32(height_B))
        case {'uint16','int16'}
            tools.array.mex.mex_update_matrix_inplace_int8(A, B, int32(x1_A), int32(y1_A), int32(x1_B), int32(y1_B), int32(width_B), int32(height_B))
        case {'uint64','int64'}
            tools.array.mex.mex_update_matrix_inplace_int8(A, B, int32(x1_A), int32(y1_A), int32(x1_B), int32(y1_B), int32(width_B), int32(height_B))
        otherwise
            error('tools.array.updateMatrixInplace - Unsupported data type');
    end
end

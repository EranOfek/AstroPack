function Result = times(A, B) %, UseMex, UseMP, UseAVX)
    %
    % Input  : - A - Array
    %          - B - 
    %
	%          - UseMex - true to use MEX optimization
    %          - UseMP - true to use threading
	%
    % Output : - The result of the operation.
    %
    % Author : Chen Tishler (Apr 2023)
    % Example: 
    %    Array = zeros(3, 3, 'int32');
    %    Flag = rand(3, 3) > 0.9;
    %    Result = tools.array.bitsetFlag(Array, Flag, 1, 1);            
    %----------------------------------------------------------------------
    arguments
        A                	% Input & Output array
        B				   	% Input array
        %UseMex = true;     	% True: Use MEX implementation, False: Use MATLAB implementaion
        %UseMP = true;      	% True: Use threading with OpenMP multi-threading library
		%UseAVX = false;    	% True: Use AVX2/AVX512 implementation
    end

    % MATLAB implementation
%     if ~UseMex
%         if nargout == 0
%             error('When UseMex=false, must call with lvalue');
%         end
%         
%         Result = A .* B;
%         return;
%     end
%     
%     if nargout > 0
%         error('When UseMex=true, must call without lvalue');
%     end
%     
    % Must be of same type
    %assert(isequal(class(A), class(B)));
    
%     if UseAVX
%         error('AVX implementation is not supported yet');
%     end
    
    % From some reason, the MEX function does not update the input when
    % numel is 1
    if numel(A) < 2
        error('operators.times: input size must be > 1 due to unknwon issue in mex implemenation')
    end

    % MEX implementation
    % Call function according to input data type
    C = class(A);  %lower(class(A));    
    switch C
        case {'uint8','int8'}
             tools.operators.mex.mex_times_int8(A, B); %, int32(UseMP));                               
        case {'uint16','int16'}
             tools.operators.mex.mex_times_int16(A, B); % , int32(UseMP));                   
        case {'uint32','int32'}
             tools.operators.mex.mex_times_int32(A, B); % int32(UseMP));                               
        case {'uint64','int64'}
             tools.operators.mex.mex_times_int64(A, B); %, int32(UseMP));                               
        case {'single'}
             tools.operators.mex.mex_times_single(A, B);  %, int32(UseMP));                   
        case {'double'}
            %if UseAVX
                %avx = tools.os.get_avx_supported();
                %if avx == 2
                %    tools.operators.mex.mex_timesDouble_avx2(A, B, int32(UseMP));
				%else
                %tools.operators.mex.mex_times_double(A, B, int32(UseMP));
                %end
            %else

              tools.operators.mex.mex_times_double(A, B); % , int32(UseMP));

            %end
        otherwise
            error('tools.operators.times - Unsupported data type');
    end
end

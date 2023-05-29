function Val=bitor_array(Array, Dim, UseMex)
    % Perform a bitor operation along all elements in an array.
    % Package: Util.array
    % Description: Perform a bitor operation along all elements in an array
    %              along a specific dimension.
    % Input  : - An array of integers.
    %          - Dimension along to perform the bitor operation. Default is 1.
    %          - Flag, true to use MEX optimization if possible. Default is 1.
    % Output : - The result of the bitor operation. If input is empty, then
    %            the output is empty.
    % See also: sum_bitor.m (the same)
    % License: GNU general public license version 3
    % Tested : Matlab R2015b
    %     By : Eran O. Ofek                    Jun 2016
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: Array = uint32(randi(2^16,1600,1600,20));
    %          Val = tools.array.bitor_array(Array,3,false);
    % Reliable: 2
    %--------------------------------------------------------------------------

    arguments
        Array
        Dim              = 1;
        UseMex logical   = false;
    end


    if isempty(Array)
       Val = [];
    else

        C = lower(class(Array));
        switch C
            case {'uint8','int8'}
                Nbit = 8;
                Fun  = @uint8;
            case {'uint16','int16'}
                Nbit = 16;
                Fun  = @uint16;
            case {'uint32','int32'}
                Nbit = 32;
                Fun  = @uint32;
            case {'uint64','int64'}
                Nbit = 64;
                Fun  = @uint64;
            otherwise
                error('Unknown class - only integers are allowed');
        end

        % Check if we can use MEX implementation, convert input to uint64
        if UseMex && (ndims(Array) <= 3) && (Dim <= ndims(Array))
            switch Nbit
                case 8
                    Val = tools.array.mex.mex_bitor_array_int8(Array, Dim);       
                case 16
                    Val = tools.array.mex.mex_bitor_array_int16(Array, Dim);       
                case 32
                    Val = tools.array.mex.mex_bitor_array_int32(Array, Dim);       
                case 64
                    Val = tools.array.mex.mex_bitor_array_int64(Array, Dim);       
            end    
        else
            Val = 0;
            for Ibit=1:1:Nbit
                Val = Val + (2.^(Ibit-1)).*any(bitget(Array,Ibit),Dim);
            end

            % transform back to uint
            Val = Fun(Val);    
        end
    end
end

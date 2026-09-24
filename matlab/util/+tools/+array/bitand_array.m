function Val=bitand_array(Array, Dim, UseMex)
	% Perform a bitand operation along all elements in an array.
	% Package: Util.array
	% Description: Perform a bitand operation along all elements in an array
	%              along a specific dimension.
	% Input  : - An array of integers.
	%          - Dimension along to perform the bitand operation. Default is 1.
    %          - A logical - true for the new MEX optimization
    %            (tools.array.mex.bitand_dim). false for old mex
    %            (tools.array.mex.mex_bitand_array*)
    %            if [false false], then use matlab implementation.
    %            Default is [false false]
    % Output : - The result of the bitand operation.
	% See also: sum_bitor.m (the same)
	% License: GNU general public license version 3
	% Tested : Matlab R2015b
	%     By : Eran O. Ofek                    Jun 2016
	%    URL : http://weizmann.ac.il/home/eofek/matlab/
	% Example: Val=tools.array.bitand_array(Array);
	% Reliable: 2
	%--------------------------------------------------------------------------

    arguments
        Array
        Dim    = 1;
        UseMex = [false false];
    end
   
    if UseMex
        % new version
        Val = tools.array.mex.bitand_dim(Array, Dim);
    else
        % old version

    
	    % MEX version of AND still has bugs and is currently disabled
	    % @Todo, Chen, 22/12/2021
    
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
	    if isscalar(UseMex) && (ndims(Array) <= 3) && (Dim <= ndims(Array))
		     switch Nbit
			    case 8
				    Val = tools.array.mex.mex_bitand_array_int8(Array, Dim);       
			    case 16
				    Val = tools.array.mex.mex_bitand_array_int16(Array, Dim);       
			    case 32
				    Val = tools.array.mex.mex_bitand_array_int32(Array, Dim);       
			    case 64
				    Val = tools.array.mex.mex_bitand_array_int64(Array, Dim);       
		    end            
	    else
		    Val = 0;
		    for Ibit=1:1:Nbit
			    Val = Val + (2.^(Ibit-1)).*all(bitget(Array,Ibit),Dim);
		    end
    
		    % transform back to uint
		    Val = Fun(Val);    
	    end
    end
end


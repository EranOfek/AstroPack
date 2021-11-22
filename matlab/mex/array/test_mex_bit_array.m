% Test the original mex_WriteMatrix (mex_WriteMatrix.c)

function test_mex_bit_array()

	% create 'magic square' matrix 5x5
	Z=magic(5);

	% write it to 'magic.txt' 
	mex_WriteMatrix('magic.txt', Z, '%10.10f', ',', 'w+');

	% append it transposed to 'magic.txt' 
	mex_WriteMatrix('magic.txt', Z', '%10.10f', ',', 'a+');

	% free mex function
	clear mex_WriteMatrix;

end

%----------------------------------------------------------------------------

function Result = bitwise_cutouts(Obj, XY, Operator, Args)
	% Apply bitwise operator to a cutouts
	% Input  : - A single-element MaskImage object.
	%          - A two column matrix of [X, Y] positions.
	%            Positions will be rounded.
	%          - Operator: ['or'] | 'and'
	%          * ...,key,val,...
	%            'HalfSize' - Cutout half size (actual size will be
	%                   1+2*HalfSize. Default is 3.
	%            'CutAlgo' - Algorithm: ['mex'] | 'wmat'.
	%            'IsCircle' - If true then will pad each cutout
	%                   with NaN outside the HalfSize radius.
	%                   Default is false.
	%            'DataProp' - Data property from which to extract
	%                   the cutouts. Default is 'Image'.
	% Output : - A column vector of function output (bitwise or/and
	%            on all numbers in cutout) per each cutout.
	% Author : Eran Ofek (Jul 2021)
	% Example: IC=MaskImage({uint32(ones(100,100))});
	%          Result = bitwise_cutouts(IC, [1 1; 2 2; 10 10; 30 30], 'or')
	
	arguments
		Obj(1,1)
		XY
		Operator                    = 'or';
		Args.HalfSize               = 3;
		Args.CutAlgo                = 'wmat';  % 'mex' | 'wmat'
		Args.IsCircle               = false;
		Args.DataProp               = 'Image';
		
	end
	
	switch lower(Operator)
		case 'or'
			Fun = @tools.array.bitor_array;
		case 'and'
			Fun = @tools.array.bitand_array;
		otherwise
			error('Unnown Operator option');
	end
		
	Result = funCutouts(Obj, XY, Fun, 'HalfSize',Args.HalfSize,...
											  'PadVal',0,...
											  'CutALgo',Args.CutAlgo,...
											  'IsCircle',Args.IsCircle,...
											  'DataProp',Args.DataProp);
 
	
end

		
%----------------------------------------------------------------------------

function varargout = getMatrix(Obj, FieldNames)
	% Get matrix using field name
	% Input  : - A single element MatchedSources object.
	%          - Field name, or a cell array of field names.
	% Output : - A mtrix of [Epoch X Source] for each requested
	%            field.
	% Author : Eran Ofek (Jun 2021)
	% Example: MS = MatchedSources; MS.Data.FLUX = rand(100,200);
	%          A = MS.getMatrix('FLUX');
	
	arguments
		Obj(1,1)
		FieldNames
	end
   
	if ischar(FieldNames)
		FieldNames = {FieldNames};
	end
	
	Nfn = numel(FieldNames);
	if nargout>Nfn
		error('Number of output arguments is larger than the number of requested fields');
	end
	varargout = cell(1, nargout);
	for Ifn=1:1:nargout
		varargout{Ifn} = Obj.Data.(FieldNames{Ifn});
	end
	
	
end


%----------------------------------------------------------------------------

function Result = combineFlags(Obj, Args)
	% Combine (bitwise or) the flags of each sourc over all epochs
	% Input  : - - A MatchedSources object.
	%          * ...,key,val,...
	%            'FlagsNameDic' - A cell array of dictionary field names for the
	%                   Flags property for which to combine using or operator for each source.
	%                   If empty, then do not calculate.
	%                   Default is 'FLAGS'.
	%            'FlagsType' -  Afunction handle for FLAGS type.
	%                   Default is @uint32.
	% Output : - A structure array with the column wise (sources)
	%            of bitwise-or combine of all flags.
	% Author : Eran Ofek (Sep 2021)
	% Example: MS = MatchedSources;
	%          MS.addMatrix(uint32(rand(100,200).*1000),'FLAGS')
	%          Result = combineFlags(MS)
	
	arguments
		Obj
		Args.FlagsNameDic                = 'FLAGS';
		Args.FlagsType                   = @uint32;
	end

	[FlagsName] = getFieldNameDic(Obj(1), Args.FlagsNameDic);
	
	N = numel(Obj);
	for I=1:1:N
		% get Mag matrix
		
		DataFlags       = getMatrix(Obj(I), FlagsName);
		Result(I).FLAGS = tools.array.bitor_array(Args.FlagsType(DataFlags));
	end

end

%----------------------------------------------------------------------------

function Val = matlab_bitor_array(Array,Dim)

	if (nargin==1)
		Dim = 1;
	end

	C = class(Array);
	switch lower(C)
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

	Val = 0;
	for Ibit=1:1:Nbit
		Val = Val + (2.^(Ibit-1)).*any(bitget(Array,Ibit),Dim);
	end
	% transform back to uint
	Val = Fun(Val);
		
end		


%----------------------------------------------------------------------------

function Val = matlab_bitand_array(Array,Dim)

	if (nargin==1)
		Dim = 1;
	end

	C = class(Array);
	switch lower(C)
		case {'uint8','int8'}
			Nbit = 8;
		case {'uint16','int16'}
			Nbit = 16;
		case {'uint32','int32'}
			Nbit = 32;
		case {'uint64','int64'}
			Nbit = 64;
		otherwise
			error('Unknown class - only integers are allowed');
	end

	Val = 0;
	for Ibit=1:1:Nbit
		Val = Val + (2.^(Ibit-1)).*all(bitget(Array,Ibit),Dim);
	end
    
end
    
%----------------------------------------------------------------------------    
    
	
	
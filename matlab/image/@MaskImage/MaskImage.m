% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description:
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example :
% Reliable: 2
%--------------------------------------------------------------------------

% #functions (autogen)
% bitStat - Example: Stat = Bias.MaskData.bitStat
% bitwise_cutouts - Apply bitwise operator to a cutouts
% findBit - find pixels with some specific mask-bit open
% maskSet - Set the value of a bit in a bit mask
% #/functions (autogen)
%

classdef MaskImage < ImageComponent    % ImageComponent & BitDictionary
    
    properties (Dependent) % Access image data directly
%         Image
%         Data
%         Scale
%         ScaleMethod
%         FileName
    end
    properties (SetAccess = public)
        Dict BitDictionary                      % The dictionary of a bit mask image
    end
    properties (Hidden, SetAccess = public)
        % MaskData ImageComponent
    end
    
    
    methods % Constructor
        function Obj=MaskImage(varargin)
            % Constructor of MaskImage class using the superclass
            % (ImageComponent) constructor
            
            Obj@ImageComponent(varargin{:});
            for Iobj=1:1:numel(Obj)
                Obj(Iobj).DataType    = AstroDataType.Mask;
                Obj(Iobj).ScaleMethod = 'nearest';   % Use nearest interpolation for MaskData
            end
        end
        
    end
  
    methods % Setters/Getters
        
    end
    
    methods (Static)  % static methods
       
    end
    
    methods % functionality
        function Result = maskSet(Obj, Flag, BitName, SetVal, Args)
            % Set the value of one, or several, bits in a bit mask
            % Input  : - An ImageMask Object.
            %          - A matrix of logical, with the same size as the
            %            Image in the ImageMask, in which values which are
            %            true will be set.
            %            Alternatively, this can be a vector of indices.
            %            For multi-bit mode (BitName is a cell array), this
            %            must be a cell array of logical matrices of the
            %            same length as BitName (indices are not supported).
            %          - Bit name, or bit index (start from 0), to set.
            %            Alternatively, a cell array of bit names/indices,
            %            to set several bits in a single pass (multi-bit
            %            mode); in this case Flag must be a matching cell
            %            array of logical matrices, and SetVal must be a
            %            scalar (applied to all bits) or a vector matching
            %            BitName. Multi-bit mode requires UseMex=true.
            %          - Value to set (0 | 1). Default is 1.
            %          * ...,key,val,...
            %            'DefBitDict' - Default bit dictionary if
            %                   not exist. Default is
            %                   BitDictionary('BitMask.Image.Default').
            %            'CreateNewObj' - A logical indicating if to copy
            %                   the input object. Default is false.
            %            'UseFlags' - A logical indicating if to use flags
            %                   or indices (speed considerations).
            %                   Default is true.
            %            'UseMex' - Use the tools.array.mex.bitsetFlag
            %                   mex function. Default is true.
            % Output : - An ImageMaks object.
            % Author : Eran Ofek (May 2021)
            % Example:
            %       MI=MaskImage;
            %       MI.Dict=BitDictionary('BitMask.Image.Default')
            %       Flag = false(3,3); Flag(1,2)=true;
            %       Result = MI.maskSet(Flag,'Saturated')
            %       Result = MI.maskSet(Flag,'Streak')
            %       Result = MI.maskSet({Flag,Flag},{'Saturated','Streak'},[1 0])

            arguments
                Obj
                Flag                         % matrix of logicals, or vector of indices, or a cell array of these (multi-bit mode)
                BitName                      % name or bit index (start with zero), or a cell array of these (multi-bit mode)
                SetVal                            = 1;
                Args.DefBitDict                   = BitDictionary('BitMask.Image.Default');
                Args.CreateNewObj                 = false;
                Args.UseFlags                     = false;
                Args.UseMex                       = true;
            end

            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end

            Nobj = numel(Obj);

            IsMulti = iscell(BitName);
            if IsMulti
                Nbit = numel(BitName);
                if ~iscell(Flag) || numel(Flag)~=Nbit
                    error('When BitName is a cell array, Flag must be a cell array of the same length');
                end
                if ~Args.UseMex
                    error('Multi-bit mode (cell array BitName) requires UseMex=true');
                end
                if isscalar(SetVal)
                    SetVal = repmat(SetVal, 1, Nbit);
                elseif numel(SetVal)~=Nbit
                    error('SetVal must be scalar or match the number of BitNames');
                end
            end

            % a single Flag image
            if IsMulti
                if ~isempty(Flag) && islogical(Flag{1})
                    SizeImage = size(Flag{1});
                else
                    SizeImage = [];
                end
            else
                if islogical(Flag)
                    SizeImage = size(Flag);
                else
                    SizeImage = [];
                end
            end
            for Iobj=1:1:Nobj
                % check that BitDictionary is populated
                if isempty(Obj(Iobj).Dict)
                    Result(Iobj).Dict = Args.DefBitDict;
                end

                if IsMulti
                    BitInd = zeros(1,Nbit);
                    for Ibit=1:1:Nbit
                        if isnumeric(BitName{Ibit})
                            BitInd(Ibit) = BitName{Ibit} + 1;
                        else
                            BitInd(Ibit) = Result(Iobj).Dict.name2bit(BitName{Ibit}) + 1;
                        end
                    end
                else
                    if isnumeric(BitName)
                        BitInd = BitName + 1;
                    else
                        BitInd = Result(Iobj).Dict.name2bit(BitName) + 1;
                    end
                end

                if isempty(Obj(Iobj).Data)
                    % no maks image - allocate
                    if isempty(SizeImage)
                        error('When Flag is a vector of indices, image mask must be pre defined');
                    end
                    Result(Iobj).Image = Result(Iobj).Dict.Class(zeros(SizeImage));
                end

                if IsMulti
                    % set several bits in a single pass over the array
                    if ~all(cellfun(@(F) islogical(F) && numel(F)==numel(Result(Iobj).Data), Flag))
                        error('In multi-bit mode all Flag cells must be logical arrays of the same size as the mask');
                    end
                    Triplets = cell(1,3*Nbit);
                    for Ibit=1:1:Nbit
                        Triplets{3*(Ibit-1)+1} = squeeze(Flag{Ibit});
                        Triplets{3*(Ibit-1)+2} = BitInd(Ibit);
                        Triplets{3*(Ibit-1)+3} = SetVal(Ibit);
                    end
                    Result(Iobj).Data = tools.array.mex.bitsetFlagMulti(Result(Iobj).Data, Triplets{:});
                elseif Args.UseMex
                    if ~isempty(Flag)
                        if numel(Flag)==numel(Result(Iobj).Data)
                            % assume that Flag in logical array of the same
                            % size
                            Result(Iobj).Data = tools.array.bitsetFlag(Result(Iobj).Data, squeeze(Flag), BitInd, SetVal, true, true);
                        else
                            % Flag is indices:
                            % consider using: tools.array.mex.bitsetInd (only slightly faster)
                            Result(Iobj).Data(Flag) = bitset(Result(Iobj).Data(Flag), BitInd, SetVal);
                        end
                    end
                else

                    % use indices instead of flags - maybe faster in some cases
                    if Args.UseFlags
                        Result(Iobj).Data(Flag) = bitset(Result(Iobj).Data(Flag), BitInd, SetVal);
                    else
                        Ind = find(Flag);
                        Result(Iobj).Data(Ind) = bitset(Result(Iobj).Data(Ind), BitInd, SetVal);
                    end
                end
            end


        end
                
        function [Result, XY, Ind] = findBit(Obj, BitNames, Args)
            % find pixels with some specific mask-bit open
            % Input  : - A MaskImage object (multi elements supported).
            %          - A cell array of bit names, or a decimal number
            %            representing several bits.
            %          * ...,key,val,...
            %            'Method' - Indicating if to look for pixels in
            %                   which all the requested bits are on
            %                   ('all'), or one or more of the requested
            %                   bits are on ('any').
            %                   Default is 'all'.
            %            'OutType' - ['mat'] | 'ImageComponent'.
            %                   Output is a matrix of logicals (works only
            %                   if the MaskImage is a single element), or
            %                   an ImageComponent of images of logicals.
            %            'DataProp' - Data prop on which to operate, and in
            %                   which to store the results.
            %                   Default is 'Image'.
            % Output : - An array of logicals which size equal to the
            %            MaksImage 'Image' property.
            %            Each element of the array indicating if the
            %            specified bits, in each pixel position, are open.
            %          - A two column matrix of [X,Y] pixel positions in
            %            which the specified bits are open.
            %            Provided only for the last image in the
            %            MaskImage object.
            %          - A vector of indices of the pixels in which the
            %            specified bits are open.
            %            Provided only for the last image in the
            %            MaskImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example:
            
            
            arguments
                Obj
                BitNames
                Args.Method           = 'all';     % 'all' | 'any'
                Args.OutType          = 'mat';     % 'mat' | 'ImageComponent'
                Args.CCDSEC           = [];
                Args.DataProp         = 'Image';
            end
            
            if ischar(BitNames)
                BitNames = {BitNames};
            end
            
            Nobj = numel(Obj);
            
            switch lower(Args.OutType)
                case 'imagecomponent'
                    Result = ImageComponent(size(Obj));
                case 'mat'
                    if numel(Obj)>1
                        error('If MaskImage is multi element, then the OutType must be of type ImageComponent')
                    end
                otherwise
                    error('Unknown OutType option');
            end
            
            for Iobj=1:1:Nobj
                if isempty(BitNames)
                    Flag = [];
                else
                    if iscell(BitNames) || isstring(BitNames)
                        [BitInd,BitDec,SumBitDec,BitDescription] = name2bit(Obj(Iobj).Dict, BitNames);
                    else
                        SumBitDec = BitNames;
                    end

                    switch lower(Args.Method)
                        case 'all'
                            if isempty(Args.CCDSEC)
                                Flag = bitand(Obj(Iobj).(Args.DataProp), SumBitDec) == SumBitDec;
                            else
                                Flag = bitand(Obj(Iobj).(Args.DataProp)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)), SumBitDec) == SumBitDec;
                            end
                        case 'any'
                            if isempty(Args.CCDSEC)
                                Flag = bitand(Obj(Iobj).(Args.DataProp), SumBitDec) > 0;
                            else
                                Flag = bitand(Obj(Iobj).(Args.DataProp)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)), SumBitDec) > 0;
                            end
                        otherwise
                            error('Unknown Method option');
                    end
                end
                switch lower(Args.OutType)
                    case 'imagecomponent'
                        Result(Iobj).(Args.DataProp) = Flag;
                    case 'mat'
                        Result = Flag;
                    otherwise
                        error('Unknown OutType option');
                end

            end
            
            if nargout>1
                Ind = find(Flag);
                [X,Y]  = imUtil.image.ind2sub_fast(size(Flag), Ind);
                XY     = [X,Y];
            end
            
            
        end
        
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
            %            'UseMex' - A logical indicating if to use the mex
            %                   version. Default is true.
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
                
                Args.UseMex logical         = true;
            end
            
            switch lower(Operator)
                case 'or'
                    Fun = @tools.array.bitor_array;
                case 'and'
                    Fun = @tools.array.bitand_array;
                otherwise
                    error('Unnown Operator option');
            end
                
            if Args.UseMex
                % use the mex version (considerably faster)
                Result = imUtil.cut.bitwise_cutouts(Obj.Data, XY(:,1), XY(:,2), Args.HalfSize, strcmp(Operator,'or'));
            else
                Result = funCutouts(Obj, XY, Fun, 'HalfSize',Args.HalfSize,...
                                                      'PadVal',0,...
                                                      'CutALgo',Args.CutAlgo,...
                                                      'IsCircle',Args.IsCircle,...
                                                      'DataProp',Args.DataProp);
            end
         
            
        end
        
    end
    
    methods % bit statistics
        function [Tab, Result] = bitStat(Obj, Args)
            % Calculate bit mask statistics.
            %   How many times each bit appaers in an image.
            % Input  : - A MaskImage object.
            %          * ...,key,val,...
            %            'CCDSEC' - CCDSEC in which to calculate bit stat.
            %                   If empty, use entire image.
            %                   Default is [].
            %            'Show' - A logical iondicating if to display stat
            %                   on screen. Default is true.
            %            'DataProp' - Internal.
            % Output : - A Table of bit stat summary.
            %
            % Example: Stat = Bias.MaskData.bitStat
            
            arguments
                Obj
                Args.CCDSEC             = [];
                Args.Show logical       = true;
                Args.DataProp           = 'Image';
                
            end
            
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj).BitName = bitind2name(Obj(Iobj).Dict, (0:1:Obj(Iobj).Dict.Nbit-1) ); % should we subtract 1???
                Result(Iobj).SumBit  = zeros(1,Obj(Iobj).Dict.Nbit);
                Result(Iobj).FracBit = zeros(1,Obj(Iobj).Dict.Nbit);
                Npix                 = numel(Obj(Iobj).Image);
                for Ibit=0:1:Obj(Iobj).Dict.Nbit-1
                    [Flag] = findBit(Obj(Iobj), Result(Iobj).BitName{Ibit+1}, 'Method','any', 'OutType','mat', 'DataProp',Args.DataProp);
                    Result(Iobj).SumBit(Ibit+1)  = sum(Flag,'all');
                    Result(Iobj).FracBit(Ibit+1) = Result(Iobj).SumBit(Ibit+1)./Npix;
                end
                Tab(Iobj).BitSummary = cell2table([Result(Iobj).BitName(:), num2cell(Result(Iobj).SumBit)', num2cell(Result(Iobj).FracBit)']);
                Tab(Iobj).BitSummary.Properties.VariableNames = {'BitMask Name','Number','Fraction'};
                if Args.Show
                    Tab(Iobj).BitSummary
                end
            end
            
            
            
        end
        
    end
    

    methods (Static) % Unit-Test
        Result = unitTest()

    end
    
end

           

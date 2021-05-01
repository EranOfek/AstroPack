% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

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
       

    end
  
    methods % Setters/Getters

        
    end
    
    methods (Static)  % static methods
       
    end
    
    methods % functionality
        function Result = maskSet(Obj, Flag, BitName, SetVal, Args)
            % Set the value of a bit in a bit mask
            % Input  : - An ImageMask Object.
            %          - A matrix of logical, with the same size as the
            %            Image in the ImageMask, in which values which are
            %            true will be set.
            %          - Bit name, or bit index (start from 0), to set.
            %          - Value to set (0 | 1). Default is 1.
            %          * ...,key,val,...
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            % Output : - An ImageMaks object.
            % Author : Eran Ofek (May 2021)
            % Example: 
            %       MI=MaskImage;
            %       MI.Dict=BitDictionary('BitMask.Image.Default')
            %       Flag = false(3,3); Flag(1,2)=true;
            %       Result = MI.maskSet(Flag,'Saturated')
            %       Result = MI.maskSet(Flag,'Streak')
            
            arguments
                Obj
                Flag logical                 % matrix of logicals
                BitName                      % name or bit index (start with zero)
                SetVal                 = 1;
                Args.CreateNewObj      = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Result = Obj;
                else
                    % create new obj
                    Result = Obj.copyObject;
                end
            else
                if Args.CreateNewObj
                    Result = Obj.copyObject;
                else
                    Result = Obj;
                end
            end
                    
            Nobj = numel(Obj);
            
            % a single Flag image
            SizeImage = size(Flag);
            for Iobj=1:1:Nobj
                if isnumeric(BitName)
                    BitInd = BitName + 1;
                else
                    BitInd = Obj(Iobj).Dict.name2bit(BitName) + 1;
                end

                if isempty(Obj(Iobj).Image)
                    % no maks image - allocate
                    Obj(Iobj).Image = Obj(Iobj).Dict.Class(zeros(SizeImage));
                end

                Result(Iobj).Image = bitset(Obj(Iobj).Image, BitInd, SetVal);
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
                if iscell(BitNames) || isstring(BitNames)
                    [BitInd,BitDec,SumBitDec,BitDescription] = name2bit(Obj(Iobj), BitNames);
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
                XY  = imUtil.image.ind2sub_fast(size(Flag), Ind);
            end
            
            
        end
        
        
%         function bitwise_cutout(Obj, Operator, Args)
%             % Apply bitwise operator to a cutouts
%             
%             arguments
%                 Obj
%                 Operator function_handle
%                 Args.XY
%                 Args.Radius
%                 Args.Shape
%             end
%            
%         end
        
    end
    
    methods % bit statistics
        function Result = bitStat(Obj, Args)
            %
            
            arguments
                Obj
                Args.CCDSEC             = [];
                Args.DataProp           = 'Image';
            end
            
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj).BitName = bitind2name(Obj(Iobj), (1:1:Obj(Iobj).Dict.Nbit) ); % should we subtract 1???
                Result(Iobj).SumBit  = zeros(1,Obj(Iobj).Dict.Nbit);
                for Ibit=0:1:Obj(Iobj).Dict.Nbit-1

                    [Flag] = findBit(Obj(Iobj), BitNames, 'Method','any', 'OutType','mat', 'DataProp',Args.DataProp);
                    Result(Iobj).SumBit(Ibit+1) = sum(Flag,'all');
                end
            end
            
            
        end
        
    end
    

    methods % Unit-Test
        function Result = unitTest()
            M = MaskImage;
            
            M = MaskImage({uint32(zeros(3,3))});
            Result = true;
        end
    end
    

end

            

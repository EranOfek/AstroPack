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
       
%         function Obj = MaskImage(FileNames)
%             %
%             
%             arguments
%                 FileNames       = [];
%             end
%             
%             if isempty(FileNames)
%                 Obj.MaskData = ImageComponent([]);
%             end
%             
%         end

    end
  
    methods % Setters/Getters
%         function Result = get.Image(Obj)
%             % getter for Image (from MaskData.Image)
%             Result = Obj.MaskData.Image;
%         end
%                 
%         function Obj = set.Image(Obj, Val)
%             % setter for Image (to MaskData.Image)
%             if ~isinteger(Val)
%                 error('Mask image must be integers');
%             end
%             Obj.MaskData.Image = Val;
%         end
%         
%         function Result = get.Data(Obj)
%             % getter for Image (from MaskData.Data)
%             Result = Obj.MaskData.Data;
%         end
%         
%         function Obj = set.Data(Obj, Val)
%             % setter for Image (to MaskData.Data)
%             if ~isinteger(Val)
%                 error('Mask image must be integers');
%             end
%             Obj.MaskData.Data = Val;
%         end   
%         
%         function Result = get.Scale(Obj)
%             % getter for Scale (from MaskData.Scale)
%             Result = Obj.MaskData.Scale;
%         end
%                 
%         function Obj = set.Scale(Obj, Val)
%             % setter for Scale (to MaskData.Scale)
%             
%             Obj.MaskData.Scale = Val;
%         end
        
    end
    
    methods (Static)  % static methods
       
    end
    
    methods % functionality
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
%         function [BitDec, BitNames] = bitNames(Obj, XY)
%             % 
%             
%         end
%         
%         function Result = bitStat(Obj)
%             % Return the bit mask statistics
%             
%         end
        
    end
    

    methods % Unit-Test
        function Result = unitTest()
            M = MaskImage;
            
            M = MaskImage({uint32(zeros(3,3))});
            Result = true;
        end
    end
    

end

            

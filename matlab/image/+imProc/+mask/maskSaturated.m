function [Result, FlagSat, FlagNL] = maskSaturated(Obj, Args)
    % set mask bits for saturated and non-linear pixels
    % Input  : - An AstroImage object (multi elements supported).
    %          * ...,key,val,...
    %            'SatLevel' - A saturation level - A pixel above this level
    %                   will be set in the mask image. If empty, then will
    %                   attempt to read the level from the header using the
    %                   keyword in the 'SatKey' argument.
    %                   If also the level is not found in the header then
    %                   the mask will not be set.
    %                   Default is [].
    %            'SatKey' - Saturation level header keyword name.
    %                   Default is 'SATURVAL'.
    %            'BitName_Saturated' - Bit name of the saturated pixel.
    %                   Default is 'Saturated'.
    %            
    %            'MultLevelByGain' - A logical indicating if the saturation and non-linear
    %                   levels are needed to be multiplied by the gain.
    %                   Default is false.
    %            'Gain' - Either the gain value or an header keyword name from
    %                   which to read the gain. 
    %                   If gain is not available in header then will be set
    %                   to 1. Default is 'GAIN'.
    %            'SatPix2NaN' - A logical indicating if to replace
    %                   saturated pixels with NaN. Default is false.
    %
    %            'NonLinLevel' - Like 'SatLevel', but for non-linearity.
    %                   Default is [].
    %            'NonLinKey' - Like 'SatKey' but for non-linearity.
    %                   Default is 'NONLIN'.
    %            'BitName_NonLinear' - Like 'BitName_Saturation'.
    %                   Default is 'NonLinear'
    %            'NonLinPix2NaN' - A logical indicating if to replace
    %                   non-linear pixels with NaN. Default is false.
    %           
    %            'DefBitDict' - Default bit dictionary if
    %                   not exist. Default is
    %                   BitDictionary('BitMask.Image.Default').
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   Default is false.
    %            Some additional hidden arguments
    % Output : - An AstroImage object in which the MaskData is set with
    %            saturated and non-linear pixels.
    %          - A matrix of logicals (for the latest images) indicating
    %            saturated pixels.
    %          - A matrix of logicals (for the latest images) indicating
    %            non-linear pixels.
    % Author : Eran Ofek (May 2021)
    % Example: AI = AstroImage({rand(10,10).*1000});
    %          Result = imProc.mask.maskSaturated(AI, 'SatLevel',500)
    %          Result = imProc.mask.maskSaturated(AI, 'SatLevel',500, 'NonLinLeve',100)
    %          Result = imProc.mask.maskSaturated(AI, 'SatLevel',500, 'NonLinLeve',100,'MultLevelByGain',true,'Gain',5);
    
    arguments
        Obj AstroImage
        
        Args.SatLevel                  = [];    % in ADUs
        Args.SatKey                    = 'SATURVAL';
        Args.BitName_Saturated char    = 'Saturated';
        Args.SatPix2NaN(1,1) logical   = false;
        
        Args.MultLevelByGain(1,1) logical = false;
        Args.Gain                      = 'GAIN';
        
        Args.NonLinLevel               = [];    % in ADUs
        Args.NonLinKey                 = 'NONLIN';
        Args.BitName_NonLinear char    = 'NonLinear';
        Args.NonLinPix2NaN(1,1) logical= false;
        
        Args.DefBitDict                = BitDictionary('BitMask.Image.Default');
        Args.CreateNewObj              = false;
        
        % hidden
        Args.ImageProp char            = 'ImageData';
        Args.ImagePropIn char          = 'Image';
        Args.MaskProp char             = 'MaskData';
        Args.MaskPropIn char           = 'Image';
        
    end
    
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % Check units
        if Args.MultLevelByGain
            % read gain
            if isnumeric(Args.Gain)
                Gain = Args.Gain;
            elseif ischar(Args.Gain)
                % read from header
                Gain = Obj(Iobj).HeaderData.getVal(Args.Gain);
                if isnan(Gain)
                    % gain is not found in header - set to 1
                    Gain = 1;
                end
            else
                error('Unknown Gain option');
            end
        else
            Gain = 1;
        end
        
        % get stauration level for image
        if isempty(Args.SatLevel)
            % get from header
            SatLevel = Obj(Iobj).HeaderData.getVal(Args.SatKey);
        else
            SatLevel = Args.SatLevel;
        end
        % find saturated pixels
        if ~isnan(SatLevel)
            FlagSat = Obj(Iobj).(Args.ImageProp).(Args.ImagePropIn) > (SatLevel.*Gain);
            % set bit mask
            maskSet(Result(Iobj).(Args.MaskProp), FlagSat, Args.BitName_Saturated, 1, 'CreateNewObj',false, 'DefBitDict',Args.DefBitDict);
            
            if Args.SatPix2NaN
                Result(Iobj).(Args.MaskProp).(Args.ImagePropIn)(FlagSat) = NaN;
            end
        end
        
        % get non-lin level for image
        if isempty(Args.NonLinLevel)
            % get from header
            NonLinLevel = Obj(Iobj).HeaderData.getVal(Args.NonLinKey);
        else
            NonLinLevel = Args.NonLinLevel;
        end
        % find non-linear pixels
        if ~isnan(NonLinLevel)
            FlagNL  = Obj(Iobj).(Args.ImageProp).(Args.ImagePropIn) > (NonLinLevel.*Gain);
            % set bit mask
            maskSet(Result(Iobj).(Args.MaskProp), FlagNL, Args.BitName_NonLinear, 1, 'CreateNewObj',false, 'DefBitDict',Args.DefBitDict);
            
            if Args.NonLinPix2NaN
                Result(Iobj).(Args.MaskProp).(Args.ImagePropIn)(FlagNL) = NaN;
            end
        end
    end
        
end
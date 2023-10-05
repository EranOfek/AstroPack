function Result = interpOverMaskedPix(Obj, Args)
    % Interpolate over pixels with specific bit mask
    % Input  : - An AstroImage object array.
    %          * ...,key,val,...
    %            'BitNamesToInterp' - A bit name, or a cell array of bit
    %                   names. Pixels in the Image which their
    %                   corresponding bit mask pixel is on, will be
    %                   interpolated over.
    %                   Default is {'Saturated','HighRN','DarkHighVal','Hole','Spike','CR_DeltaHT'};
    %            'interpOverNanArgs' - A cell array of additional arguments
    %                   to pass to imProc.image.interpOverNan.
    %                   Default is {}.
    %            'BitNameInterpolated' - A bit name marking interpolated
    %                   pixels. If empty, ignore, otherwise, for any interpolated
    %                   pixel, set this bit to on.
    %                   Default is 'Interpolated'.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   object. Default is false.
    % Output : - An AstroImage object in which the 'Image' data bad pixels
    %            are interpolated over.
    % Author : Eran Ofek (Nov 2021)
    % Example: Result = imProc.mask.interpOverMaskedPix(Obj);
    
    arguments
        Obj AstroImage
        Args.BitNamesToInterp        = {'Saturated','HighRN','DarkHighVal','Hole','Spike','CR_DeltaHT'};
        Args.interpOverNanArgs       = {};
        Args.BitNameInterpolated     = 'Interpolated';
        Args.CreateNewObj logical    = false;
        
    end
    
    if ischar(Args.BitNamesToInterp)
        % convert to cell array
        Args.BitNamesToInterp = {Args.BitNamesToInterp};
    end
    
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        [~, ~, Ind] = findBit(Obj(Iobj).MaskData, Args.BitNamesToInterp, 'Method','any');
        % set selected pixels to NaN
        Result(Iobj).Image(Ind) = NaN;

        if numel(Ind)>1e5
            % too many band pixels
            error('Too many bad pixels: %d',numel(Ind));
        end
     
        % interpolate over staurated pixels

        Result(Iobj) = imProc.image.interpOverNan(Result(Iobj),...
                                                 Args.interpOverNanArgs{:},...
                                                 'CreateNewObj',false);
                                             
        % set Interpolate bit mask to on
        Result(Iobj).MaskData = maskSet(Result(Iobj).MaskData, Ind, Args.BitNameInterpolated, 1);
    end
     
end
    
    
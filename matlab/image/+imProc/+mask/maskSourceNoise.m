function Obj = maskSourceNoise(Obj, Args)
    % Mask pixels which are dominated by source noise (rather than background noise).
    %       If Image, Back, and Var properties are populated then set the
    %       mask SrcNoiseDominated bit to true if (Image-Back)> Factor*Var.
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'Factor' - A factor to multiply the variance.
    %                   Default is 1.
    %            'CreateNewObj' - Create a new mask (only) object.
    %                   Default is false.
    % Output : - An AstroImage object with the populated MaskData.
    % Author : Eran Ofek (Dec 2021)
    % Example: Obj = imProc.mask.maskSourceNoise(Obj);
    
    arguments
        Obj AstroImage
        Args.Factor                 = 1;
        Args.BitName                = 'SrcNoiseDominated';
        
        Args.CreateNewObj logical   = false;   % for mask only
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each image
        if ~isempty(Obj(Iobj).Image) && ~isempty(Obj(Iobj).Back) && ~isempty(Obj(Iobj).Var)
            FlagSourceNoise = (Obj(Iobj).Image - Obj(Iobj).Back) > Args.Factor.*Obj(Iobj).Var;
            Obj(Iobj).MaskData = maskSet(Obj(Iobj).MaskData, FlagSourceNoise, Args.BitName, 1, 'CreateNewObj',Args.CreateNewObj);
        end
    end
end
    
    
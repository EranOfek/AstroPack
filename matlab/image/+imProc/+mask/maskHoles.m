function Result = maskHoles(Obj, Args)
    % Search for holes in images and add a bit mask marking the hole central position.
    %       Holes are defined as negative sources (created e.g., by stars
    %       in flat images).
    % Input  : - An AstroImage object. If the 'SN' argument is not provided,
    %            then the object must contains the .Back and .Var
    %            properies.
    %          * ...,key,val,...
    %            'Threshold' - Search threshold [sigmas]. Default is 5.
    %            'Template' - Template to search.
    %                   Default is @imUtil.kernel2.gauss.
    %            'PsfFunPar' - Cell array of parameters to pass to the
    %                   template function. Default is {[1.2]}.
    %            'SN' - A S/N image. If empty, then will bec calculated
    %                   using imUtil.filter.filter2_snBank and the 'Template'.
    %                   Default is [].
    %            'Conn' - Connectivity parameter to pass to imUtil.image.local_maxima
    %                   Default is 8.
    %            'HoleBitName' - Bit name in the Mask. This bit will be
    %                   masked. Default is 'Hole'.
    %            'CreateNewObj' - Create a new object. Default is false.
    % Output : - An AstroImage array in which the Mask image is populated
    %            with the hole bit.
    % Author : Eran Ofek (Nov 2021)
    % Example: Result = maskHoles(Obj);
    
   
    arguments
        Obj AstroImage
        
        Args.Threshold               = 5;
        Args.Template                = @imUtil.kernel2.gauss;
        Args.PsfFunPar cell          = {[1.2]};
        Args.SN                      = [];   % S/N map - generate if empty
        Args.Conn                    = 8;
        
        Args.HoleBitName             = 'Hole';
        Args.CreateNewObj logical    = false;
        
    end
        
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each image
        
        % Verify Back and Var exist
        if isempty(Obj(Iobj).Back) || Obj(Iobj).Var
            error('Back and Var properties must exist - execute background');
        end
        
        if isempty(Args.SN)
            [SN]    = imUtil.filter.filter2_snBank(Obj(Iobj).Image, Obj(Iobj).Back, Obj(Iobj).Var, Args.Template, Args.PsfFunPar{:});
        else
            SN = Args.SN;
        end
        % note calling -SN
        [~,Pos] = imUtil.image.local_maxima(-SN, 1, Args.Threshold, Args.Conn);
        % Pos contains [X,Y,SN,IndexTemplate]
        
        % mask positions (only peak pixel is masked)
        Ind                   = imUtil.image.sub2ind_fast(size(Obj(Iobj).MaskData), Pos(:,2), Pos(:,1));
        Result(Iobj).MaskData = maskSet(Obj(Iobj).MaskData, Ind, Args.HoleBitName, 1);
        
    end
    
    
end

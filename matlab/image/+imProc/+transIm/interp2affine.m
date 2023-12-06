function Result=interp2affine(Obj, AffineTran, Args)
    % Transform an image by an affine transformation, using interp2.
    % Input  : - An AstroImage object.
    %          - An affine2d, or affinetform2d object, or a two column
    %            matrix. If a two column matrix, then the columns are the X
    %            and Y shifts.
    %          * ...,key,val,...
    %            'InterpMethod' - Interpolation method for images.
    %                   See interp2 for options.
    %                   Default is 'cubic'.
    %            'InterpMethodMask' - Interpolation method for the mask
    %                   image. Default is 'nearest'.
    %            'DataProp' - data properties in the AstroImage to
    %                   interpolate.
    %                   Default is {'Image','Mask'}.
    %            'ExtrapVal' - Extrapolation value. Default is NaN.
    %            'CopyPSF' - Copy PSF from input image. Default is true.
    %            'WCS' - An optional AstroWCS or AstroImage to insert (WCS) into the registered image.
    %                   If empty, then do not insert WCS.
    %                   Default is [].
    %            'CopyWCS' - Copy WCS from input image. Default is true.
    %            'CopyHeader' - Copy Header from input image. Default is true.
    %                   If CopyWCS is true, then will update header by the
    %                   WCS.
    %            'CreateNewObj' - A logical indicating if the copy
    %                   operations of PSF, WCS, Header will be create a new
    %                   copy of the handle object.
    %                   Default is true.
    % Output : - A registered AstroImage, with the optional WCS copied from
    %            the provided WCS.
    % Author : Eran Ofek (Jun 2023)
    % Example: AI = AstroImage({imUtil.kernel2.gauss});
    %          AIreg=imProc.transIm.interp2affine(AI, [3 3])

    arguments
        Obj AstroImage
        AffineTran
        Args.InterpMethod             = 'cubic';
        Args.InterpMethodMask         = 'nearest';
        Args.DataProp                 = {'Image','Mask'};
        Args.ExtrapVal                = NaN;
        Args.CopyPSF logical          = true;
        Args.WCS                      = [];
        Args.CopyWCS logical          = true;
        Args.CopyHeader logical       = true;
        Args.CreateNewObj logical     = true;

        Args.Sampling                 = 20;
        
    end
    
    warning('NOT TESTED');

    if isempty(Args.WCS)
        Args.CopyWCS = false;
    end

    
    if isnumeric(AffineTran)
        Nref = size(AffineTran, 1);
    else
        Nref  = numel(AffineTran);
    end
    Nwcs = numel(Args.WCS);
    Nprop = numel(Args.DataProp);

    Nobj=numel(Obj);
    Result = AstroImage(size(Obj));
    for Iobj=1:1:Nobj
        Iref = min(Iobj, Nref);
        
        SizeIm = size(Obj(Iobj).Image);
        CCDSEC = [1 SizeIm(2) 1 SizeIm(1)];
        
        VecX = (1:1:SizeIm(2));
        VecY = (1:1:SizeIm(1));
        [MatX, MatY] = meshgrid(VecX, VecY);
        
        switch class(AffineTran)
            case 'affine2d'
                [FullRefX, FullRefY] = transformPointsForward(AffineTran(Iref), MatX, MatY);
                
            case 'affinetform2d'
                [FullRefX, FullRefY] = transformPointsForward(AffineTran(Iref), MatX, MatY);
                
            otherwise
                % assume numeric input
                switch size(AffineTran,2)
                    case 2
                        FullRefX = MatX - AffineTran(Iref,1);
                        FullRefY = MatY - AffineTran(Iref,2);
                    otherwise
                        error('Numeric transformation - only two columns option is supported');
                end
        end
       
        for Iprop=1:1:Nprop
            if ~isempty(Obj(Iobj).(Args.DataProp{Iprop}))
                switch Args.DataProp{Iprop}
                    case 'Mask'
                        Result(Iobj).(Args.DataProp{Iprop}) = interp2(VecX, VecY, Obj(Iobj).(Args.DataProp{Iprop}), FullRefX, FullRefY, Args.InterpMethodMask, Args.ExtrapVal);
                    otherwise
                        Result(Iobj).(Args.DataProp{Iprop}) = interp2(VecX, VecY, Obj(Iobj).(Args.DataProp{Iprop}), FullRefX, FullRefY, Args.InterpMethod, Args.ExtrapVal);
                end
            end
        end

        if Args.CopyPSF
            if Args.CreateNewObj
                Result(Iobj).PSFData = Obj(Iobj).PSFData.copy;
            else
                Result(Iobj).PSFData = Obj(Iobj).PSFData;
            end
        end
        if Args.CopyWCS
            Iwcs = min(Iobj, Nwcs);
            if isa(Args.WCS, 'AstroWCS')
                if Args.CreateNewObj
                    Result(Iobj).WCS = Args.WCS(Iwcs).copy;
                else
                    Result(Iobj).WCS = Args.WCS(Iwcs);
                end
            elseif isa(Args.WCS, 'AstroImage')
                if Args.CreateNewObj
                    Result(Iobj).WCS = Args.WCS(Iwcs).WCS.copy;
                else
                    Result(Iobj).WCS = Args.WCS(Iwcs).WCS;
                end
            else
                error('WCS argument must be AstroWCS or AstroImage'); 
            end
        end
        if Args.CopyHeader
            if Args.CreateNewObj
                Result(Iobj).HeaderData = Obj(Iobj).HeaderData.copy;
            else
                Result(Iobj).HeaderData = Obj(Iobj).HeaderData;
            end

            % update header
            if Args.CopyWCS
                Result(Iobj).HeaderData = wcs2header(Result(Iobj).WCS, Result(Iobj).HeaderData);
            end
        end

    end

end
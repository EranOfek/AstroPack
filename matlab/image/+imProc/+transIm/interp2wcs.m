function Result=interp2wcs(Obj, Ref, Args)
    % Transform an image with a WCS into a new grid defined by a ref WCS, using interp2.
    % Input  : - An AstroImage object, with WCS updated.
    %            Use populateWCS to update WCS from the header.
    %          - An AstroWCS object, or AstroImage containing AstroWCS.
    %            This is either a single element object, or have the same
    %            number of elements as in the first input argument.
    %            This is the reference WCS on which the input image will be
    %            interpolated.
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
    %            'CopyWCS' - Copy WCS from input image. Default is true.
    %            'CopyHeader' - Copy Header from input image. Default is true.
    %                   If CopyWCS is true, then will update header by the
    %                   WCS.
    %            'CreateNewObj' - A logical indicating if the copy
    %                   operations of PSF, WCS, Header will be create a new
    %                   copy of the handle object.
    %                   Default is true.
    %            'Sampling' - AstroWCS/xy2refxy sampling parameter.
    %                   Default is 20.
    % Output : - An AstroImage registered to the reference WCS.
    % Author : Eran Ofek (Jun 2023)
    % Example: AIreg1=imProc.transIm.interp2wcs(AI, AI(1))

    arguments
        Obj AstroImage
        Ref
        Args.InterpMethod             = 'cubic';  % 'makima'
        Args.InterpMethodMask         = 'nearest';
        Args.DataProp                 = {'Image','Mask'};
        Args.ExtrapVal                = NaN;
        Args.CopyPSF logical          = true;
        Args.CopyWCS logical          = true;
        Args.CopyHeader logical       = true;
        Args.CreateNewObj logical     = true;

        Args.Sampling                 = 20;
        
    end

    % if Args.CreateNewObj
    %     Result = Obj.copy;
    % else
    %     Result = Obj;
    % end


    if isa(Ref, 'AstroWCS')
        RefWCS = Ref;
    elseif isa(Ref, 'AstroImage')
        Nref = numel(Ref);
        RefWCS = AstroWCS(size(Ref));
        for Iref=1:1:Nref
            RefWCS(Iref) = Ref(Iref).WCS;
        end
    else
        error('2nd input must be an AstroWCS or AstroImage object');
    end
    Nref  = numel(RefWCS);
    Nprop = numel(Args.DataProp);

    Nobj=numel(Obj);
    Result = AstroImage(size(Obj));
    for Iobj=1:1:Nobj
        Iref = min(Iobj, Nref);

        SizeIm = size(Obj(Iobj).Image);
        CCDSEC = [1 SizeIm(2) 1 SizeIm(1)];
        %[RefX, RefY, X, Y] = Obj(Iobj).WCS.xy2refxy(CCDSEC, RefWCS(Iref), 'Sampling',Args.Sampling);
        [RefX, RefY, X, Y] = RefWCS(Iref).xy2refxy(CCDSEC, Obj(Iobj).WCS, 'Sampling',Args.Sampling);

        VecX = (1:1:SizeIm(2));
        VecY = (1:1:SizeIm(1));

        SizeRefIm = size(Ref(Iref).Image);
        VecRefX = (1:1:SizeRefIm(2));
        VecRefY = (1:1:SizeRefIm(1));

        FullRefX = interp2(X, Y, RefX, VecRefX(:).', VecRefY(:), 'cubic');
        FullRefY = interp2(X, Y, RefY, VecRefX(:).', VecRefY(:), 'cubic');

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
            if Args.CreateNewObj
                Result(Iobj).WCS = RefWCS(Iref).copy;
            else
                Result(Iobj).WCS = RefWCS(Iref);
            end
            %Result(Iobj).WCS = Obj(Iobj).WCS.copy;
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
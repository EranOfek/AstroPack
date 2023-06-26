function Result=interp2wcs(Obj, Ref, Args)
    % Intepolate an image with a WCS into a new grid defined by a ref WCS.
    % Input  : -
    % Output : - 
    % Author : Eran Ofek (Jun 2023)
    % Example: AIreg1=imProc.transIm.interp2wcs(AI, AI(1))

    arguments
        Obj AstroImage
        Ref
        Args.InterpMethod             = 'cubic';
        Args.InterpMethodMask         = 'nearest';
        Args.DataProp                 = {'Image','Mask'};
        Args.ExtrapVal                = 0;
        Args.CopyPSF logical          = true;
        Args.CopyWCS logical          = true;
        Args.CopyHeader logical       = true;
        Args.CreateNewObj logical     = false;

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
        FullRefX = interp2(X, Y, RefX, VecX(:).', VecY(:));
        FullRefY = interp2(X, Y, RefY, VecX(:).', VecY(:));

        for Iprop=1:1:Nprop
            switch Args.DataProp{Iprop}
                case 'Mask'
                    Result(Iobj).Image = interp2(VecX, VecY, Obj(Iobj).(Args.DataProp{Iprop}), FullRefX, FullRefY, Args.InterpMethodMask, Args.ExtrapVal);
                otherwise
                    Result(Iobj).Image = interp2(VecX, VecY, Obj(Iobj).(Args.DataProp{Iprop}), FullRefX, FullRefY, Args.InterpMethod, Args.ExtrapVal);
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
                Result(Iobj).WCS = Obj(Iobj).WCS.copy;
            else
                Result(Iobj).WCS = Obj(Iobj).WCS;
            end
        end
        if Args.CopyHeader
            if Args.CreateNewObj
                Result(Iobj).HeaderData = Obj(Iobj).HeaderData.copy;
            else
                Result(Iobj).HeaderData = Obj(Iobj).HeaderData;
            end

            % update header
            Result(Iobj).HeaderData = wcs2header(Obj(Iobj).WCS, Result(Iobj).HeaderData);
        end


            

    end
        



end
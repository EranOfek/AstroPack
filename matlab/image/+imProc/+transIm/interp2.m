function [Result] = interp2(Obj, TransRef, Args)
    % Interpolate/register images in AstroImage into a new reference frame.
    %   Register images to a common frame.
    % Input  : - AstroImage/AstroDiff/AstroZOGY object containing images.
    %          - One of the following options:
    %               An AstroWCS object, or AstroImage containing AstroWCS.
    %            This is either a single element object, or have the same
    %            number of elements as in the first input argument.
    %            This is the reference WCS on which the input image will be
    %            interpolated.
    %               An affine2d, or affinetform2d object, or a two column
    %            matrix. If a two column matrix, then the columns are the X
    %            and Y shifts.
    %          * ...,key,val,... 
    %            'InterpMethod' - Interpolation method for images.
    %                   See interp2 for options.
    %                   Default is 'cubic'.
    %            'InterpMethodBackVar' - Interpolation method for Back and
    %                   Var. Default is 'linear'.
    %            'InterpMethodMask' - Interpolation method for the mask
    %                   image. Default is 'nearest'.
    %            'DataProp' - data properties in the AstroImage to
    %                   interpolate.
    %                   Default is {'Image','Mask'}.
    %            'ExtrapVal' - Extrapolation value. Default is NaN.
    %            --- Other properties ---
    %            'CopyPSF' - Copy PSF from input image. Default is true.
    %            'CopyWCS' - Copy WCS from input image. Default is true.
    %            'CopyHeader' - Copy Header from input image. Default is true.
    %                   If CopyWCS is true, then will update header by the
    %                   WCS.
    %            'CreateNewObj' - A logical indicating if the copy
    %                   operations of PSF, WCS, Header will be create a new
    %                   copy of the handle object.
    %                   Default is true.
    %            --- Aux ---
    %            'Sampling' - AstroWCS/xy2refxy sampling parameter.
    %                   The WCS is evaluated with this steps and
    %                   interpolated. For linear transformations, higher
    %                   values may be used.
    %                   Default is 20.
    % Output : - An AstroImage object containing the registered images.
    % Author : Eran Ofek (2026 Mar) 
    % Example: R=imProc.transIm(AI, AI(1));

    arguments
        Obj
        TransRef
        Args.InterpMethod             = 'mex_lanczos3'; % 'cubic'; %'mex_lanczos3';
        Args.InterpMethodBackVar      = 'mex_bilinear'; % 'linear'; %'mex_bilinear';
        Args.InterpMethodMask         = 'mex_nearest'; % 'nearest'; %'mex_nearest';
        Args.ExtrapVal                = NaN; % for mex_ interpolation options this is always NaN
        Args.DataPropOut              = {'ImageData','MaskData'};
        Args.DataPropIn               = 'Data';

        Args.CopyPSF logical          = true;
        Args.CopyWCS logical          = true;
        Args.CopyHeader logical       = true;
        Args.CreateNewObj logical     = true;
        Args.CopyFilename logical     = true;

        Args.Sampling                 = 20;
    end


    Nprop = numel(Args.DataProp);
    Nobj = numel(Obj);
    Result = AstroImage(size(Obj));
    for Iobj=1:1:Nobj
        SizeIm = size(Obj(Iobj).ImageData.Data);
        CCDSEC = [1 SizeIm(2) 1 SizeIm(1)];
        
        VecX = cast((1:1:SizeIm(2)), 'like',Obj(Iobj).ImageData.Data);
        VecY = cast((1:1:SizeIm(1)), 'like',VecX);

        % Transformation types
        switch class(TransRef)
            case 'AstroWCS'
                Iref = min(Iobj, Nref);

                % Object is AstroWCS
                [RefX, RefY, X, Y] = Obj(Iref).xy2refxy(CCDSEC, Obj(Iobj).WCS, 'Sampling',Args.Sampling);
                
                SizeRefIm = size(Ref(Iref).Image);
                VecRefX = (1:1:SizeRefIm(2));
                VecRefY = (1:1:SizeRefIm(1));
        
                FullRefX = interp2(X, Y, RefX, VecRefX(:).', VecRefY(:), 'cubic');
                FullRefY = interp2(X, Y, RefY, VecRefX(:).', VecRefY(:), 'cubic');

            case {'AstroImage', 'AstroZOGY', 'AstroDiff'}
                Iref = min(Iobj, Nref);

                % WCS in AstroImage:
                [RefX, RefY, X, Y] = Obj(Iref).WCS.xy2refxy(CCDSEC, Obj(Iobj).WCS, 'Sampling',Args.Sampling);
        
                SizeRefIm = size(Ref(Iref).Image);
                VecRefX = (1:1:SizeRefIm(2));
                VecRefY = (1:1:SizeRefIm(1));
        
                FullRefX = interp2(X, Y, RefX, VecRefX(:).', VecRefY(:), 'cubic');
                FullRefY = interp2(X, Y, RefY, VecRefX(:).', VecRefY(:), 'cubic');

            case {'affine2d', 'affinetform2d'}
                VecX = cast((1:1:SizeIm(2)), 'like',Obj(Iobj).ImageData.Data);
                VecY = cast((1:1:SizeIm(1)), 'like',VecX);
                [MatX, MatY] = meshgrid(VecX, VecY);
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
        end % switch class(TransRef)

        % Interpolation        
        for Iprop=1:1:Nprop
            if ~isempty(Obj(Iobj).(Args.DataProp{Iprop}))
                switch Args.DataProp{Iprop}
                    case 'Mask'
                        Result(Iobj).(Args.DataPropOut{Iprop}).(Args.DataPropIn) = tools.interp.interp2(VecX, VecY, Obj(Iobj).(Args.DataPropOut{Iprop}).(Args.DataPropIn), FullRefX, FullRefY, Args.InterpMethodMask, Args.ExtrapVal);
                    case {'Back','Var'}
                        Result(Iobj).(Args.DataPropOut{Iprop}).(Args.DataPropIn) = tools.interp.interp2(VecX, VecY, Obj(Iobj).(Args.DataPropOut{Iprop}).(Args.DataProp), FullRefX, FullRefY, Args.InterpMethodBackVar, Args.ExtrapVal);
                    otherwise
                        % Image part:
                        Result(Iobj).(Args.DataPropOut{Iprop}).(Args.DataPropIn) = tools.interp.interp2(VecX, VecY, Obj(Iobj).(Args.DataPropOut{Iprop}).(Args.DataPropIn), FullRefX, FullRefY, Args.InterpMethod, Args.ExtrapVal);
                end
            end
        end % for Iprop=1:1:Nprop

        % Fill the other proprties of the regsitered object:
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
        if Args.CopyFilename
            Result(Iobj).ImageData.FileName = Obj(Iref).ImageData.FileName;
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



    end % for Iobj=1:1:Nobj


end

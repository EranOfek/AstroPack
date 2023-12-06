function [Result]=imwarp(Obj, Trans, Args)
    % Apply the imwarp function on AstroImage object
    %   The Header, CatData, and PSF are not transformed.
    % Input  : - An AstroImage object
    %          - A transformation information, or reference.
    %            This can be one of the following:
    %            1. A two column [X Y] shift matrix
    %            2. An 3x3 affine transformation
    %            3. A cube containing a displacment field - 3rd dim
    %               for X,Y displacments.
    %            4. AstroImage with AstroWCS
    %            5. AstroWCS
    %            6. Tran2D object
    %            7. An affine transformation object
    %               struct array with displacement fields in DF field.
    %   
    %          * ...,key,val,...
    %            'DeleteCat' - Delete the catalog (CatData) for the output
    %                   AstroImage. If 'CreateNeObj'=false, then this will
    %                   also delete the CatData property from the input
    %                   AstroImage. Default is true.
    %            'DeleteWCS' - The same as DeleteCat, but for the WCS
    %                   information.
    %                   Note that if TransWCS=true, then the WCS of the
    %                   registerd image will be repopulated with the
    %                   correct WCS.
    %                   Default is true.
    %            'DeleteHeader' - The same as DeleteCat, but for the
    %                   HeaderData property. Default is false.
    %            'DataProp' - A cell array of data properies in the input
    %                   AstroImage on which to operate the imwarp transformation
    %                   with the interpolation provided by the InterpMethod 
    %                   argument.
    %                   Default is {'Image', 'Back', 'Var'}.
    %            'DataPropMask' - A char array of a MASK field in the
    %                   AstroImage, on which to operate the imwarp transformation
    %                   with the interpolation provided by the InterpMethodMask
    %                   argument.
    %                   If empty, then do not warp the Mask image.
    %                   Default is 'Mask'.
    %
    %            'Sampling' - Sampling rate of WCS [pix]. Default is 10.
    %            'InterpMethod' -  An interpolation method for the images
    %                   in the properties listed in the DataProp argument.
    %                   Default is 'cubic'.
    %            'BoundsStyle' - BoindsStyle argument for the imwarp
    %                   function.
    %                   Do not modidy this argument unless you understand
    %                   what it is doing.
    %                   Default is 'CenterOutput'.
    %            'InterpMethodMask' - An interpolation method for the images
    %                   in the properties listed in the DataPropMask argument.
    %                   Default is 'nearest'.
    %
    %            'FillValues' - Replace NaNs/out of range with the
    %                   following numeric value. If a char array then will replace
    %                   with the image median.
    %                   Default is 'back'.
    %            'SmoothEdges' - A logical indicating if to smooth the
    %                   edges. See imwarp for details.
    %                   Default is true.
    %
    %            'ReplaceNaN' - Replace NaN with FillVall. Default is true.
    %            'TransWCS' - A logical indicating if to copy the reference WCS
    %                   to the transformed image WCS and header.
    %                   Default is true.
    %            'RefWCS' - A WCS to store in the output registered images.
    %                   If given this will override the TransWCS=true/false
    %                   option.
    %                   This is useful for a non-WCS transformations.
    %                   Default is empty (not used).
    %
    %            'CreateNewObj' - A logical indicating if the output is a
    %                   new copy of the input AstroImage (true), or just an handle
    %                   to the original input (false).
    %                   Default is true.
    %            'CopyPSF' - A logical indicating if to copy the PSF from
    %                   the original AstroImage to the transformed AstroImage.
    %                   Will create a new PSFData object.
    %                   Default is true.
    % Output : - An AstroImage object containing the input images registered to the reference images.
    % Author : Eran Ofek (Nov 2021)
    % Example: RegisteredAI = imProc.transIm.imwarp(AI, WCS);
    
    arguments
        Obj
        Trans
        Args.DeleteCat logical         = true;
        Args.DeleteWCS logical         = true;
        Args.DeleteHeader logical      = false;
        
        Args.DataProp                  = {'Image', 'Back', 'Var'};
        Args.DataPropMask              = 'Mask'

        Args.Sampling                  = 10;
        Args.InterpMethod              = 'cubic';
        Args.InterpMethodMask          = 'nearest';
        Args.BoundsStyle               = 'CenterOutput'; %'SameAsInput'; %'CenterOutput';
        
        Args.FillValues                = 'back';
        Args.SmoothEdges logical       = true;
        Args.ReplaceNaN logical        = true;  % replace NaN with FillVal | not working on mask
   
        Args.TransWCS logical          = true;
        Args.RefWCS                    = [];  % override input ref WCS
        
        Args.CreateNewObj logical      = true;
        Args.CopyHeader logical        = true; % copy header
        Args.CopyPSF logical           = true;
       
    end
    
    FieldName = 'DF';
    
    if Args.CreateNewObj
        Result  = Obj.copy;
    else
        Result  = Obj;
    end
    
    Nobj = numel(Obj);

    if ~isempty(Args.RefWCS)
        Args.TransWCS = true;
    end

    % Delete CatData
    if Args.DeleteCat
        if ~Args.CreateNewObj
            warning('Delete CatData object - will result in deleting the CatData object in the input AstroImage');
        end
        for Iobj=1:1:Nobj
            Result(Iobj).CatData = AstroCatalog;
        end
    end
    
    % Delete WCS
    if Args.DeleteWCS
        if ~Args.CreateNewObj
            warning('Delete WCS object - will result in deleting the WCS object in the input AstroImage');
        end
        for Iobj=1:1:Nobj
            Result(Iobj).WCS = AstroWCS;
        end
    end
    
    % Delete Header
    if Args.DeleteHeader
        if ~Args.CreateNewObj
            warning('Delete Header object - will result in deleting the Header object in the input AstroImage');
        end
        for Iobj=1:1:Nobj
            Result(Iobj).HeaderData = AstroHeader;
        end
    end
    
    % Treat the diffrent cases of Trans:
    IsDisplacment   = false;  % This parameter is set to true if using imwarp with displacment field / otherwise affine transformation
    % affine trans will be stored in: ImWarpTransformation array
    % displacment field will be stored in: DispField struct array
    if isnumeric(Trans)
        % Trans can be:
        % a two column [X Y] shift matrix
        % An 3x3 affine transformation
        % a cube containing a displacment field
        
        if size(Trans,2)==2
            % A two column matrix:
            % use ShiftXY
            % convert shifts to affine2d
            
            [NrowSh, NcolSh] = size(Trans);
            if NcolSh~=2
                error('Number of columns in ShiftXY must be 2');
            end
            
            for Itran=1:1:NrowSh
                ImWarpTransformation(Itran) = affine2d([1 0 0; 0 1 0; Trans(Itran,1) Trans(Itran,2) 1]);
            end
            Ntran = NrowSh;
        elseif all(size(Trans)==[3 3])
            % A 3x3 matrix
            % assume this is an affine transformation
            
            ImWarpTransformation = affine2d(Trans);
            Ntran                = numel(Trans);
            
        elseif ndims(Trans)==3 && size(Trans,3)==2
            % A 3D cube in which 3rd dim has size of 2
            % Assume this is a distortion field
            IsDisplacment            = true;
            DispField(1).(FieldName) = Trans;
            Ntran                    = 1;
        else
            error('Unknown Trans (2nd input argument) numeric option'); 
        end
        OutWCS = [];
        
    elseif isa(Trans, 'AstroWCS')
        % Convert AstroWCS to displacment field
        IsDisplacment   = true;
        
        [DispField, OutWCS] = imProc.transIm.wcs2displacment(Obj, Trans, 'Sampling',Args.Sampling, 'FieldName',FieldName);
        Ntran = numel(DispField);

    elseif isa(Trans, 'AstroImage')
        % Convert AstroWCS in AstroImage to displacment field
        IsDisplacment   = true;
        
        [DispField, OutWCS] = imProc.transIm.wcs2displacment(Obj, Trans, 'Sampling',Args.Sampling, 'FieldName',FieldName);
        Ntran = numel(DispField);

    elseif isa(Trans, 'struct')
        % assume as truct array of displacment fields
        % The struct must contains a .DF field
        IsDisplacment   = true;
        DispField      = Trans;
        OutWCS         = [];
        Ntran          = numel(DispField);
    elseif isa(Trans, 'affine2d')
        ImWarpTransformation = Trans;
        OutWCS   = [];
        Ntran    = numel(ImWarpTransformation);
    elseif isa(Trans, 'Tran2D')
        error('Tran2D option is not yet supported');
    else
        error('Unknown Trans (2nd input argument) object option'); 
    end
    
    % At this time the following variables are available:
    % if IsDisplacment=true: DispField,    otherwise: ImWarpTransformation
    % Also populated: Ntran, OutWCS
    
    
    Nprop = numel(Args.DataProp);

    for Iobj=1:1:Nobj
        
        % get the FillVal for each image:
        if ischar(Args.FillValues)
            % use median of background
            FillVal = median(Obj(Iobj).Back,[1 2],'omitnan');
        else
            FillVal = Args.FillValues;
        end
        
        SizeInput = size(Obj(Iobj).(Args.DataProp{1}));
        
        % treat the two cases:
        % transformation is in: DispField
        % transformation is in: ImWarpTransformation (affine)
        if IsDisplacment
            TranArg = DispField(Iobj).DF;
            OutView = [];

          

        else
            Ntran   = numel(ImWarpTransformation);
            Itran   = min(Ntran,Iobj);
            TranArg = ImWarpTransformation(Itran);
            % BoundsStyle is likely incorrect
            % see issue #105
            % Try: "SameAsInput"
            OutView = affineOutputView(SizeInput, ImWarpTransformation(Itran), 'BoundsStyle',Args.BoundsStyle);
        end
        
        % applying imwarp for each image property (except Mask):
        for Iprop=1:1:Nprop
            if ~isemptyImage(Obj(Iobj), Args.DataProp{Iprop})
                
                if isempty(OutView)
                    Result(Iobj).(Args.DataProp{Iprop}) = imwarp(Obj(Iobj).(Args.DataProp{Iprop}), TranArg, Args.InterpMethod,...
                                                    'FillValues',FillVal,...
                                                    'SmoothEdges',Args.SmoothEdges);
                else
                    Result(Iobj).(Args.DataProp{Iprop}) = imwarp(Obj(Iobj).(Args.DataProp{Iprop}), TranArg, Args.InterpMethod,...
                                                    'OutputView',OutView,...
                                                    'FillValues',FillVal,...
                                                    'SmoothEdges',Args.SmoothEdges);
                end
                if Args.ReplaceNaN
                    Result(Iobj).(Args.DataProp{Iprop})(isnan(Result(Iobj).(Args.DataProp{Iprop}))) = FillVal;
                    % FFU: update Mask
                end
            end

        end
        
        % mask transformation
        if ~isempty(Args.DataPropMask)
            if ~isemptyImage(Obj(Iobj), Args.DataPropMask)
                if isempty(OutView)
                    Result(Iobj).(Args.DataPropMask) = imwarp(Obj(Iobj).(Args.DataPropMask), TranArg, Args.InterpMethodMask,...
                                                    'FillValues',FillVal,...
                                                    'SmoothEdges',Args.SmoothEdges);
                else
                    Result(Iobj).(Args.DataPropMask) = imwarp(Obj(Iobj).(Args.DataPropMask), TranArg, Args.InterpMethodMask,...
                                                    'OutputView',OutView,...
                                                    'FillValues',FillVal,...
                                                    'SmoothEdges',Args.SmoothEdges);
                end
            end
        end
        
        % Delete additional properties:
        %Result(Iobj) = deleteProp(Result(Iobj), Args.DeleteProp);
        
        if Args.TransWCS
            % Transform the WCS into the new reference frame
            if isempty(OutWCS) && isempty(Args.RefWCS)
                warning('TransWCS=true option is not available for non-WCS transformations - ALternatively, give RefWCS');
            else
                if ~isempty(Args.RefWCS)
                    Result(Iobj).WCS = Args.RefWCS.copy;
                    Result(Iobj).HeaderData = wcs2header(Result(Iobj).WCS, Result(Iobj).HeaderData);
                else
                    Iwcs = min(Iobj, Ntran);
                    Result(Iobj).WCS = OutWCS(Iwcs);
                    Result(Iobj).HeaderData = wcs2header(Result(Iobj).WCS, Result(Iobj).HeaderData);
                end
            end
        end

        if Args.CopyPSF
            Result(Iobj).PSFData = Obj(Iobj).PSFData.copy;
        end
        
    end % for Iobj=1:1:Nobj
    
end
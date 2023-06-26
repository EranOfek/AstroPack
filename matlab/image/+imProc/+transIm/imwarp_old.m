function Result = imwarp(Obj, Trans, Args)
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
    %            'InterpMethod' -  An interpolation method for the images
    %                   in the properties listed in the DataProp argument.
    %                   Default is 'cubic'.
    %            'InterpMethodMask' - An interpolation method for the images
    %                   in the properties listed in the DataPropMask argument.
    %                   Default is 'nearest'.
    %            'DeleteProp' - A cell array of Data properties to delete
    %                   from the output AstroImage after the transformation was
    %                   applied. Default is {}.
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
    %            'TransWCS' - A logical indicatinf if to copy the reference WCS
    %                   to the transformed image WCS and header.
    %                   Default is true.
    %
    %            'CreateNewObj' - A logical indicating if the output is a
    %                   new copy of the input AstroImage (true), or just an handle
    %                   to the original input (false).
    %                   Default is true.
    %            'GetAllFields' - A logical indicating if to copy all
    %                   fields. If false, then only the header will be
    %                   copied (only if CopyHeader=true).
    %                   Default is false.
    %            'CopyHeader' - A logical indicating if to copy the header
    %                   (used only if GetAllFields=false).
    %                   Default is true.
    %            'CopYPSF' - A logical indicating if to copy the PSF from
    %                   the original AstroImage to the transformed AstroImage.
    %                   Will create a new PSFData object.
    %                   Default is true.
    % Output : - An AstroImage object containing the input images registered to the reference images.
    % Author : Eran Ofek (Nov 2021)
    % Example: imProc.transIm.imwarp
    
    arguments
        Obj AstroImage
        Trans                          = [0 0];
        Args.Sampling                  = 10;
        
        Args.DataProp                  = {'Image', 'Back', 'Var'};
        Args.DataPropMask              = 'Mask'
        Args.InterpMethod              = 'cubic';
        Args.InterpMethodMask          = 'nearest';
        Args.DeleteProp                = {};
        
        Args.FillValues                = 'back';
        Args.SmoothEdges logical       = true;
        
        Args.ReplaceNaN logical        = true;  % replace NaN with FillVal
   
        Args.TransWCS logical          = true;
        
        Args.CreateNewObj logical      = true;
        Args.GetAllFields logical      = false;  % if CreateNewObj=true & GetAllFields=false then the other fields are not copied
        Args.CopyHeader logical        = true; % copy header even if GetlAllFields=false
        Args.CopyPSF logical           = true;
    end
    
    Nobj = numel(Obj);
    
    OutWCS = [];
    % create a new copy
    if Args.CreateNewObj
        if Args.GetAllFields
            Result = Obj.copy;
        else
            % create a new empty AstroImage object
            Result = AstroImage(size(Obj));
            % copy Header
            if Args.CopyHeader
                for Iobj=1:1:Nobj
                    Result(Iobj).HeaderData = Obj(Iobj).HeaderData;
                end
            end
        end
    else
        Result = Obj;
    end
    
    % number of data properties to transform
    Nprop = numel(Args.DataProp);
    
    IsDisplacment   = false;
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
        elseif all(size(Trans)==[3 3])
            % A 3x3 matrix
            % assume this is an affine transformation
            
            ImWarpTransformation = affine2d(Trans);
        elseif ndims(Trans)==3 && size(Trans,3)==2
            % A 3D cube in which 3rd dim has size of 2
            % Assume this is a distortion field
            IsDisplacment   = true;
            DispField(1).DF = Trans;
        else
            error('Unknown Trans (2nd input argument) numeric option'); 
        end
    else
        % Trans can be:
        % AstroImage with AstroWCS
        % AstroWCS
        % Tran2D object
        % An affine transformation object
        % struct array with displacement fields in DF
        
        if isa(Trans, 'AstroWCS')
            % Convert AstroWCS to displacment field
            IsDisplacment   = true;
            
            [ImageSizeY, ImageSizeX]   = sizeImage(Obj);

            DispField = struct('DF',cell(Nobj,1));
            for Iobj=1:1:Nobj
                DispField(Iobj).DF  = xy2refxyDisp(Obj(Iobj).WCS, [1, ImageSizeX(Iobj), 1, ImageSizeY(Iobj)], Trans, 'Sampling', Args.Sampling);
            end
            OutWCS = Trans.copy;
        elseif isa(Trans, 'AstroImage')
            % Convert AstroWCS in AstroImage to displacment field
            IsDisplacment   = true;
            
            [ImageSizeY, ImageSizeX]   = sizeImage(Obj);

            DispField = struct('DF',cell(Nobj,1));

            if ~Trans.WCS.Success
                Trans.populateWCS;
            end 

            for Iobj=1:1:Nobj
                if ~isempty(Obj(Iobj).WCS) && Obj(Iobj).WCS.Success
                    DataWCS = Obj(Iobj).WCS;
                else
                    % attempt to generate WCS from header
                    Obj(Iobj).populateWCS;
                    DataWCS = Obj(Iobj).WCS;
                end

                DispField(Iobj).DF  = xy2refxyDisp(DataWCS, [1, ImageSizeX(Iobj), 1, ImageSizeY(Iobj)], Trans.WCS, 'Sampling', Args.Sampling);
            end
            OutWCS = Trans.WCS.copy;
        elseif isa(Trans, 'affine2d')
            ImWarpTransformation = Trans;
            Ntran = numel(ImWarpTransformation);
            OutWCS = [];
        elseif isa(Trans, 'Tran2D')
            error('Tran2D option is not yet supported');
        elseif isa(Trans, 'struct')
            % assume as truct array of displacment fields
            % The struct must contains a .DF field
            IsDisplacment   = true;
            DispField = Trans;
            OutWCS = [];
        else
            error('Unknown Trans (2nd input argument) object option'); 
        end
    end
    
    
%     if isempty(Args.DisplacmentField) && isempty(Args.RefWCS)
%         IsDisplacment = false;
%         if isempty(Args.AffineMat)
%             % use ShiftXY
%             % convert shifts to affine2d
%             [NrowSh, NcolSh] = size(Args.ShiftXY);
%             if NcolSh~=2
%                 error('Number of columns in ShiftXY must be 2');
%             end
%             
%             for Itran=1:1:NrowSh
%                 ImWarpTransformation(Itran) = affine2d([1 0 0; 0 1 0; Args.ShiftXY(Itran,1) Args.ShiftXY(Itran,2) 1]);
%             end
%             
%         else
%             % use affine2d
%             if isa(Args.AffineMat, 'affine2d')
%                 ImWarpTransformation = Args.AffineMat;
%             else
%                 % single matrix of affine tran
%                 ImWarpTransformation = affine2d(Args.AffineMat);
%             end
%                 
%         end
%     else 
%         IsDisplacment = true;
%         if isempty(Args.RefWCS)
%             if isempty(Args.DisplacmentField)
%                 error('Either RefWCS or DisplacmentField must be provided');
%             end
%             
%             DispField(1).DF = Args.DisplacmentField; 
%         
%         else
%             % generate displacment field for pairs of images
%             [ImageSizeY, ImageSizeX]   = sizeImage(Obj);
% 
%             DispField = struct('DF',cell(Nobj,1));
%             for Iobj=1:1:Nobj
%                 DispField(Iobj).DF  = xy2refxy(Obj(Iobj).WCS, [1, ImageSizeX(Iobj), 1, ImageSizeY(Iobj)], Args.RefWCS, 'Sampling', 10);
%             end
%         end
%     end
   
    % set OutView
    % Not clear what to do about this
    %OutView = affineOutputView(sizeA, ImWarpTransformation)
    
    

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        
        if ischar(Args.FillValues)
            % use median of background
            FillVal = median(Obj(Iobj).Back,[1 2],'omitnan');
        else
            FillVal = Args.FillValues;
        end
        
        SizeInput = size(Obj(Iobj).(Args.DataProp{1}));
        
        
        if IsDisplacment
            TranArg = DispField(Iobj).DF;
            OutView = [];
        else
            Ntran   = numel(ImWarpTransformation);
            Itran   = min(Ntran,Iobj);
            TranArg = ImWarpTransformation(Itran);
            OutView = affineOutputView(SizeInput, ImWarpTransformation(Itran),'BoundsStyle','CenterOutput');
        end
        
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
        
        Result(Iobj) = deleteProp(Result(Iobj), Args.DeleteProp);
        
        if Args.TransWCS
            % Transform the WCS into the new reference frame
            if isempty(OutWCS)
                warning('TransWCS=true option is not available for non-WCS transformations')
            else
                Result(Iobj).WCS = OutWCS;
                Result(Iobj).HeaderData = wcs2header(Result(Iobj).WCS, Result(Iobj).HeaderData);
            end
        end

        if Args.CopyPSF
            Result(Iobj).PSFData = Obj(Iobj).PSFData.copy;
        end
        
    end
    
    
    
end
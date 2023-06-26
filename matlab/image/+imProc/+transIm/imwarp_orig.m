function Result = imwarp_old(Obj, Args)
    % Apply the imwarp function on AstroImage object
    %   The Header, CatData, and PSF are not transformed.
    % Input  : - 
    % Output : - 
    % Author : Eran Ofek (Nov 2021)
    % Example: imProc.transIm.imwarp(
    
    arguments
        Obj AstroImage
        Args.ShiftXY                   = [0 0];
        Args.AffineMat                 = [];    % matrix or affine2d object
        Args.DisplacmentField          = [];
        Args.RefWCS                    = [];
        Args.Tran2D                    = [];
        
        Args.DataProp                  = {'Image', 'Back', 'Var'};
        Args.DataPropMask              = 'Mask'
        Args.InterpMethod              = 'cubic';
        Args.InterpMethodMask          = 'nearest';
        Args.DeleteProp                = {};
        
        Args.FillValues                = 'back';
        Args.SmoothEdges logical       = true;
        
        Args.ReplaceNaN logical        = true;  % replace NaN with FillVal
        
        Args.TransWCS logical          = true;
        
        Args.CreateNewObj logical      = false;
        Args.GetAllFields logical      = false;  % if CreateNewObj=true & GetAllFields=false then the other fields are not copied
        Args.CopyHeader logical        = true; % copy header even if GellAllFields=false
    end
    
    Nobj = numel(Obj);
    
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
    
    if ~isempty(Args.Tran2D)
        % transform 2D to dsiplacment map
        
        error('Tran2D option is not yet supported');
        
    end
    
    Nprop = numel(Args.DataProp);
    
    if isempty(Args.DisplacmentField) && isempty(Args.RefWCS)
        IsDisplacment = false;
        if isempty(Args.AffineMat)
            % use ShiftXY
            % convert shifts to affine2d
            [NrowSh, NcolSh] = size(Args.ShiftXY);
            if NcolSh~=2
                error('Number of columns in ShiftXY must be 2');
            end
            
            for Itran=1:1:NrowSh
                ImWarpTransformation(Itran) = affine2d([1 0 0; 0 1 0; Args.ShiftXY(Itran,1) Args.ShiftXY(Itran,2) 1]);
            end
            
        else
            % use affine2d
            if isa(Args.AffineMat, 'affine2d')
                ImWarpTransformation = Args.AffineMat;
            else
                % single matrix of affine tran
                ImWarpTransformation = affine2d(Args.AffineMat);
            end
                
        end
    else 
        IsDisplacment = true;
        if isempty(Args.RefWCS)
            if isempty(Args.DisplacmentField)
                error('Either RefWCS or DisplacmentField must be provided');
            end
            
            DispField(1).DF = Args.DisplacmentField; 
        
        else
            % generate displacment field for pairs of images
            [ImageSizeY, ImageSizeX]   = sizeImage(Obj);

            DispField = struct('DF',cell(Nobj,1));
            for Iobj=1:1:Nobj
                DispField(Iobj).DF  = xy2refxyDisp(Obj(Iobj).WCS, [1, ImageSizeX(Iobj), 1, ImageSizeY(Iobj)], Args.RefWCS, 'Sampling', 10);
            end
        end
    end
   
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
            TranArg = ImWarpTransformation(Iobj);
            OutView = affineOutputView(SizeInput, ImWarpTransformation(Iobj),'BoundsStyle','CenterOutput');
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
            warning('WCS transformation is not yet available');
        end
        
    end
    
    
    
end
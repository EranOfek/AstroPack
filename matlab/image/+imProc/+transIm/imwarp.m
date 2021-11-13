function Result = imwarp(Obj, Args)
    % Apply the imwarp function on AstroImage object
    %   The Header, CatData, and PSF are not transformed.
    % Input  : - 
    % Output : - 
    % Author : Eran Ofek (Nov 2021)
    % Example: 
    
    arguments
        Obj AstroImage
        Args.ShiftXY                   = [0 0];
        Args.AffineMat                 = [];    % matrix or affine2d object
        Args.DisplacmentField          = [];
        Args.Tran2D                    = [];
        
        Args.DataProp                  = {'Image', 'Back', 'Var'};
        Args.DataPropMask              = 'Mask'
        Args.InterpMethod              = 'cubic';
        Args.InterpMethodMask          = 'nearest';
        Args.DeleteProp                = {};
        
        Args.FillValues                = 'back';
        Args.SmoothEdges logical       = true;
        
        Args.TransWCS logical          = true;
        
        Args.CreateNewObj logical      = false;
        Args.GetAllFields logical      = false;  % if CreateNewObj=true & GetAllFields=false then the other fields (e.g., Header) are not copied
    end
    
    if Args.CreateNewObj
        if Args.GetAllFields
            Result = Obj.copy;
        else
            % create a new empty AstroImage object
            Result = AstroImage(size(Obj));
        end
    else
        Result = Obj;
    end
    
    if ~isempty(Args.Tran2D)
        % transform 2D to dsiplacment map
        
        error('Tran2D option is not yet supported');
        
    end
    
    Nprop = numel(Args.DataProp);
    
    IsDisplacment = false;
    if isempty(Args.DisplacmentField)
        if isempty(Args.AffineMat)
            % use ShiftXY
            % convert shifts to affine2d
            [NrowSh, NcolSh] = size(Args.ShiftXY);
            if NcolSh~=2
                error('Number of columns in ShiftXY must be 2');
            end
            
            for Itran=1:1:NrowSh
                ImWarpTransformation(Itran) = affine2d([1 0 0; 0 1 0; ShiftX ShiftY 1]);
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
        ImWarpTransformation = Args.DisplacmentField;
        IsDisplacment = true;
    end
   
    %OutView = affineOutputView(sizeA, ImWarpTransformation)
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        
        if ischar(Args.FillValues)
            % use median of background
            FillVal = median(Obj(Iobj).Back,[1 2],'omitnan');
        else
            FillVal = Args.FillValues;
        end
        
        for Iprop=1:1:Nprop
            Result(Iobj).(Args.DataProp{Iprop}) = imwarp(Obj(Iobj).(Args.DataProp{Iprop}), ImWarpTransformation, Args.InterpMethod,...
                                                    'OutputView',OutView,...
                                                    'FillValues',FillVal,...
                                                    'SmoothEdges',Args.SmoothEdges);
            
        end
        
        % mask transformation
        if ~isempty(Args.DataPropMask)
            Result(Iobj).(Args.DataPropMask) = imwarp(Obj(Iobj).(Args.DataPropMask), ImWarpTransformation, Args.InterpMethodMask,...
                                                    'OutputView',OutView,...
                                                    'FillValues',FillVal,...
                                                    'SmoothEdges',Args.SmoothEdges);
        end
        
        Result(Iobj) = deleteProp(Result(Iobj), Args.DeleteProp);
        
        if Args.TransWCS
            % Transform the WCS into the new reference frame
            warning('WCS transformation is not yet available');
        end
        
    end
    
    
    
end
function [Result, CoaddN] = coadd_WRobust(Obj, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 Mar) 
    % Example: 

    arguments
        Obj   % AstroImage, AstroDiff, AstroZOGY
        Args.SubBack         = true;
        Args.BackArgs        = {};
        Args.ScalarVar       = true;
        Args.CCDSEC          = [];
        Args.ZP              = [];  % if empty use equal flux matching
        Args.ZP0             = 25;
        
        %--- coadd ---
        Args.RemoveMinMax    = true;
        Args.Niter           = 1;
        Args.SigmaClip       = [2.5 2.5];
        Args.StdMethod       = 3;
        Args.CoaddUseMex     = true;
        Args.AddBack         = true;
        Args.backVarArgs     = {};

        %--- Mask ---
        Args.AddMask         = true;
        Args.BitCoaddUseMex  = true;

        %--- Header ---
        Args.HeaderCopy1 logical                    = true;
        Args.NewHeader                              = [];
        Args.UpdateTimes(1,1) logical               = true;
        Args.SumExpTime(1,1) logical                = true;
        Args.UpdateImagePathKeys logical            = true;
        Args.StackMethod                            = '';
        Args.CoaddN                                 = NaN;
        Args.KeyExpTime                             = 'EXPTIME';

        %--- aux ---
        Args.PreAllocCube    = [];

    end
    StackMethod = 'wrbosut';
    
    Nim = numel(Obj);
    DimIndex  = 3;
    % is background needed?
    if Args.SubBack
        % Get background
        Flag = Obj.isemptyProperty('Back');
        Obj(Flag) = imProc.background.backVar(Obj(Flag), Args.BackArgs{:});
    
        [ImageCube, BackCube, VarCube, MaskCube] = imProc.image.images2cube(Obj, 'CCDSEC',Args.CCDSEC, 'DimIndex',DimIndex,...
                                                                                 'DataProp',{'ImageData','BackData', 'VarData', 'MaskData'},...
                                                                                 'DataPropIn','Data',...
                                                                                 'Cube',Args.PreAllocCube);
    else
        % no background subtraction is needed
        [ImageCube, VarCube, MaskCube] = imProc.image.images2cube(Obj, 'CCDSEC',Args.CCDSEC, 'DimIndex',DimIndex,...
                                                                                 'DataProp',{'ImageData', 'VarData', 'MaskData'},...
                                                                                 'DataPropIn','Data',...
                                                                                 'Cube',Args.PreAllocCube);
        BackCube = [];
    end

    % convert Var cube to scalar per pimage
    if Args.ScalarVar
        Var = squeeze(mean(VarCube,[1 2], 'omitnan'));
    else
        Var = VarCube;
    end
    if ~strcmp(class(ImageCube), class(Var))
        Var = cast(Var, 'like',ImageCube);
    end

    % flux matching parameters
    if isempty(Args.ZP)
        % Equal flux matching
        FluxMatch = ones(Nim,1);
        ZP0       = NaN;
    else
        [FluxMatch,ZP0] = imProc.stack.getFluxMatch(Obj, 'ZP',Args.ZP, 'ZP0',Args.ZP0);
    end
    % make sure Flux match ha sthe same units as the images
    FluxMatch = cast(FluxMatch, 'like',ImageCube);

    % create AstroImage for results
    Result = AstroImage;

    % coadd
    [Result.ImageData.Data, Result.VarData.Data] = imUtil.stack.wcoaddRobust(ImageCube, BackCube, 'Var',Var, 'F',FluxMatch, 'ZP',[],'ZP0',[],...
                                                                       'RemoveMinMax',Args.RemoveMinMax,'Niter',Args.Niter,'SigmaClip',Args.SigmaClip, 'StdMethod',Args.StdMethod,...
                                                                       'UseMex',Args.CoaddUseMex);

    CoaddN = Nim;

    if Args.AddBack
        Result.BackData.Data = imUtil.background.backVar(Result.ImageData.Data, Args.backVarArgs{:});
    end

    % coadd mask
    if Args.AddMask
        Result.MaskData.Data  = squeeze(tools.array.bitor_array(MaskCube, DimIndex, Args.BitCoaddUseMex));
    end

    % Update header:
    Result.HeaderData = imProc.stack.coaddHeader(Obj, 'HeaderCopy1', Args.HeaderCopy1,...
                                                      'NewHeader',Args.NewHeader,...
                                                      'UpdateTimes',Args.UpdateTimes,...
                                                      'SumExpTime',Args.SumExpTime,...
                                                      'UpdateImagePathKeys',Args.UpdateImagePathKeys,...
                                                      'StackMethod',StackMethod,...
                                                      'CoaddN',CoaddN,...
                                                      'KeyExpTime',Args.KeyExpTime);


end

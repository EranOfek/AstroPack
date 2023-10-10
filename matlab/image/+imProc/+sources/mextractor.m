function Result=mextractor(Obj, Args)
    % 

    arguments
        Obj AstroImage

        % back,var,PSF finding
        Args.populatePSFArgs cell      = {};
        Args.ThresholdPSF              = 20;
        Args.RangeSN                   = [50 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell          = {[0.1;2]};
        
        % PSF fit
        Args.Threshold                 = [50 16.5 5]; % This also specifies the # of iterations
        Args.ReMeasBack logical        = true;
        
        Args.PrelimPsf               = @imUtil.kernel2.gauss;
        Args.PrelimPsfArgs cell      = {[0.1 2]};
        Args.PrelimThreshold         = 30;
        Args.Conn                    = 8;
        Args.PrelimCleanSrc logical  = true;
        Args.PrelimCleanSrcArgs cell = {'ColSN_sharp',1, 'ColSN_psf',2, 'SNdiff',0, 'MinEdgeDist',15, 'RemoveBadSources',true};

        Args.backgroundArgs cell     = {};
        Args.ReBack logical          = false; % remeasure if background exits 
        Args.CreateNewObj logical    = false;
    end

    % create new copy
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    
    % populatePSF will:
    %   1. populate background
    %   2. populate variance
    %   3. populate PSF
    [Result, ResPSF] = imProc.psf.populatePSF(Result, Args.populatePSFArgs{:},...
                                                      'ThresholdPSF',Args.ThresholdPSF,...
                                                      'RangeSN',Args.RangeSN,...
                                                      'InitPsf',Args.InitPsf,...
                                                      'InitPsfArgs',Args.InitPsfArgs);
        
    % find sources using PSF - multi teration
    Niter = numel(Args.Threshold);
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        SubImage = Result(Iobj).Image;
        for Iiter=1:1:Niter
            % re-measure background
            if Iiter>1 && Args.ReMeasBack

            end

            % find sources
            ResSrc(Iobj,Iiter) = imUtul.sources.findSources(SubImage, 'Threshold',Args.Threshold(Iiter),...
                                                                      'Psf',Result(Iobj).PSFData.getPSF,...
                                                                      'BackIm',Result(Iobj).Back,...
                                                                      'VarIm',Result(Iobj).Var,...
                                                                      'CleanSources',flase,...
                                                                      'AddValAtPos',true);
            % Clean sources
            % Use VAL to calculate SN for delta function
            
  
            % PSF fit sources

            % Add sources to list

            % subtract suorces

        end
    end
    
    % Find diffraction spikes
    
    % Cleaning
    
    
    
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % measure background/variance if needed
        if Args.ReBack || any(Obj(Iobj).isemptyImage({'Back','Var'}))
            % measure background and variance
            Obj(Iobj) = imProc.background.background(Obj(Iobj), Args.backgroundArgs{:});

        end

        if Obj(Iobj).PSFData.isemptyPSF
            % NO PSF - attempt to measure
        
            % find sources using a prelimnary PSF
            ResSt = imUtil.sources.findSources(Result(Iobj), 'Threshold',Args.PrelimThreshold,...
                                                        'Psf',Args.PrelimPsf,...
                                                        'PsfArgs',Args.PrelimPsfArgs,...
                                                        'ForcedList',[],...
                                                        'OnlyForced',false,...
                                                        'Conn',Args.Conn,...
                                                        'CleanSources',Args.PrelimCleanSrc,...
                                                        'cleanSourcesArgs',Args.PrelimCleanSrcArgs);


            % construct PSF
            

            % what to do if no PSF?

        end
        
        % find sources using updated PSF

        % cleaning iteration

        % Measure properties
      
        % another cleaning iteration?





    end






end
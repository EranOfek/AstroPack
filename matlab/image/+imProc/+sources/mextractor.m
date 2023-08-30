function Result=mextractor(Obj, Args)
    % 

    arguments
        Obj AstroImage

        Args.PrelimPsf               = @imUtil.kernel2.gauss;
        Args.PrelimPsfArgs cell      = {[0.1 2]};
        Args.PrelimThreshold         = 30;
        Args.Conn                    = 8;
        Args.PrelimCleanSrc logical  = true;
        Args.PrelimCleanSrcArgs cell = {'ColSN_sharp',1, 'ColSN_psf',2, 'SNdiff',0, 'MinEdgeDist',15, 'RemoveBadSources',true};

        Args.ReBack logical          = false; % remeasure if background exits 
        Args.CreateNewObj logical    = false;
    end

    % create new copy
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % measure background/variance if needed
        if Args.ReBack || ~all(Obj(Iobj).isemptyImage({'Back','Var'}))
            % measure background and variance
            

        end

        % find sources using a prelimnary PSF
        ResSt = imProc.sources.findSources(Result, 'Threshold',Args.PrelimThreshold,...
                                                    'Psf',Args.PrelimPsf,...
                                                    'PsfArgs',Args.PrelimPsfArgs,...
                                                    'ForcedList',[],...
                                                    'OnlyForced',false,...
                                                    'Conn',Args.Conn,...
                                                    'CleanSources',Args.PrelimCleanSrc,...
                                                    'cleanSourcesArgs',Args.PrelimCleanSrcArgs);


        % construct PSF

        % what to do if no PSF?

        % find sources using updated PSF

        % cleaning iteration

        % Measure properties
      
        % another cleaning iteration?





    end






end
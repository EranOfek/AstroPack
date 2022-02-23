function findOrphansClean(Obj, Args)
    %
   
    
    
    arguments
        Obj MatchedSources
        Args.MaxNepochs              = 3;
        Args.SelectFieldName         = MatchedSources.DefNamesRA;
        Args.OutputFields            = {'RA','Dec'};
        Args.SN_Field                = 'SN_2';
        Args.Flags_Field             = 'FLAGS';
        
        Args.CheckSucessive logical  = true;
        Args.MinSN                   = @(X)(8 - X);   % X is Ndet
    end

    % look for all orphan candidates in a MatchedSources object
    OutputFields = [Args.OutputFields, Args.SN_Field, Args.Flags_Field];
    OrphansList = lcUtil.findOrphans(Obj, 'SelectFieldName',Args.SelectFieldName, 'MaxNepochs',Args.MaxNepochs, 'OutputFields',OutputFields);
    
    Nobj = numel(Obj);
    
    for Iobj=1:1:Nobj
        Nsrc = numel(OrphansList);
        for Isrc=1:1:Nsrc
            OrphansList(Iobj).Src(Isrc).GoodOrphan = true;
            
            % check that observations are sucessive (assuming sorted by JD)
            if Args.CheckSucessive && OrphansList(Iobj).Src(Isrc)>1
                if max(diff(OrphansList(Iobj).Src(Isrc).EpochInd)) > 1
                    % Remove source
                    OrphansList(Iobj).Src(Isrc).GoodOrphan = false;
                end
            end
            
            % check that S/N is larger than threshold
            
            
            % Check FLAGS
            
            
        end
    end
    
end

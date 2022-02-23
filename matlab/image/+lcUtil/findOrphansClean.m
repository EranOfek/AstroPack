function findOrphansClean(Obj, Args)
    %
   
    
    
    arguments
        Obj MatchedSources
        Args.MaxNepochs        = 1;
        Args.SelectFieldName   = MatchedSources.DefNamesRA;
        Args.OutputFields      = {'RA','Dec'};
        Args.SN_Field          = 'SN_2';
        Args.Flags_Field       = 'FLAGS';
        
        Args.MinSN             = @(Ndet) 8 - Ndet
    end

    % look for all orphan candidates in a MatchedSources object
    OutputFields = [Args.OutputFields, Args.SN_Field, Args.Flags_Field];
    OrphansList = lcUtil.findOrphans(Obj, 'SelectFieldName',Args.SelectFieldName, 'MaxNepochs',Args.MaxNepochs, 'OutputFields',OutputFields);
    
    
    
    
    
end

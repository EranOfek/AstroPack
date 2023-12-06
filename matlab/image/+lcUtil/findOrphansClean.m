function [OrphansList,CleanOrphansList,Norphans] = findOrphansClean(Obj, Args)
    % Select orphan sources from a MatchedSources object and clean them.
    %   Orphan sources are defined to be sources which appears in
    %   only 1 (or a few) epochs - These are selected using lcUtil.findOrphans
    %   Clean sources are those which satisfy some additional criteria like
    %   1. sucessive observations; 2. SN above some threshold; 3. no bad
    %   flags; 4. 2nd moment are larger than some threshold.
    % Input  : - A MatchedSources object.
    %          * ...,key,val,...
    %            'BitDict' - 
    %            'MaxNepochs' - Max number of epochs in which an
    %                   orphan sources is detected in.
    %                   Default is 1.
    %            'SelectFieldName' - Field name in which to look for NaNs
    %                   in order to search for orphan sources.
    %                   Default is MatchedSources.DefNamesX
    %            'OutputFields' - A cell array of fields in
    %                   MatchedSources.Data
    %                   For each field will return the value for
    %                   the orphan sources at the detected epochs.
    %                   'SN_Field', and 'Flags_Field' will be added to this
    %                   list. Default is {'RA','Dec'}.
    %
    %            'SN_Field' - A field containing S/N of source.
    %                   Default is 'SN_2'.
    %            'Flags_Field' - A field containing FLAGS of source.
    %                   Default is 'FLAGS'.
    %            'M2_Fields' - Fields of 2nd moment. If empty, then will
    %                   not use this. Default is {'X2','Y2'}
    %            'M2_Min' - Minimum value for [X2, Y2] 2nd moment. 
    %                   If all detections in either X2 or Y2 are below
    %                   these threshold then set to GoodOrphan = false.
    %                   Default is [0.7 0.7].
    %            'CheckSucessive' - A logical indicating if to check that
    %                   all the observations are sucessive.
    %                   If true, then sources without sucessive
    %                   observations will have GoodOrphan = false.
    %                   Default is true.
    %            'MinSN' - Parameters that will be used to calculate the
    %                   threshold S/N as a function of number of detections
    %                   (Ndet). The function is of the form @(Ndet,MaxSN,MinSN) max(MaxSN - Ndet, MinSN);
    %                   and the parameters are [MaxSN, MinSN].
    %                   Sources below this S/N will set to GoodOrphan = false.
    %                   Default is [10,5].
    %            'MeanFun' - Function handle that will be used to calculate
    %                   the mean S/N over all detections.
    %                   Default is @mean.
    %            'BadFlags' - A cell array of flags. A source that contains
    %                   one of these flags is set to GoodOrphan = false.
    %                   If empty, then will skip this test.
    %                   Default is {'Saturated','NaN','Interpolated','Spike','CR_DeltaHT','CR_Laplacian','CR_Streak','Ghost','Persistent','Streak','NearEdge','Overlap'}
    % Output : - A structure array (element per MatchedSources
    %            element) with fields:
    %            .Flag - a row vector of logical indicating if a
    %                   source in the [Epoch, Src] matrix is an
    %                   orphan.
    %            .NotNanMatrix - A matrix of logicals indicating
    %                   the not-nan elemsnts in the matrix.
    %            .Src - Astructure array. Element per orphan, with
    %                   the following fields:
    %               .SrcInd - Source index.
    %               .EpochInd - Vector of epoch indices in which
    %                       the source was detected.
    %               .JD - Vector of JD.
    %               .Ndet - Numbre of detections.
    %               .(all the fields in OutputFields) - Vector of
    %                       requested properties.
    %               .GoodOrphan - a logical indicating if the orphan
    %                       satisfy the cleaning criteria (sucessive
    %                       observations; S/N; flags).
    %          - A clean version of the first output argument.
    %            This includes only the .Src field and only sources with
    %               GoodOrphan=true.
    %          - The total number of good orphans found in all
    %               MatchedSources object.
    % Author : Eran Ofek (Feb 2022)
    % Example: [OrphansList,CleanOrphansList,Norphans] = lcUtil.findOrphansClean(MatchedS, 'BitDict',AllSI(1).MaskData.Dict);
   
    
    
    arguments
        Obj MatchedSources
        Args.BitDict                 = [];    % must be provided
        
        Args.MaxNepochs              = 3;
        Args.SelectFieldName         = MatchedSources.DefNamesRA;
        Args.OutputFields            = {'RA','Dec'};
        
        Args.SN_Field                = 'SN_2';
        Args.Flags_Field             = 'FLAGS';
        Args.M2_Fields               = {}; %{'X2','Y2'};
        Args.M2_Min                  = [0.7 0.7];
        
        Args.CheckSucessive logical  = true;
        Args.MinSN                   = [9, 5]; %@(X)max(8 - X, 5);   % X is Ndet
        Args.MeanFun function_handle = @mean;
        
        Args.BadFlags cell           = {'Saturated','NaN','Interpolated','Spike','CR_DeltaHT','CR_Laplacian','CR_Streak','Ghost','Persistent','Streak','NearEdge','Overlap'};
    end
    
    if isempty(Args.BitDict) && ~isempty(Args.BadFlags)
        error('If BadFlags is not empty, then BitDict must be provided');
    end
    
    if isempty(Args.BadFlags)
        BadFlags = Args.BadFlags;
    else
        % Generate a decimal FLAG
        % this is in order to save time
        [~,~,BadFlags] = name2bit(Args.BitDict, Args.BadFlags);
    end
        
    
    if isempty(Args.MinSN)
        FunMinSN = [];
    else
        FunMinSN = @(Ndet,MaxSN,MinSN) max(MaxSN - Ndet, MinSN);
    end
        
    % look for all orphan candidates in a MatchedSources object
    OutputFields = [Args.OutputFields, Args.SN_Field, Args.Flags_Field, Args.M2_Fields];
    OrphansList = lcUtil.findOrphans(Obj, 'SelectFieldName',Args.SelectFieldName, 'MaxNepochs',Args.MaxNepochs, 'OutputFields',OutputFields);
    
    Nobj = numel(Obj);
    
    for Iobj=1:1:Nobj
        Nsrc = numel(OrphansList(Iobj).Src);
        for Isrc=1:1:Nsrc
            OrphansList(Iobj).Src(Isrc).GoodOrphan = true;
            
            % check that observations are sucessive (assuming sorted by JD)
            if Args.CheckSucessive && OrphansList(Iobj).Src(Isrc).Ndet>1
                if max(diff(OrphansList(Iobj).Src(Isrc).EpochInd)) > 1
                    % Remove source
                    OrphansList(Iobj).Src(Isrc).GoodOrphan = false;
                end
            end
            
            % check that S/N is larger than threshold
            if ~isempty(Args.MinSN)
                if OrphansList(Iobj).Src(Isrc).Ndet>1
                    MeanSN = Args.MeanFun(OrphansList(Iobj).Src(Isrc).(Args.SN_Field));
                else
                    MeanSN = OrphansList(Iobj).Src(Isrc).(Args.SN_Field);
                end
                
                %if Args.MeanFun(OrphansList(Iobj).Src(Isrc).(Args.SN_Field)) < FunMinSN(OrphansList(Iobj).Src(Isrc).Ndet, Args.MinSN(1), Args.MinSN(2))
                if MeanSN < FunMinSN(OrphansList(Iobj).Src(Isrc).Ndet, Args.MinSN(1), Args.MinSN(2))
                    % source is below S/N threshold
                    % Remove source
                    OrphansList(Iobj).Src(Isrc).GoodOrphan = false;
                end
            end
            
            % Check FLAGS
            if ~isempty(Args.BadFlags)
                [Flag] = findBit(Args.BitDict, OrphansList(Iobj).Src(Isrc).FLAGS, BadFlags, 'Method','any');
                if any(Flag)
                   % bad flag
                   % Remove source
                   OrphansList(Iobj).Src(Isrc).GoodOrphan = false;
                end
            end
            
            % Check 2nd moment
            if ~isempty(Args.M2_Fields)
                if all(OrphansList(Iobj).Src(Isrc).(Args.M2_Fields{1}) < Args.M2_Min(1)) || ...
                                    all(OrphansList(Iobj).Src(Isrc).(Args.M2_Fields{2}) < Args.M2_Min(2))
                    OrphansList(Iobj).Src(Isrc).GoodOrphan = false;
                end
            end
            
        end
    end
    
    if nargout>1
        % count GoodOrphans
        Norphans = 0;
        CleanOrphansList = struct('Src',cell(Nobj,1));
        for Iobj=1:1:Nobj
            FlagGood = [OrphansList(Iobj).Src(:).GoodOrphan];
            CleanOrphansList(Iobj).Src = OrphansList(Iobj).Src(FlagGood);
            
            if nargout>2
                Norphans = Norphans + sum(FlagGood);
            end
            
            % calc mean properties
            Nsrc = numel(CleanOrphansList(Iobj).Src);
            for Isrc=1:1:Nsrc
                CleanOrphansList(Iobj).Src(Isrc).MeanRA  = mean(CleanOrphansList(Iobj).Src(Isrc).RA);
                CleanOrphansList(Iobj).Src(Isrc).MeanDec = mean(CleanOrphansList(Iobj).Src(Isrc).Dec);
                CleanOrphansList(Iobj).Src(Isrc).MeanJD = mean(CleanOrphansList(Iobj).Src(Isrc).JD);
            end
        end
    end
    
end

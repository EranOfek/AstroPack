function FlagOrphan = findOrphans(Obj, Args)
    % Select orphan sources from a MatchedSources object.
    %   Orphan sources are defined to be sources which appears in
    %   only 1 (or a few) epochs.
    % Input  : - A MatchedSources object.
    %          * ...,key,val,...
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
    % Author : Eran Ofek (Feb 2022)
    % Example: MS = MatchedSources;
    %          MS.addMatrix(rand(100,200).*10,'MAG');
    %          MS.Data.MAG(:,[1 10]) = NaN; MS.Data.MAG(1,1) = 10; MS.Data.MAG(10:11,10) = 10;
    %          MS.addMatrix(rand(100,200).*10,'RA');
    %          MS.addMatrix(rand(100,200).*10,'Dec');
    %          F = lcUtil.findOrphans(MS,'SelectFieldName','MAG', 'MaxNepochs',2)

    arguments
        Obj MatchedSources
        Args.MaxNepochs           = 1;
        Args.RemoveNdet0 logical  = true;
        Args.SelectFieldName      = MatchedSources.DefNamesX;

        Args.OutputFields         = {'RA','Dec'};
    end

    % features to add:
    % choose only Ndet which are sucessive
    % remove sourece by some criteria - e.g., SN_std... FLAGS...

    Nout = numel(Args.OutputFields);
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        Matrix = getMatrix(Obj(Iobj), Args.SelectFieldName);
        % number of epochs in which each source was detected
        NotNanMatrix = ~isnan(Matrix);
        NepochPerSrc = sum(NotNanMatrix, 1);

        % flag orphan sources
         if Args.RemoveNdet0
            % Remove sources with Ndet=0
            % This can happen when some property (Args.SelectFieldName) of detected source can
            % not be measured 
            FlagOrphan(Iobj).Flag         = NepochPerSrc <= Args.MaxNepochs & NepochPerSrc>0;
         else
             FlagOrphan(Iobj).Flag         = NepochPerSrc <= Args.MaxNepochs;
         end
        FlagOrphan(Iobj).NotNanMatrix = NotNanMatrix;

        if ~isempty(Args.OutputFields)
            % retrieve data for orphan sources
            [OutCell{1:1:Nout}] = getMatrix(Obj(Iobj), Args.OutputFields);
        end

        % prep JD to save time
        JD = Obj(Iobj).JD;
        
        % look for indices of epochs in which the orphan sources
        % were detected
        IndOrphans  = find(FlagOrphan(Iobj).Flag);
        NIndOprhans = numel(IndOrphans);
        for Iind=1:1:NIndOprhans
           
                
            FlagOrphan(Iobj).Src(Iind).SrcInd   = IndOrphans(Iind);
            FlagOrphan(Iobj).Src(Iind).EpochInd = find(NotNanMatrix(:,IndOrphans(Iind)));
            FlagOrphan(Iobj).Src(Iind).JD       = JD(NotNanMatrix(:,IndOrphans(Iind)));
            FlagOrphan(Iobj).Src(Iind).Ndet     = numel(FlagOrphan(Iobj).Src(Iind).JD);

            for Iout=1:1:Nout
                FlagOrphan(Iobj).Src(Iind).(Args.OutputFields{Iout}) = OutCell{Iout}(NotNanMatrix(:,IndOrphans(Iind)), IndOrphans(Iind) );
            end
        end
    end
end
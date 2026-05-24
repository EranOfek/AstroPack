function AI = addNdet(AI, MS, Args)
    % add to catalog the number of single-exposure detections (Ndet) derived from a matched source object 
    %     Optional detailed description
    % Input  : - an array of AstroImages (e.g., Coadd)
    %          - the relevant MatchedSource object 
    %          * ...,key,val,... 
    %          'NotIsEmptyImages' - an optional pre-calculated flag on empty-image input objects 
    %          'SearchRadius' - (arsec) a search readius for the objects to be consdiered matched 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2026 May) 
    % Example: tic;imProc.cat.addNdet(Coadd,MS,'NotIsEmptyImages',NotIsEmptyCoadd);toc

    arguments
        AI
        MS
        Args.NotIsEmptyImages   = [];        
        Args.SearchRadius       = 3; % arcsec
    end
    %
    Nsub = numel(AI);
    %
    if isempty(Args.NotIsEmptyImages)
       Args.NotIsEmptyImages = ~AI.isemptyImage; 
    end
    %
    for Isub = 1:1:Nsub
        if Args.NotIsEmptyImages(Isub) && ~isempty(MS(Isub).Data) && isfield(MS(Isub).Data, 'RA')
            RAMatched  = MS(Isub).Data.RA;
            DecMatched = MS(Isub).Data.Dec;
            N_DET          = sum(~isnan(RAMatched), 1).';
            MSCat          = AstroCatalog;
            MSCat.Catalog  = [median(RAMatched, 1, 'omitnan').', median(DecMatched, 1, 'omitnan').', double(N_DET)];
            MSCat.ColNames = {'RA', 'Dec', 'N_DET'};
            MSCat.ColUnits = {'deg', 'deg', ''};
            
            ResInd         = imProc.match.matchInd(AI(Isub), MSCat, 'Sort2', true, 'SearchRadius', Args.SearchRadius);
            Flag           = ~isnan(ResInd.Ind);
            NdetCol        = nan(numel(ResInd.Ind), 1);
            NdetCol(Flag)  = N_DET(ResInd.Ind(Flag));
            
            AI(Isub).CatData.insertCol(NdetCol, Inf, {'N_DET'}, {''});
        end
    end
end


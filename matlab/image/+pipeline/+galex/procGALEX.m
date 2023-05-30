function AC=procGALEX

    %


    FileList = io.files.filelist('WTTHV2*-int.fits');
    Nim = numel(FileList);
    for Iim=1:1:Nim
        [Iim, Nim]
        % read image (int) and Flags
        Int_FN   = FileList{Iim};
        Flag_FN  = strrep(Int_FN,'-nd-int.','-nd-flags.');
        RRHR_FN  = strrep(Int_FN,'-nd-int.','-nd-rrhr.');

        AI=AstroImage(Int_FN,'Mask',Flag_FN);
        ExpAI = AstroImage(RRHR_FN);
        AI.MaskData.Scale=8;             
        AI.MaskData.ScaleMethod='nearest';
        AI.MaskData.Dict = BitDictionary('BitMask.Image.GALEX');
        AI.MaskData.Data = uint32(AI.MaskData.Image);
        AI.MaskData.Scale=1;
    
        
        FlagRimPix = AI.MaskData.findBit({'Rim'});
        var(AI.Image(~FlagRimPix),[],'all','omitnan');
        
        %BI = AI.copy;
        %BI.Image(FlagRimPix) = NaN;
        %BI = imProc.background.background(BI, 'BackFun',@mean, 'BackFunPar',{'all','omitnan'}, 'VarFun',@var,'VarFunPar',{[],'all','omitnan'});
        
        %AI.Var  = 0.003; %BI.Var;
        ExpAI.Image(ExpAI.Image<10) = 0;
        
    
        MeanBack = mean(AI.Image(~FlagRimPix)>0,'all');  % in counts
        AI.Var = MeanBack./ExpAI.Image;
        AI.Back = zeros(size(AI.Var));
    
        AI = imProc.sources.findMeasureSources(AI);
        
        AI.populateWCS;
        AI.propagateWCS('UpdateHead',false);
    
        AC(Iim) = AI.CatData;
    end

    ResInd = imProc.match.matchReturnIndices(AC, AC(1));
    MAC = imProc.match.match(AC, AC(1));

    MS = MatchedSources;
    [MS, Matched] = unifiedCatalogsIntoMatched(MS, AC);

end

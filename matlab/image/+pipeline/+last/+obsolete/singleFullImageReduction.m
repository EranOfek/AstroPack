function AI = singleFullImageReduction(List, Args)
    % Analyze a full farme LAST images
    % Example: pipeline.last.singleFullImageReduction(List);
    
    arguments
        List         
        Args.Node      = 1
        Args.DataNum   = 1;
        Args.CropSize  = [];
        Args.DoAstrom  = true;
    end

    
    [BasePath, CalibDir, NewFilesDir, ProjName] = pipeline.last.constructArchiveDir('DataNum',Args.DataNum, 'Node',Args.Node);
    CI = CalibImages.loadFromDir(CalibDir);

    Nim = numel(List);
   
    AI = AstroImage(List);
    AI = CI.processImages(AI,'SubtractOverscan',false);

    if Args.DoAstrom
        [ResFit, AI] = imProc.astrometry.astrometryCropped(AI,'RA','RA', 'Dec','DEC', 'CropSize',Args.CropSize, 'Threshold',5,'findMeasureSourcesArgs',{'PsfFunPar',{[0.1; 1.0; 1.5; 2.6; 5]}});
        AI = imProc.calib.photometricZP(AI);
    else
        % measure background
        AI = imProc.background.background(AI);
        % find sources
        AI = imProc.sources.findMeasureSources(AI, 'MomPar',{'AperRadius',[2 4 6 10],'Annulus',[14 18]});
        
    end
    
end


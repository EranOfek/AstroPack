function [AC,MS]=procGALEX

    % Example: [AC,MS]=pipeline.galex.procGALEX;


ColCell             = {'XPEAK','YPEAK',...
                                                 'X1', 'Y1',...
                                                 'X2','Y2','XY',...
                                                 'SN','BACK_IM','VAR_IM',...  
                                                 'BACK_ANNULUS', 'STD_ANNULUS', ...
                                                 'FLUX_APER', 'FLUXERR_APER',...
                                                 'MAG_APER', 'MAGERR_APER',...
                                                 'FLUX_CONV', 'MAG_CONV', 'MAGERR_CONV','MAG_APER','MAGERR_APER'};
MatchedColums    = {'RA','Dec','X1','Y1','SN_1','SN_2','SN_3','SN_4','MAG_CONV_2','MAGERR_CONV_2','MAG_CONV_3','MAGERR_CONV_3','FLAGS', 'MAG_PSF','MAGERR_PSF'};

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
    
        AI = imProc.sources.findMeasureSources(AI, 'ColCell',ColCell);
        AI = imProc.psf.constructPSF(AI);
        AI = imProc.sources.psfFitPhot(AI);
        
        AI.populateWCS;
        AI.propagateWCS('UpdateHead',false);
    
        AC(Iim) = AI.CatData;
    end

    ResInd = imProc.match.matchReturnIndices(AC, AC(1));
    MAC = imProc.match.match(AC, AC(1));

    MS = MatchedSources;
    [MS, Matched] = unifiedCatalogsIntoMatched(MS, AC, 'MatchedColums',MatchedColums);

end

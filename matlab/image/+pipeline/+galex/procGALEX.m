function [AC,MS]=procGALEX(Args)
    % 
    % Example: [AC,MS]=pipeline.galex.procGALEX;

    arguments

        Args.MinExpTime          = 10;  % mimimum exp time to use:
        Args.ColCell             = {'XPEAK','YPEAK',...
                                                 'X1', 'Y1',...
                                                 'X2','Y2','XY',...
                                                 'SN','BACK_IM','VAR_IM',...  
                                                 'BACK_ANNULUS', 'STD_ANNULUS', ...
                                                 'FLUX_APER', 'FLUXERR_APER',...
                                                 'MAG_APER', 'MAGERR_APER',...
                                                 'FLUX_CONV', 'MAG_CONV', 'MAGERR_CONV','MAG_APER','MAGERR_APER'};

        Args.MatchedColums    = {'RA','Dec','X1','Y1','SN_1','SN_2','SN_3','SN_4','MAG_CONV_2','MAGERR_CONV_2','MAG_CONV_3','MAGERR_CONV_3','FLAGS', 'X','Y','MAG_PSF','MAGERR_PSF', 'PSF_CHI2DOF','SN', 'MergedCatMask'};
        Args.ZP               = VO.GALEX.zp('nuv');
    end


    FileList = io.files.filelist('WTTHV2*-int.fits');
    Nim = numel(FileList);
    JD  = nan(Nim,1);
    for Iim=1:1:Nim
        [Iim, Nim]
        % read image (int) and Flags
        Int_FN   = FileList{Iim};
        Flag_FN  = strrep(Int_FN,'-nd-int.','-nd-flags.');
        RRHR_FN  = strrep(Int_FN,'-nd-int.','-nd-rrhr.');
        AI = AstroImage(Int_FN, 'Mask',Flag_FN, 'Exp',RRHR_FN);
        AI.setAutoScale;  % set the sacle of the Mask image...
        
        Time1 = AI.HeaderData.getVal('EXPSTART');
        Time2 = AI.HeaderData.getVal('EXPEND');
        Time  = 0.5.*(Time1+ Time2);
        Date  = datetime(Time,'ConvertFrom','epochtime');
        JD(Iim) = juliandate(Date);

        %ExpAI = AstroImage(RRHR_FN);
        %AI.MaskData.Scale=8;             
        
        
        AI.MaskData.Dict = BitDictionary('BitMask.Image.GALEX');
        AI.MaskData.Data = uint32(AI.MaskData.Image);
        AI.MaskData.Scale=1;
    
        % find pixels that are outside the image edge:
        FlagRimPix = AI.MaskData.findBit({'Rim'});
        % calculate the variance of number of pixels with count>0, within
        % the image edge:
        var(AI.Image(~FlagRimPix),[],'all','omitnan');
        
        %BI = AI.copy;
        %BI.Image(FlagRimPix) = NaN;
        %BI = imProc.background.background(BI, 'BackFun',@mean, 'BackFunPar',{'all','omitnan'}, 'VarFun',@var,'VarFunPar',{[],'all','omitnan'});
        
        %AI.Var  = 0.003; %BI.Var;
        % for regions with ExpTime<10 s - set it to 0:
        AI.Exp(AI.Exp<Args.MinExpTime) = 0;
    
        % assuming, there are no background pixels with >1 count per pixel,
        % we can calculate the background from the mean of open pixels:
        % THIS IS TRUE ONLY FOR SHORT GALEX exposures:
        MeanBack = mean(AI.Image(~FlagRimPix)>0,'all');  % in counts

        % MeanBack is the mean background (and hence variance) in counts (not counts/s)
        % In order to get the variance in count/s we use arithmatic of the
        % variance: ????? not clear:
        AI.Var = MeanBack./(AI.Exp);
        AI.Back = zeros(size(AI.Var));
    
        AI = imProc.sources.findMeasureSources(AI, 'ColCell',Args.ColCell, 'ZP',Args.ZP);
        AI = imProc.psf.populate(AI);
        AI = imProc.sources.psfFitPhot(AI, 'ZP',Args.ZP);
        
        AI.populateWCS;
        AI.propagateWCS('UpdateHead',false);
    
        % match sources to the MergedCat:
        AI.CatData = imProc.match.match_catsHTMmerged(AI.CatData);
        AI.CatData.JD = JD(Iim);
        
        AC(Iim) = AI.CatData;

        New_FN  = strrep(Int_FN,'-nd-int.','-nd-mycat.');
        FITS.writeTable1(AC(Iim),New_FN); 
    end

    %ResInd = imProc.match.matchReturnIndices(AC, AC(1));
    %MAC = imProc.match.match(AC, AC(1));

    MS = MatchedSources;
    [MS, Matched] = unifiedCatalogsIntoMatched(MS, AC, 'MatchedColums',Args.MatchedColums);
    MS.JD = JD;

    MS.write1('MatchedSources.hdf5')
    
end

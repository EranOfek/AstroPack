    function [Result,JD] = createMatchedSources(Files, Args)
    %
    % Example: Result = wfast.createMatchedSources('WFAST_Balor_20210529-011312-278_F505W_0_Image.h5z')
    %          [Result,JD] = wfast.createMatchedSources('WFAST_Balor_20210529-0025*','UseRegExp',false)
    %          [Result,JD] = wfast.createMatchedSources('WFAST*.h5z','UseRegExp',false)
    
    arguments
        Files
        
        Args.Gain                      = 0.8;
        Args.PsfFunPar                 = {[0.1;1.5;3]};
        Args.TrimPix                   = 10;
        Args.MaxBack                   = 1000;
        
        Args.ReadType                  = 'image';
        Args.UseRegExp(1,1) logical    = true;
        Args.CalibDir                  = '/data/euler/archive/WFAST/calibration';
        Args.CalibFile                 = 'calibration_2021-03-07_WFAST_Balor.mat';
        Args.CalibObj                  = [];
    end
    
    
    % load Calibration object
    if isempty(Args.CalibObj)
        PWD = pwd;
        cd(Args.CalibDir);
        cd  /data/euler/archive/WFAST/calibration
        CalibObj = io.files.load2(Args.CalibFile);
        cd(PWD);
    else
        CalibObj = Args.CalibObj;
    end
        
    
    List  = io.files.filelist(Files,Args.UseRegExp);
    Nlist = numel(List);
    Result = AstroCatalog;
    Ind    = 0;
    for Ilist=1:1:Nlist
        FileName = List{Ilist};
        
        AI = wfast.read2AstroImage(FileName, 'CalibObj',CalibObj, 'Gain',Args.Gain);
        
        Median = median(AI.Image(:));
        if Median<Args.MaxBack
            Ind
            Ind = Ind + 1;
            
            % estimate background
            %imProc.background.filterSources(AI);
            imProc.background.background(AI);

            % measure sources
            imProc.sources.findMeasureSources(AI, 'PsfFunPar',Args.PsfFunPar);

            % trim sources near edges
            CCDSEC = imUtil.ccdsec.trim_ccdsec(size(AI.Image), Args.TrimPix);
            AI.CatData.cropXY(CCDSEC,'UpdateXY',false);

            % remove cosmic rays
            SN1  = getCol(AI.CatData,'SN_1');
            SN2  = getCol(AI.CatData,'SN_2');
            Flag = SN2>5 & SN2>SN1;

            AI.CatData.Catalog = AI.CatData.Catalog(Flag,:);
            
            % ds9.plot(AI.CatData.Catalog(Flag,1:2))

            % store catalog
            Result(Ind) = AI.CatData;
            JD(Ind)     = julday(AI);
        end
        
    end
    
    % match the sources
    if 1==0
    [MC,UM,TUM] = imProc.match.matchOld(Result,Result(1),'Radius',2,'CooType','pix',...
                    'ColCatX','X', 'ColCatY','Y',...
                    'ColRefX','X', 'ColRefY','Y');
    
    % create a MatchedSources object
    MS = MatchedSources;
    MS.addMatrix(MC, {'X','Y','SN_1','SN_2','SN_3',...
        'FLUX_CONV_2','FLUX_CONV_3','BACK_IM','VAR_IM',...
        'X2','Y2',...
        'FLUX_APER_2','FLUXERR_APER_2',...
        'MAG_CONV_2',...
        'BACK_ANNULUS','STD_ANNULUS',...
        'Nmatch','Dist'});
    
    MS.JD = JD(:);
    
    MS.Data.MAG= MS.Data.MAG_CONV_2;                                    
    MS.Data.MAGERR = 1.086.*MS.Data.FLUXERR_APER_2./MS.Data.FLUX_APER_2;

    
    end
                
end
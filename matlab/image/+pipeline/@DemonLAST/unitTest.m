function Result = unitTest(Args)
    % a unit test for DemonLast
    % NB: if running at a LAST node with Args.Insert2DB  = true,
    % use the last_operational credentials in Args.AstroDBArgs ! 
    % 
    % Author: A.M. Krassilchtchikov (Oct 2023)

    arguments
        Args.RestoreNew = true;  % copy the raw data back to new 
        Args.Insert2DB  = false; % whether to perform the DB part
        Args.AstroDBArgs cell  = {'Host','socsrv','DatabaseName','lastdb','Port',5432};
%         Args.AstroDBArgs cell  = {'Host','10.23.1.25','DatabaseName','last_operational','Port',5432};  % use this when running on a LAST node
        Args.DB_ImageBulk   logical = false; % whether to use bulk or direct injection method
        Args.DB_CatalogBulk logical = true;  % whether to use bulk or direct injection method
        % for some test we'd need all the epoch products, but the output will weigh 12 Gb instead of 1 Gb! 
        Args.SaveEpochProduct = {[],[],'Cat',[]}; % {'Image','Mask','Cat','PSF'}; 
    end
    
    I = Installer;
    BaseDir = I.getDataDir('LASTpipelineUnitTest');
    
    CatsHTMdir = strcat(BaseDir,'/catsHTM/');
    startup('AstroPack_CatsHTMPath',CatsHTMdir)
    
    D = pipeline.DemonLAST;
    D.setPath(BaseDir);
%     D.RefPath = strcat(BaseDir,'/reference/');   % not needed?  

    D.main('StopButton',false,'StopWhenDone',true,'HostName','last08w',...
           'Insert2DB',Args.Insert2DB,'AstroDBArgs',Args.AstroDBArgs,...
           'DB_ImageBulk',Args.DB_ImageBulk,'DB_CatalogBulk',Args.DB_CatalogBulk,...
           'SaveEpochProduct',Args.SaveEpochProduct);
    
    if Args.RestoreNew % copy the raw data back to new
        % NB: this is hard-coded, because the particular observation
        % used for the unitTest and distributed with Installer is of 2023/06/16 
        CurrentDir = pwd; cd(BaseDir);
        !cp 2023/06/16/raw/LAST*fits new/  
        cd(CurrentDir);
    end
   
    Result = true;
    
end



function TestDataProducts
    %

    % cd to data products directory


    %% Test the X1 and X position in the image catalogs
    AC=AstroCatalog('LAST.*_010_sci_proc_Cat_1.fits');
    T=AC(1).toTable;

    % test 1st moment position
    DiffX = median(T.XPEAK-T.X1, 'all', 'omitnan');
    DiffY = median(T.YPEAK-T.Y1, 'all', 'omitnan');
    if abs(DiffX)>0.1 || abs(DiffY)>0.1
        error('X1/Y1 1st moment positions are not consistent with XPEAK/YPEAK positions');
    end

    FracInRangeX = sum(abs(T.XPEAK-T.X1)>0.5)./numel(T.X1);
    FracInRangeY = sum(abs(T.YPEAK-T.Y1)>0.5)./numel(T.Y1);
    if FracInRangeX>0.1 || FracInRangeY>0.1
        error('High fraction of X1/Y1 position with difference larger than 0.5 pix relative to XPEAK/YPEAK');
    end

    % test X/Y PSF fitting position
    FracInRangeXp = sum(abs(T.XPEAK-T.X)>0.5)./numel(T.X);
    FracInRangeYp = sum(abs(T.YPEAK-T.Y)>0.5)./numel(T.Y);
    if FracInRangeXp>0.1 || FracInRangeYp>0.1
        error('High fraction of X1/Y1 position with difference larger than 0.5 pix relative to XPEAK/YPEAK');
    end

    % looks good
    plot(T.SN_3, T.SN_3./T.SN_5,'.')
    % looks good
    plot(T.SN_3, T.SN_3./T.SN_1,'.')

    % NOT good
    plot(T.SN, T.SN./T.SN_1,'.')

    % looks like a bias at S/N>1000 (the one from our paper)
    semilogx(T.SN_3, T.BACK_ANNULUS./T.BACK_IM,'.')

    % looks like a severe bias - the variance is factor of 2 higher than
    % expected - CORRECTED (issue 300)
    hist(T.BACK_IM./T.VAR_IM)

    % should be 1 - not too bad
    median(T.PSF_CHI2DOF)


    %% Read all images of a vist
    AI=AstroImage.readFileNamesObj('LAST*sci_proc_Image_1.fits');

    LL = AI.getStructKey({'LIMMAG','PH_ZP','PH_COL1','PH_RMS','FWHM'});
    hist([LL.LIMMAG])
    plot([LL.PH_ZP],[LL.LIMMAG],'.')
    hist([LL.FWHM])
    plot([LL.FWHM],[LL.LIMMAG],'.')
    
end
function Result = unitTest()
    % ImageWCS.unitTest
    
    io.msgStyle(LogLevel.Test, '@start', 'AstroWCS test started')

    % Test DS9 only on Linux/Mac
    have_ds9 = ds9.supported();

    % Change to data directory
    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);  

    % construct an empty AstroWCS
    AW = AstroWCS([2 2]);

    % construct a AstroWCS from AstroHeader with full TAN projection
    Im_name = 'FOCx38i0101t_c0f.fits';
    AH = AstroHeader(Im_name);
    AW = AstroWCS.header2wcs(AH);

    % Test for header without some: CD matrix missing, no CD matrix,  partial PC matrix, no PC matrix
    ValCD = cell2mat(AH.getCellKey({'CD1_1','CD1_2','CD2_1','CD2_2'}));

    AH.deleteKey({'CD1_2','CD2_2'});
    AW = AstroWCS.header2wcs(AH); % should fill with zeros

    AH.deleteKey({'CD1_1','CD2_1'});
    AH.insertKey({'CDELT1',ValCD(1);'CDELT2',ValCD(4)}); 
    AW = AstroWCS.header2wcs(AH); % should give diagonal CD with CDELT

    AH.insertKey({'PC1_1',1;'PC2_2',1});
    AW = AstroWCS.header2wcs(AH); % should give diagonal CD using PC and filling with zeros

    AH.insertKey({'PC1_2',ValCD(2)/ValCD(1);'PC2_1',ValCD(3)/ValCD(4)});
    AW = AstroWCS.header2wcs(AH); % using CDELT and PC should give identical CD to original

    % Test with no projection (fill ProjType=ProjClass='none')
    AH.replaceVal({'CTYPE1','CTYPE2'},{'RA','DEC'});
    AW = AstroWCS.header2wcs(AH);

    % Test with no radesys info
    AH.deleteKey('EQUINOX');
    AW = AstroWCS.header2wcs(AH);

    % Test with only radesys (no equinox)
    AH.insertKey({'RADESYS','FK5'});
    AW = AstroWCS.header2wcs(AH);

    % xy2sky tests
    RAD = 180./pi;

    % get [alpha, delta] for TAN projection
    %Im_name = 'FOCx38i0101t_c0f.fits';
    Im_name = 'WD0802+387-S019-Luminance-R001-Luminance.fts';
    HDU=1;

    %Im_name = 'coj1m011-fl12-20180413-0057-e91.fits.fz';
    %HDU = 2;

    AH = AstroHeader(Im_name,HDU);
    PX = rand(1,500) * AH.Key.NAXIS1;
    PY = rand(1,500) * AH.Key.NAXIS2;           

    AW = AstroWCS.header2wcs(AH);
    [Alpha, Delta]  = AW.xy2sky(PX,PY);

    % Test DS9 only on Linux/Mac
    if have_ds9
        ds9(Im_name);
        [ds9_alpha,ds9_delta] = ds9.xy2coo(PX,PY,AW.RADESYS);
        d_mas = convert.angular('rad','mas',(celestial.coo.sphere_dist_fast(Alpha'./RAD,Delta'./RAD,ds9_alpha./RAD,ds9_delta./RAD)));
        disp(sprintf('Max distance for TAN projection (xy2sky vs. ds9) is %.1f [mas]',max(d_mas)));
    end

    % test sky2xy for TAN. 
    % First compare to xy2sky and then compared to ds9
    [PX1,PY1]  = AW.sky2xy(Alpha,Delta);
    d_pix = sqrt((PX-PX1).^2 + (PY-PY1).^2);
    disp(sprintf('Max distance for TAN projection (xy2sky<->sky2xy) is %.1f [mili-pix]',max(d_pix)*1000));

    if have_ds9
        [ds9_PX1,ds9_PY1] = ds9.coo2xy(Alpha, Delta);
        d_pix = sqrt((ds9_PX1'-PX1).^2 + (ds9_PY1'-PY1).^2);
        disp(sprintf('Max distance for TAN projection (sky2xy vs. ds9) is %.1f [mili-pix]',max(d_pix)*1000));
    end

    % test cropWCS
    OrigCRPIX = AW.CRPIX;
    AW = AW.cropWCS(AW.CRPIX,'centerCRPIX',true);
    [Alpha2, Delta2]  = AW.xy2sky(PX-OrigCRPIX(1)+1,PY-OrigCRPIX(1)+1);
    d_mas2 = convert.angular('rad','mas',(celestial.coo.sphere_dist_fast(Alpha'./RAD,Delta'./RAD,Alpha2'./RAD,Delta2'./RAD)));
    disp(sprintf('Max distance for TAN projection (cropped WCS #1) is %.1f [mas]',max(d_mas2)));
    
    AW = AstroWCS.header2wcs(AH);
    [Alpha, Delta]  = AW.xy2sky(PX,PY);
    AW = AW.cropWCS([1,AH.Key.NAXIS1,1,AH.Key.NAXIS2],'centerCRPIX',true);
    [Alpha2, Delta2]  = AW.xy2sky(PX,PY);
    d_mas2 = convert.angular('rad','mas',(celestial.coo.sphere_dist_fast(Alpha'./RAD,Delta'./RAD,Alpha2'./RAD,Delta2'./RAD)));
    disp(sprintf('Max distance for TAN projection (cropped WCS #2) is %.1f [mas]',max(d_mas2)));
    
    % construct a AstroWCS from Header with TPV projection and get [alpha, delta]
    %Im_name = 'tpv.fits';
    Im_name = 'WD0548-001_2457842_215821_Clear_meter.fits';
    AH = AstroHeader(Im_name);
    PX = rand(1,500) * AH.Key.NAXIS1;
    PY = rand(1,500) * AH.Key.NAXIS2;

    AW = AstroWCS.header2wcs(AH);
    [Alpha, Delta]  = AW.xy2sky(PX,PY);

    if have_ds9
        ds9(Im_name);
        [ds9_alpha,ds9_delta] = ds9.xy2coo(PX,PY,AW.RADESYS);
        d_mas = convert.angular('rad','mas',(celestial.coo.sphere_dist_fast(Alpha'./RAD,Delta'./RAD,ds9_alpha./RAD,ds9_delta./RAD)));
        disp(sprintf('Max distance for TPV projection (xy2sky vs. ds9) is %.1f [mas]',max(d_mas)));
    end

    % test sky2xy for TPV. 
    % First compare to xy2sky and then compared to ds9
    [PX1,PY1]  = AW.sky2xy(Alpha,Delta);
    d_pix = sqrt((PX-PX1).^2 + (PY-PY1).^2);
    disp(sprintf('Max distance for TPV projection (xy2sky<->sky2xy) is %.1f [mili-pix]',max(d_pix)*1000));          

    if have_ds9            
        [ds9_PX1,ds9_PY1] = ds9.coo2xy(Alpha, Delta);
        d_pix = sqrt((ds9_PX1'-PX1).^2 + (ds9_PY1'-PY1).^2);
        disp(sprintf('Max distance for TPV projection (sky2xy vs. ds9) is %.1f [mili-pix]',max(d_pix)*1000));            
    end

    % construct a AstroWCS from Header with TAN-SIP projection  and get [alpha, delta]
    Im_name = 'SPITZER_I1_70576896_0000_0000_1_bcd.fits';
    AH = AstroHeader(Im_name);
    PX = rand(1,500) * AH.Key.NAXIS1;
    PY = rand(1,500) * AH.Key.NAXIS2;

    AW = AstroWCS.header2wcs(AH); 
    [Alpha, Delta]  = AW.xy2sky(PX,PY);

    if have_ds9            
        ds9(Im_name);
        [ds9_alpha,ds9_delta] = ds9.xy2coo(PX,PY,AW.RADESYS);
        d_mas = convert.angular('rad','mas',(celestial.coo.sphere_dist_fast(Alpha'./RAD,Delta'./RAD,ds9_alpha./RAD,ds9_delta./RAD)));
        disp(sprintf('Max distance for TAN-SIP projection (xy2sky vs. ds9) is %.1f [mas]',max(d_mas)));
    end

    % test sky2xy for  TAN-SIP. 
    % First compare to xy2sky and then compared to ds9
    [PX1,PY1]  = AW.sky2xy(Alpha,Delta);
    d_pix = sqrt((PX-PX1).^2 + (PY-PY1).^2);
    disp(sprintf('Max distance for  TAN-SIP projection (xy2sky<->sky2xy) is %.1f [mili-pix]',max(d_pix)*1000));          

    if have_ds9
        [ds9_PX1,ds9_PY1] = ds9.coo2xy(Alpha, Delta);
        d_pix = sqrt((ds9_PX1'-PX1).^2 + (ds9_PY1'-PY1).^2);
        disp(sprintf('Max distance for  TAN-SIP projection (sky2xy vs. ds9) is %.1f [mili-pix]',max(d_pix)*1000)); 
    end

    % Check with no distortions
    [Alpha_no, Delta_no]  = AW.xy2sky(PX,PY,'includeDistortion',false);
    d_mas = convert.angular('rad','mas',(celestial.coo.sphere_dist_fast(Alpha./RAD,Delta./RAD,Alpha_no./RAD,Delta_no./RAD)));
    disp(sprintf('Max distance for TAN-SIP projection (compared to no distortion) is %.1f [mas]',max(d_mas)));            

    [PX1_no,PY1_no]  = AW.sky2xy(Alpha,Delta,'includeDistortion',false);
    d_pix = sqrt((PX1_no-PX1).^2 + (PY1_no-PY1).^2);
    disp(sprintf('Max distance for TAN-SIP projection (compared to no distortion) is %.1f [mili-pix]',max(d_pix)*1000));             

   % Check with no RevPV
    AW.RevPV = AstroWCS.DefPVstruct; % clear fields
    [PX1,PY1]  = AW.sky2xy(Alpha,Delta);
    d_pix = sqrt((PX-PX1).^2 + (PY-PY1).^2);
    disp(sprintf('Max distance for TAN-SIP projection (xy2sky<->sky2xy no RevPV) is %.1f [mili-pix]',max(d_pix)*1000));               

    % construct a AstroWCS from AstroHeader with Naxis=3, and empty
    % projtype in CTYPE3 and get [alpha, delta]
    Im_name = 'WFPC2u5780205r_c0fx.fits';
    AH = AstroHeader(Im_name);
    PX = rand(1,500) * AH.Key.NAXIS1;
    PY = rand(1,500) * AH.Key.NAXIS2; 

    AW = AstroWCS.header2wcs(AH,'read2axes',true);
    [Alpha, Delta]  = AW.xy2sky(PX,PY);

    if have_ds9
        ds9(Im_name);
        [ds9_alpha,ds9_delta] = ds9.xy2coo(PX,PY,AW.RADESYS);
        d_mas = convert.angular('rad','mas',(celestial.coo.sphere_dist_fast(Alpha'./RAD,Delta'./RAD,ds9_alpha./RAD,ds9_delta./RAD)));
        disp(sprintf('Max distance for Naxis=3 (xy2sky vs. ds9) is %.1f [mas]',max(d_mas)));
    end

    % test sky2xy for  AstroWCS from AstroHeader with Naxis=3. 
    % First compare to xy2sky and then compared to ds9
    [PX1,PY1]  = AW.sky2xy(Alpha,Delta);
    d_pix = sqrt((PX-PX1).^2 + (PY-PY1).^2);
    disp(sprintf('Max distance for Naxis=3 (xy2sky<->sky2xy) is %.1f [mili-pix]',max(d_pix)*1000));          

    if have_ds9            
        [ds9_PX1,ds9_PY1] = ds9.coo2xy(Alpha, Delta);
        d_pix = sqrt((ds9_PX1'-PX1).^2 + (ds9_PY1'-PY1).^2);
        disp(sprintf('Max distance for Naxis=3 (sky2xy vs. ds9) is %.1f [mili-pix]',max(d_pix)*1000));            
    end    
    
    
    % Construct AstroWCS from Tran2D
    TC=Tran2D; 
    TC.symPoly; TC.ParX = ones(1,13);TC.ParY = ones(1,13);
    TC.polyCoef; TC.polyRep;

    NAXIS = 2; CRPIX(1,:) = [1.0 1.0]; CRVAL(1,:) = [0.0 0.0];
    CD = eye(2); CTYPE(1,:) = {'RA---TPV' 'DEC--TPV'}; CUNIT(1,:) = {'deg' 'deg'};

    AW = AstroWCS.tran2wcs(TC,'NAXIS',NAXIS,'CRPIX',CRPIX,'CRVAL',CRVAL,'CD',CD,'CTYPE',CTYPE,'CUNIT',CUNIT);
    [Alpha, Delta]  = AW.xy2sky(PX,PY,'includeDistortion',false);
    [PX1,PY1]  = AW.sky2xy(Alpha,Delta,'includeDistortion',false);
    d_pix = sqrt((PX-PX1).^2 + (PY-PY1).^2);
    disp(sprintf('Max distance for Tran2D (TPV) (xy2sky<->sky2xy) is %.1f [mili-pix]',max(d_pix)*1000));            

    % Construct AstroWCS from Tran2D after real fit
    load('Result.mat');
    NAXIS = 2;             CTYPE(1,:) = {'RA---TPV' 'DEC--TPV'}; CUNIT(1,:) = {'deg' 'deg'};
    CD = Result.ParWCS(1).CD;
    CRPIX =  Result.ParWCS(1).CRPIX;
    CRVAL =  Result.ParWCS(1).CRVAL;
    TC = Result.Tran(1);
    TC.PolyRep;
    AW = AstroWCS.tran2wcs(TC,'NAXIS',NAXIS,'CRPIX',CRPIX,'CRVAL',CRVAL,'CD',CD,'CTYPE',CTYPE,'CUNIT',CUNIT);
    [Alpha, Delta]  = AW.xy2sky([1;1000],[1;1000]);
    assert(Alpha(1)~=Alpha(2));
    assert(Delta(1)~=Delta(2));

    AI = AstroImage('PTF_Cropped.fits');

    if have_ds9
        ds9(AI);
        load('AstrometricCat_PTF_Cropped.mat');
        [PX,PY]  = AW.sky2xy(AstrometricCat.Catalog(:,1),AstrometricCat.Catalog(:,2),'InUnits','rad');
        ds9.plot([PX,PY]);
    end

    % Construct/update AstroHeader from AstroWCS
    AH1 = AW.wcs2header;           
    AH = AW.wcs2header(AH);

    % test other things

    cd(PWD);                
    io.msgStyle(LogLevel.Test, '@passed', 'AstroWCS test passed');
    Result = true;            
    
end


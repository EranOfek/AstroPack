function [Result] = transmissionZP_testN3(Args)
    % test function to process catalogs of LAST N3 survey with the absolute photometry calibration suite  
    %     Optional detailed description
    % Input  : - none    
    %          * ...,key,val,... 
    % Output : - catalogs augmented with absolute photometry data and written to disk as FITS files  
    % Author : A.M. Krassilchtchikov (2025 Dec) 
    % Example: D = db.Db.connectLASTdb('Pass','*'); 
    %          Res1 = imProc.calib.transmissionZP_testN3('DB',D,'FieldID','1679.c','MountNum',2,...
    %                            'OutDir','/Data2/test/AbsCalib/','RemoteDir','/bigdata2/projects/temp/');
    %          Res2 = imProc.calib.transmissionZP_testN3('DB',D,'FieldID','1678.c','MountNum',9);
    arguments
        Args.FieldID           = '1679.c'; % []
        Args.MountNum          = 2; % []; % 2;
        Args.CamNum            = 1; % []; % 1;
        Args.CropID            = []; % 10;
        Args.AddConstraints    = []; % e.g. 'jd_start > 2.46086240482600e+06 and jd_start < 2.46086240482700e+06'
        Args.Table             = 'N3_visit_images';
        Args.DB                = [];
        Args.OutDir            = '~/Data2/test/'; % '/Data2/test/AbsCalib/'
        Args.RemoteDir         = []; % '/bigdata2/projects/temp/';
    end

    % Check that DB connection is provided
    if isempty(Args.DB)
        error('transmissionZP_testN3:NoDB', ...
              'Database connection required. Call with: D = db.Db.connectLASTdb(''Pass'',''*''); transmissionZP_testN3(''DB'', D);');
    end

    %
    if isempty(Args.FieldID)
        QField = '1>0';
    else
        QField = sprintf('(fieldid = ''%s'' or fieldid = ''%s'')',Args.FieldID, Args.FieldID(1:4));
    end
    %
    if isempty(Args.MountNum)
        QMount = '';
    else
        QMount = sprintf('and mountnum = %d',Args.MountNum);
    end
    %
    if isempty(Args.CamNum)
        QCam = '';
    else
        QCam = sprintf('and camnum = %d',Args.CamNum);
    end
    %
    if isempty(Args.CropID)
        QCrop = '';
    else
        QCrop = sprintf('and cropid = %d',Args.CropID);
    end
    %
    if isempty(Args.AddConstraints)
        QAdd = '';
    else
        QAdd = sprintf('and %s',Args.AddConstraints);
    end

    Q = sprintf('select * from %s where %s %s %s %s %s',...
        Args.Table, QField, QMount, QCam, QCrop, QAdd);

    T2 = Args.DB.query(Q);

    save('QueryResult.mat','T2');

    Nvis = height(T2);    
        
    for Ivis = 1:Nvis
        % construct the file name (later will use an AstroFileName object)
        Mt  = compose('%02d',T2.mountnum(Ivis)); Cam = compose('%02d',T2.camnum(Ivis));
        YY  = compose('%04d',T2.diryear(Ivis)); MM = compose('%02d',T2.dirmon(Ivis)); 
        DD = compose('%02d',T2.dirday(Ivis));
        if str2double(extractBetween(T2.filetime(Ivis),1,2)) < 12
            DD2 = compose('%02d',T2.dirday(Ivis)+1);
        else
            DD2 = DD;
        end
        
        FN = strcat('/mnt/euclid/last/data/LAST.01.',Mt,'.',Cam,'/',YY,'/',MM,'/',DD,...
            '/proc/',T2.subdir(Ivis),'/LAST.01.',Mt,'.',Cam,'_',YY,MM,DD2,'.',T2.filetime(Ivis),...
            '_clear_',string(T2.fieldid(Ivis)),'_000_001_',compose('%03d',T2.cropid(Ivis)),...
            '_sci_coadd_Image_1.fits');
        
        % read the data files into an AI
        AI = AstroImage.readProducts(FN,'ExtraOutProduct',"Cat");

        % diagnostics:
        fprintf('%d: %s\n', Ivis, FN)
        
        % process the AI (this is the main part where absolute calibration is performed)  
        [~,~, Result(Ivis)] = imProc.calib.fitPhotCalibTrans(AI, 'addZP', true, 'Verbose', false);
        
        % write the output catalog to file 
        FN1 = strcat(Args.OutDir,'/LAST.01.',Mt,'.',Cam,'/',YY,'/',MM,'/',DD,...
            '/proc/',T2.subdir(Ivis),'/LAST.01.',Mt,'.',Cam,'_',YY,MM,DD2,'.',T2.filetime(Ivis),...
            '_clear_',string(T2.fieldid(Ivis)),'_000_001_',compose('%03d',T2.cropid(Ivis)),...
            '_sci_coadd_Cat_1.fits');
        
        AI.write1(FN1,'CatData','OverWrite',true,'MkDir',true);
       
        clear AI;

        if ~isempty(Args.RemoteDir) && Result(Ivis).NCalUsed > 0 
            try
                tools.os.copyFileOverNFS(FN1, Args.RemoteDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
            catch ME
                fprintf('%d: movement of processed file %s failed due to %s\n', Ivis, FN1, ME.message)
            end
        end
    end
end

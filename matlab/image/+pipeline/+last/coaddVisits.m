function [CI, AI] = coaddVisits(In, Args)
    % Coadd multiple LAST visits of a single field
    %     Given a list of visits path, or a table from which the bvisit
    %     paths can be constructed, select all the coadd images of the same
    %     field and CropID in all the visits, register them and coadd them.
    % Input  : -  A cell array or a strings array of paths for list of
    %             visits to coadd.
    %             Alternatively, a table from which the paths can be
    %             constructed. For example, a table generated by 
    %             pipeline.DemonLAST/findAllVisits can be used.
    %          * ...,key,val,... 
    %            'CropID' - A vector of CropID to coadd.
    %                   Default is [10].
    %            'BasePath' - Proc data directory base path.
    %                   Default is '/marvin'
    %            'NodeNum' - LAST node number. Default is 1.
    %            'ProjName' - Project Name in image name. Default is 'LAST.
    %            'TempName' - File emplate name to coadd.
    %                   Default is '*_%03d_sci_coadd_Image_1.fits'
    %            'StackMethod' - Coadd method used by imProc.stack.coadd
    %                   Default is 'sigmaclip'
    %            'StackArgs' - Cell array of StackArgs argument to pass to
    %                   imProc.stack.coadd
    %                   Default is {'MeanFun',@tools.math.stat.nanmean, 'StdFun', @tools.math.stat.std_mad, 'Nsigma',[2 2]}
    % Output : - An AstroImage array of coadd images. Image per CropID.
    %          - An AstroImage array of registered images.
    %            Different Columns for different CropID.
    % Author : Eran Ofek (2024 May) 
    % Example: D=pipeline.DemonLAST; D.BasePath='/marvin/LAST.01.10.01';
    %          [Res,T,FT]=D.findAllVisits;
    %          F=strcmp(T.FieldID,"1441");
    %
    %          CI=pipeline.last.coaddVisits(T(F,:),'CropID',10);
    %          

    arguments
        In
        Args.CropID            = [10];
        Args.BasePath          = '/marvin'
        Args.NodeNum           = 1;
        Args.ProjName          = 'LAST';

        Args.TempName          = '*_%03d_sci_coadd_Image_1.fits';

        Args.StackMethod       = 'sigmaclip';      
        Args.StackArgs         = {'MeanFun',@tools.math.stat.nanmean, 'StdFun', @tools.math.stat.std_mad, 'Nsigma',[2 2]};

        Args.FindSrc logical   = true;
    end

    % Construct paths for visits
    if istable(In)
        Mount = In.MountNum;
        Cam   = In.CamNum;
        Visit = In.Visit;
        Year  = In.Year;
        Month = In.Month;
        Day   = In.Day;
        N = numel(Day);
        List = strings(N,1);
        for I=1:1:N
            ProjectID = sprintf('%s.%02d.%02d.%02d', Args.ProjName, Args.NodeNum, Mount(I), Cam(I));
            List(I) = fullfile(Args.BasePath, ProjectID, sprintf('%04d',Year(I)), sprintf('%02d',Month(I)), sprintf('%02d',Day(I)), 'proc', Visit(I));
        end

    else
        List = In;
        N    = numel(List);
    end

    Ncrop = numel(Args.CropID);

    % go over all visits
    for I=1:1:N
        cd(List{I});

        % read the images
        for Icrop=1:1:Ncrop
            TempName = sprintf(Args.TempName, Args.CropID(Icrop));
            AI(I, Icrop)    = AstroImage.readFileNamesObj(TempName);
        end
    end

    % need background and variance due to a bug in coadd:
    %AI = imProc.background.background(AI);

    % register and coadd
    for Icrop=1:1:Ncrop
        AI(:,Icrop) = imProc.transIm.interp2wcs(AI(:,Icrop),AI(1,Icrop));
        %AI(:,Icrop) = imProc.transIm.interp2wcs(AI(:,Icrop),AI(1,Icrop), 'DataProp',{'Image','Back','Var','Mask'});

        CI(Icrop)   = imProc.stack.coadd(AI(:,Icrop), 'StackMethod',Args.StackMethod, 'StackArgs',Args.StackArgs);
        %CI(Icrop)   = imProc.stack.coaddW(AI(:,Icrop), 'StackMethod',Args.StackMethod, 'StackArgs',Args.StackArgs);
    end

    if Args.FindSrc
        CI=imProc.background.background(CI);                             
        CI=imProc.sources.findMeasureSources(CI);
        CI=imProc.astrometry.addCoordinates2catalog(CI,'OutUnits','deg');
        CI = imProc.psf.populatePSF(CI);
        CI=imProc.calib.photometricZP(CI);
        
    end
    
end

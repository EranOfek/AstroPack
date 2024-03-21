function [StitchedImage, AH, RemappedXY] = stitch(InputImages, Args)
    % Make a mosaic sky image from a set of input image files or AstroImages
    % Package: imProc.stack 
    % Input:   - A mask FITS file namesto stich into a single large image.
    %            Alternatively, this can be asn AstroImage object
    %            containing the images.
    %          * ...,key,val,...
    %          'DataDir'       : The directory containing the input images
    %          'PixScale'      : [arcsec] The pixel scale (LAST by def.)
    %          'Crop'          : X1 X2 Y1 Y2 margin sizes of the input images to be cropped out
    %          'Method'        : pixel redistribution method on the mosaic image
    %          'Exposure'      : exposure time to be written into the header of the mosaic image
    %          'ZP'            : zero point to be written into the header of the mosaic image
    %          'LASTnaming'    : whether the image file names are in the LAST convention form
    %          'SizeMargin'    : number of margin pixels added to X and Y size of the mosaic image
    %          'OutDir'        : output directory
    %          'PlotBorders'   : whether to plot the sky region with original image stamps
    %          
    % Output : - StitchedImage: an AstroImage containing a mosaic made of all the input images
    %          - AH: the header of the mosaic image containing the exposure, ZP and the WCS
    %          - RemappedXY: a 4D-matrix containing the remapped X, Y pixel
    %            coordinates from each of the input images: [Image number, Xremapped, Yremapped, 1 -> X or 2 -> Y]
    %            
    % Tested : Matlab R2020b
    % Author : A. Krassilchtchikov et al. (May 2023), function name copyright: Y. Shvartzvald
    % Example: [Mosaic, AH, RemappedXY] = imProc.stack.stitch('LAST*coadd_Image*.fits','DataDir','/home/sasha/Obs1/','PixScale',1.25);

    arguments
        
        InputImages         =    'LAST*coadd_Image*.fits';       % The mask of the input image filenames
        Args.DataDir        =    '/home/sasha/Obs1/';            % The directory containing the input images
        Args.PixScale       =    1.25;                           % [arcsec] The pixel scale (LAST by def.)
        Args.Crop           =    [20 20 20 20];                  % X1 X2 Y1 Y2 margin sizes of the input images to be cropped out
        Args.Method         =    'redistribute';                 % pixel redistribution method on the mosaic image
        Args.Exposure       =    0;                              % exposure time to be written into the header of the mosaic image
        Args.ZP             =    0;                              % zero point to be written into the header of the mosaic image
        Args.LASTnaming logical = true;                          % whether the image file names are in the LAST convention form
        Args.SizeMargin     =    [30 30];                        % number of margin pixels added to X and Y size of the mosaic
                                                                 % (needed due to the insufficient accurancy of the mosaic size determination)
        Args.OutDir         =   '.';                             % output directory
        Args.PlotBorders logical   =   false;                    % whether to plot the sky region with original image stamps

    end
    
    % set some constants and image parameters 
    
    RAD  = 180./pi;                     % the radian
    Tiny = 1e-14;                       % a small, but nonzero constant
    
    PixScale = Args.PixScale / 3600;    % [deg] pixel scale
    
    % read the input images
    
            cprintf('hyper','%s\n','Mosaicking started'); tic
            fprintf('Reading input images.. ');
            
    if isa(InputImages,'AstroImage')
        
        AI = InputImages;
        AI = populateWCS(AI); % TBC: do we really need it in all the cases? 
        
    else

        cd(Args.DataDir)
        
        if Args.LASTnaming
            
            FN = FileNames.generateFromFileName( InputImages );
            if numel(FN) < 1
                fprintf('No images found. Please, check the path and the template\n');
                return
            end
            AI = AstroImage.readFileNamesObj( FN ) ;  
            
        else 
            
            ImageFiles  = dir ( InputImages ) ;
            ImNum = numel(ImageFiles);
            if ImNum < 1
                fprintf('No images found. Please, check the path and the template\n');
                return
            end
            Imfiles = repmat({''}, ImNum, 1);
            for Img = 1:1:ImNum
                Imfiles{Img} = fullfile(ImageFiles(Img).folder, ImageFiles(Img).name);
            end
%             AI = AstroImage.readFileNamesObj( Imfiles ); % produces an error, probably, due to some changes in AstroImage.readFileNamesObj
            AI = AstroImage( Imfiles );
        end

    end
    
    NImage = size(AI,2);                % determine the number of images to be stitched    
    
            fprintf('%d%s\n',NImage,' images loaded');
    
%     RA11  = zeros(1,NImage);  RA22 = RA11; RA12 = RA11; RA21 = RA11; 
%     DEC11 = zeros(1,NImage); DEC22 = DEC11; DEC12 = DEC11; DEC21 = DEC11; 

    Exptime = zeros(NImage,1);      PH_ZP   = zeros(NImage,1); 
    Corn    = zeros(NImage,4,2);    Cent    = zeros(NImage,2);
    Xsize   = zeros(NImage,1);      Ysize   = zeros(NImage,1);

            % test output: why is the resulting FITS image so weird in the first variant?
        
% %             TestOutputImage = double ( AI(1).ImageData.Image ); % double does not help..
% %             imUtil.util.fits.fitswrite(TestOutputImage,'!./testoutput1.fits');    

%             AI(1).write1('!./testoutput0.fits'); % this works fine
            
    % determine the borders of the input images and read their exposure times, 
    % crop the input images so that border effects are eliminated
        
    for Img = 1:1:NImage
        
        % read the exposures and photometric zero points:
        
        Position     = strcmp(AI(Img).Header,'EXPTIME');            % extract a header
        Exptime(Img) = AI(Img).Header{Position,2};                  % get the value
        
        Position     = strcmp(AI(Img).Header,'PH_ZP');              % extract a header
        if max(Position,[],'all') > 0
            PH_ZP(Img)   = AI(Img).Header{Position,2};              % get the value
        end
        
        % determine the image sizes and find their corners:
                
        Xsize(Img) = size(AI(Img).Image,1);
        Ysize(Img) = size(AI(Img).Image,2);
        
        Corn(Img,:,:) = AI(Img).cooImage([1 Xsize(Img) 1 Ysize(Img)]).Corners;
        Cent(Img,:)   = AI(Img).cooImage([1 Xsize(Img) 1 Ysize(Img)]).Center;
        
        % crop the images and determine new subimage sizes:
        
%         CCDSEC = [Args.Crop(1) Xsize(Img)-Args.Crop(2) Args.Crop(3) Ysize(Img)-Args.Crop(4)];
%         AI(Img).crop(CCDSEC); % WCS should be updated here on a regular basis! 
%         AI(Img).WCS = AI(Img).WCS.cropWCS(CCDSEC);
%         % Need to write a correct version of cropWCS. Currently, WCS.cropWCS just shifts the
%         % reference point and deletes the distorsion information
%         
%         Xsize(Img) = size(AI(Img).Image,1);
%         Ysize(Img) = size(AI(Img).Image,2);
        
        % or just start the copying from CCDSEC ?
              
        % AI.cooImage gets data from the header and not from the
        % WCS with AI.WCS.xy2sky: is it exactly the same in all the cases?
        
%         [RA11(Img), DEC11(Img)]  = AI(Img).WCS.xy2sky(1,1);
%         [RA22(Img), DEC22(Img)]  = AI(Img).WCS.xy2sky(Xsize,Ysize);
%         [RA21(Img), DEC21(Img)]  = AI(Img).WCS.xy2sky(Xsize,1);
%         [RA12(Img), DEC12(Img)]  = AI(Img).WCS.xy2sky(1,Ysize);
        
    end
    
            % test output: the resulting FITS image is still very weird in the first case?
            
% %             imUtil.util.fits.fitswrite(AI(1).ImageData.Image,'!./testoutput1_cropped.fits');       
% 
%             AI(1).write1('!./testoutput0_cropped.fits');
            
    % determine the sky size of the mosaic 
    
%     RA1m  = min([RA11 RA22  RA21  RA12]);    RA2m  = max([RA11 RA22  RA21  RA12]);
%     DEC1m = min([DEC11 DEC22 DEC21 DEC12]);  DEC2m = max([DEC11 DEC22 DEC21 DEC12]);
    
    RA1m  = min(Corn(:,:,1),[],'all');  RA2m = max(Corn(:,:,1),[],'all');
    DEC1m = min(Corn(:,:,2),[],'all'); DEC2m = max(Corn(:,:,2),[],'all');
    
    RAcenter = (RA2m + RA1m)/2; DECcenter = (DEC2m + DEC1m)/2; 

    % plot the sky regions of the input images and the reference points 

    if Args.PlotBorders
        
            figure(1); hold on

            for Img = 1:1:NImage    
                plot([Corn(Img,1,1) Corn(Img,2,1) Corn(Img,3,1) Corn(Img,4,1) Corn(Img,1,1)], ...
                     [Corn(Img,1,2) Corn(Img,2,2) Corn(Img,3,2) Corn(Img,4,2) Corn(Img,1,2)]);
                text(Cent(Img,1),Cent(Img,2), num2str(Img) );
            end

            plot(RAcenter,DECcenter,'rd','MarkerSize',10);
            plot([RA1m  RA2m  RA2m  RA1m  RA1m], ...
                 [DEC1m DEC1m DEC2m DEC2m DEC1m], 'LineWidth',2,'Color',[.6 0 0]);
            xlabel RA; ylabel DEC;
            hold off    
            
    end

    % determine the sky and pixel size of the mosaic
    
    SizeRA  = RAD * celestial.coo.sphere_dist(RA1m    , DECcenter, RA2m,     DECcenter,'deg');
    SizeDEC = RAD * celestial.coo.sphere_dist(RAcenter, DEC1m,     RAcenter, DEC2m,    'deg');
    
    NPix1 = ceil( SizeRA  / PixScale ) + Args.SizeMargin(1);    
    NPix2 = ceil( SizeDEC / PixScale ) + Args.SizeMargin(2);    
    
            cprintf('hyper','%s%4.0f%s%4.0f%s\n','The mosaic size is ',NPix1,' x ',NPix2,' pixels');
            fprintf('%s%4.0f%4.0f%4.0f%4.0f\n','Number of pixels cropped out from each of the input images (XL, XR, YL, YR): '...
                    ,Args.Crop);
    
    % make arrays for the mosaic count map and exposure map 
    
    ImageM = zeros(NPix1, NPix2);        % the counts map
    ExposM = Tiny * ones(NPix1, NPix2);  % the exposure map (will appear in the denominator, thus use Tiny values)
    
    RemappedXY = zeros(NImage,max(Xsize),max(Ysize),2);
    
    AIm = AstroImage({ImageM});
    
    % make a simple TAN WCS of the mosaic image from scratch 
    
    AIm.WCS = AstroWCS();
    AIm.WCS.ProjType  = 'TAN';
    AIm.WCS.ProjClass = 'ZENITHAL';
    AIm.WCS.CooName   = {'RA'  'DEC'};
    AIm.WCS.CTYPE     = {'RA---TAN','DEC---TAN'};
    AIm.WCS.CUNIT     = {'deg', 'deg'};
    AIm.WCS.CD(1,1)   = PixScale;
    AIm.WCS.CD(2,2)   = PixScale;  
    AIm.WCS.CRVAL(1)  = RAcenter;
    AIm.WCS.CRVAL(2)  = DECcenter;
    AIm.WCS.CRPIX(1)  = NPix1/2;
    AIm.WCS.CRPIX(2)  = NPix2/2;
    AIm.WCS.AlphaP    = RAcenter;
    AIm.WCS.DeltaP    = DECcenter;
    AIm.WCS.PhiP      = 180; 

    % trying to warp the input images according to the new WCS frame: 
    % whole NaN images appear in AI2 for any sampling?
    % Something with the displacement matrix? 

%      AI2 = imProc.transIm.imwarp(AI,AIm.WCS,'Sampling',10,...
%          'InterpMethod','bilinear','SmoothEdges',0,'ReplaceNaN',0); 
%     AI2 = imProc.transIm.imwarp(AI,AIm.WCS,'Sampling',1); 
%     AI2 = imProc.transIm.imwarp(AI(1),AIm.WCS,'Sampling',1); 

    % interp2 or griddedinterpolant would not work because we do not have
    % a regular grid in RAimg DECimg (interpolation requires a monotonic
    % and pleid grid)

    % remap each of the pixels from the input images into the mosaic image 

    for Img = 1:1:NImage
        
                fprintf('%s%4.0d%s%4.0d\n','Processing tile ',Img,' out of ',NImage);
        
        SubImage = AI(Img).ImageData.Image;
        
        % get a grid of RA, DEC of a subimage and convert them to X, Y of the merged image
        
        XL = Args.Crop(1)+1; XR = Xsize(Img)-Args.Crop(2);
        YL = Args.Crop(3)+1; YR = Ysize(Img)-Args.Crop(4);
        
        [Ximg, Yimg]    = meshgrid( YL:YR, XL:XR );                % a grid of subimage pixels !NOTE: Ximg is of Ysize!
        [RAimg, DECimg] = AI(Img).WCS.xy2sky(Ximg, Yimg);          % RA, DEC of subimage pixels
        [X, Y]          = AIm.WCS.sky2xy(RAimg, DECimg);           % mosaic pixel coordinates corresponding to the subimage pixels
        
        RemappedXY(Img,XL:XR,YL:YR,1) = X; RemappedXY(Img,XL:XR,XL:YR,2) = Y; % save the remapped grid as one of the function's outputs
        
        Xred = XR - XL + 1;    % the reduced number of pixels
        Yred = YR - YL + 1;    % the reduced number of pixels

%             % here we can make a displacement matrix, but how should we use it?
%             Disp(:,:,1) = Ximg-X;
%             Disp(:,:,2) = Yimg-Y;

        ImageM = imProc.stack.addImageRedistributePixels(ImageM, SubImage, X, Y, 'Nx', Xred, 'Ny', Yred, ...
                                                         'XL', XL-1, 'YL', YL-1,'Method',Args.Method);
        ExposM = imProc.stack.addImageRedistributePixels(ExposM, Exptime(Img), X, Y, 'Nx', Xred, 'Ny', Yred, ...
                                                         'XL', XL-1, 'YL', YL-1,'Method',Args.Method);

        
    end
    
    % flux conservation check
    
    FluxM   = sum(ImageM,'all'); FluxAI = zeros(NImage,1);
    for Img = 1:1:NImage 
        FluxAI(Img) = sum ( AI(Img).Image(Args.Crop(1)+1:Xsize(Img)-Args.Crop(2),Args.Crop(3)+1:Ysize(Img)-Args.Crop(4)),'all'); 
    end
    Cons    = abs( 1- FluxM / sum(FluxAI,'all') );
    fprintf('%s%6.3d\n','Total flux conservation: ',Cons);
    
    % make a CPS image 
    
    CPS = ImageM ./ ExposM;
    
%             % illustartive plots of the counts, exposure, and CPS
%             figure(2)
%             subplot(1,2,1); imagesc(ImageM);
%             subplot(1,2,2); imagesc(ExposM);
%             figure(3); imagesc(CPS);
    
    % make the output structures
    
    StitchedImage = AstroImage({CPS'});
    StitchedImage.WCS = AIm.WCS;
    
    AH = StitchedImage.WCS.wcs2header;       % make a header from the WCS
    StitchedImage.HeaderData.Data = AH.Data; % add the header data to the AstroImage
    
    % add some keywords to the header
    if Args.Exposure ~= 0  % use the user-provided exposure time 
        StitchedImage.setKeyVal('EXPTIME',Args.Exposure);
    else                   % use the exposure time from the first image in the list
        StitchedImage.setKeyVal('EXPTIME',Exptime(1));
    end
    
    if Args.ZP ~= 0  % use the user-provided exposure time 
        StitchedImage.setKeyVal('ZP',Args.ZP);
    else                   % use the zero point from the first image in the list
        StitchedImage.setKeyVal('ZP',PH_ZP(1)); 
    end
    
    AH = StitchedImage.Header;               % save the header back from the AstroImage
    
    % make a FITS file:

%     if isa(InputImages,'AstroImage')
    if ~exist('FN','var')
        CPSOutputName = strcat('!./','stitched_image.fits');
    else
        CPSOutputName = strcat('!./',FN.ProjName{1},'_',FN.Time{1},'_',FN.FieldID{1},'_stitched_image.fits');
    end

    cd(Args.OutDir)
%     StitchedImage.write1(CPSOutputName); % write the image and header to a FITS file
    FITS.write(StitchedImage.Image, CPSOutputName, 'Header',StitchedImage.HeaderData.Data,...
                    'DataType','single', 'Append',false,'OverWrite',true,'WriteTime',true);

    
    cprintf('hyper','Mosaic constructed, see the output AstroImage and FITS image file.\n');    
    fprintf('%s\n',CPSOutputName(2:end));
    toc
    
end

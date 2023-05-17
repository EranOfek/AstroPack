function StitchedImage = stitch(Args)
    % Make a mosaic sky image from a set of input AstroImages
    % Package: imProc.stack
    % Description: Make a mosaic sky image from a set of input AstroImages
    %          - Args.DataDir       : The directory containing the input images
    %          - Args.InputImages   : The mask of the input image filenames
    %          - Args.PixScale      : [arcsec] The pixel scale (LAST by def.)
    %          - Args.Crop          : X1 X2 Y1 Y2 margin sizes of the input images to be cropped out
    %          - Args.Method        : pixel redistribution method on the mosaic image
    %          
    % Output : - Mosaic: a 2D array containing the resulting image in sky (?) coordinates
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    May 2023
    % Example: Mosaic = mosaic('DataDir','/home/sasha/Obs1/','InputImages','LAST*coadd_Image*.fits','PixScale',1.25);

    arguments
        
        Args.DataDir        =    '/home/sasha/Obs1/';            % The directory containing the input images
        Args.InputImages    =    'LAST*coadd_Image*.fits';       % The mask of the input image filenames
        Args.PixScale       =    1.25;                           % [arcsec] The pixel scale (LAST by def.)
        Args.Crop           =    [20 20 20 20];                  % X1 X2 Y1 Y2 margin sizes of the input images to be cropped out
        Args.Method         =    'redistribute'                  % 
        
    end
    
    % set some constants and image parameters
    
    RAD  = 180./pi;                     % the radian
    Tiny = 1e-14;                       % a small, but nonzero constant
    
    PixScale = Args.PixScale / 3600;    % [deg] pixel scale
    
    % read the input images
    
            cprintf('hyper','%s\n','Mosaicking started'); tic
            fprintf('Reading input images.. ');

    cd(Args.DataDir);
    FN = FileNames.generateFromFileName( Args.InputImages );
    AI = AstroImage.readFileNamesObj( FN) ;  
    
    NImage = size(AI,2);                % determine the number of images to be merged
    
            fprintf('%d%s\n',NImage,' images loaded');
    
%     RA11  = zeros(1,NImage);  RA22 = RA11; RA12 = RA11; RA21 = RA11; 
%     DEC11 = zeros(1,NImage); DEC22 = DEC11; DEC12 = DEC11; DEC21 = DEC11; 

    Exptime = zeros(NImage,1);
    Corn    = zeros(NImage,4,2);    Cent    = zeros(NImage,2);
    Xsize   = zeros(NImage,1);      Ysize   = zeros(NImage,1);

            % test output: why is the resulting FITS image so weird in the first case?
        
%             TestOutputImage = double ( AI(1).ImageData.Image ); % double does not help..
%             imUtil.util.fits.fitswrite(TestOutputImage,'!./testoutput1.fits');    

            AI(1).write1('!./testoutput0.fits'); % this works fine
            
    % determine the borders of the input images and read their exposure times, 
    % crop the input images so that border effects are eliminated
        
    for Img = 1:1:NImage
        
        % read the exposures:
        
        Position     = strcmp(AI(Img).Header,'EXPTIME');            % extract a header
        Exptime(Img) = AI(Img).Header{Position,2};                  % get the value
        
        % determine the image sizes and find their corners:
                
        Xsize(Img) = size(AI(Img).Image,1);
        Ysize(Img) = size(AI(Img).Image,2);
        
        Corn(Img,:,:) = AI(Img).cooImage([1 Xsize(Img) 1 Ysize(Img)]).Corners;
        Cent(Img,:)   = AI(Img).cooImage([1 Xsize(Img) 1 Ysize(Img)]).Center;
        
        % crop the images and determine new subimage sizes:
        
%         CCDSEC = [Args.Crop(1) Xsize(Img)-Args.Crop(2) Args.Crop(3) Ysize(Img)-Args.Crop(4)];
%         AI(Img).crop(CCDSEC); % WCS should be updated here on a regular basis! 
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
            
%             imUtil.util.fits.fitswrite(AI(1).ImageData.Image,'!./testoutput1_cropped.fits');       

            AI(1).write1('!./testoutput0_cropped.fits');
            
    % determine the sky size of the mosaic 
    
%     RA1m  = min([RA11 RA22  RA21  RA12]);    RA2m  = max([RA11 RA22  RA21  RA12]);
%     DEC1m = min([DEC11 DEC22 DEC21 DEC12]);  DEC2m = max([DEC11 DEC22 DEC21 DEC12]);
    
    RA1m  = min(Corn(:,:,1),[],'all');  RA2m = max(Corn(:,:,1),[],'all');
    DEC1m = min(Corn(:,:,2),[],'all'); DEC2m = max(Corn(:,:,2),[],'all');
    
    RAcenter = (RA2m + RA1m)/2; DECcenter = (DEC2m + DEC1m)/2; 

            % plot the sky regions of the input images and the reference points 

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

    % determine the sky and pixel size of the mosaic
    
    SizeRA  = RAD * celestial.coo.sphere_dist(RA1m    , DECcenter, RA2m,     DECcenter,'deg');
    SizeDEC = RAD * celestial.coo.sphere_dist(RAcenter, DEC1m,     RAcenter, DEC2m,    'deg');
    
    NPix1 = ceil( SizeRA  / PixScale );    
    NPix2 = ceil( SizeDEC / PixScale );    
    
            cprintf('hyper','%s%4.0f%s%4.0f%s\n','The mosaic size is ',NPix1,' x ',NPix2,' pixels');
    
    % make arrays for the mosaic count map and exposure map 
    
    ImageM = zeros(NPix1, NPix2);        % the counts map
    ExposM = Tiny * ones(NPix1, NPix2);  % the exposure map (will appear in the denominator, thus use Tiny values)
    
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
    
%                 % a check: convert the pixel coordinates to sky coordinates:
% 
%                 [RA,DEC] = AIm.WCS.xy2sky(1,        1)
%                 [RA,DEC] = AIm.WCS.xy2sky(1,    NPix2)
%                 [RA,DEC] = AIm.WCS.xy2sky(NPix1,NPix2)
%                 [RA,DEC] = AIm.WCS.xy2sky(NPix1,    1)

    % trying to warp the input images according to the new WCS frame: 
    % whole NaN images appear in AI2 for any sampling?

%     AI2 = imProc.transIm.imwarp(AI,AIm.WCS,'Sampling',10); 
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
        
        XL = Args.Crop(1); XR = Xsize(Img)-Args.Crop(2);
        YL = Args.Crop(3); YR = Ysize(Img)-Args.Crop(4);
        
        [Ximg, Yimg]    = meshgrid( YL:YR, XL:XR );                % a grid of subimage pixels !NOTE: Ximg is of Ysize!
        [RAimg, DECimg] = AI(Img).WCS.xy2sky(Ximg, Yimg);          % RA, DEC of subimage pixels
        [X, Y]          = AIm.WCS.sky2xy(RAimg, DECimg);           % mosaic pixel coordinates corresponding to the subimage pixels
        
        Xred = Xsize(Img) - ( Args.Crop(1) + Args.Crop(2) - 1);    % the reduced number of pixels
        Yred = Ysize(Img) - ( Args.Crop(3) + Args.Crop(4) - 1);    % the reduced number of pixels

%             % here we can make a displacement matrix, but how should we use it?
%             Disp(:,:,1) = Ximg-X;
%             Disp(:,:,2) = Yimg-Y;
        
        % Note: the pixel coordinates from sky2xy are not whole numbers,
        % so we are to choose a count redistribution method (see below)
        
        switch lower(Args.Method)
            
            case 'direct'   % will lead to empty pixels in the image, but does smear the dimest objects 
                
                X0 = round(X); Y0 = round(Y);        

                for iX = 1:1:Xred      %   
                    
                    imX = iX + Args.Crop(1);
                    
                    for iY = 1:1:Yred  %Ysize(Img)                          
                        
                        imY = iY + Args.Crop(3);
                        
                        ImageM ( X0(iX,iY), Y0(iX,iY) ) = ImageM ( X0(iX,iY), Y0(iX,iY) ) + SubImage (imX, imY);  % add counts from the subimage to the mosaic
                        ExposM ( X0(iX,iY), Y0(iX,iY) ) = ExposM ( X0(iX,iY), Y0(iX,iY) ) + Exptime(Img);         % add exposure of a subimage to the mosaic

                    end
                end
                
            case 'redistribute' % will make a smoother image with no holes, but can smear the dimest objects 

                X0 = floor(X);   Y0 = floor(Y);    
                X1 = X-X0;       Y1 = Y-Y0;    X1Y1 = X1 .* Y1;       

                for iX = 1:1:Xred 
                    
                    imX = iX + Args.Crop(1);
                    
                    for iY = 1:1:Yred 
                        
                        imY = iY + Args.Crop(3);

                        S = SubImage (imX, imY);

                        X11 = X0(iX,iY); Y11 = Y0(iX,iY);

                        ImageM ( X11  , Y11 )   = ImageM ( X11  , Y11 )   + S * ( 1 - X1(iX,iY) ) * (1 - Y1(iX,iY) );
                        ImageM ( X11+1, Y11+1 ) = ImageM ( X11+1, Y11+1 ) + S * X1Y1(iX,iY); 
                        ImageM ( X11+1, Y11 )   = ImageM ( X11+1, Y11 )   + S * X1(iX,iY) * (1 - Y1(iX,iY) );
                        ImageM ( X11  , Y11+1 ) = ImageM ( X11  , Y11+1 ) + S * ( 1 - X1(iX,iY) ) * Y1(iX,iY); 

                        ExposM ( X11  , Y11 )   = ExposM ( X11, Y11 )     + Exptime(Img) * ( 1 - X1(iX,iY) ) * (1 - Y1(iX,iY) );      
                        ExposM ( X11+1, Y11+1 ) = ExposM ( X11+1, Y11+1 ) + Exptime(Img) * X1Y1(iX,iY); 
                        ExposM ( X11+1, Y11 )   = ExposM ( X11+1, Y11 )   + Exptime(Img) * X1(iX,iY) * (1 - Y1(iX,iY) );
                        ExposM ( X11  , Y11+1 ) = ExposM ( X11  , Y11+1 ) + Exptime(Img) * ( 1 - X1(iX,iY) ) * Y1(iX,iY); 

                    end
                end
                
            otherwise
                
                cprintf('err','Incorrect flux redistribution method, exiting..')
                return
                
        end
        
    end
    
    % flux conservation check
    
    FluxM   = sum(ImageM,'all');
    for Img = 1:1:NImage 
        FluxAI(Img) = sum ( AI(Img).Image(Args.Crop(1):Xsize(Img)-Args.Crop(2),Args.Crop(3):Ysize(Img)-Args.Crop(4)),'all'); 
    end
    Cons    = abs( 1- FluxM / sum(FluxAI,'all') );
    fprintf('%s%6.3d\n','Total flux conservation: ',Cons);
    
    % make a CPS image 
    
    CPS = ImageM ./ ExposM;
    
%             % illustartive plots of the counts, exposure, and CPS
% 
%             figure(2)
% 
%             subplot(1,2,1); imagesc(ImageM);
%             subplot(1,2,2); imagesc(ExposM);
% 
%             figure(3)
% 
%             imagesc(CPS);
    
    % test output
    
    StitchedImage = AstroImage({CPS'});
    StitchedImage.WCS = AIm.WCS;
    AH = StitchedImage.WCS.wcs2header;       % make a header from the WCS
    StitchedImage.HeaderData.Data = AH.Data; % add the header data to the AstroImage
    % add an exposure keyword
    CPSOutputName = '!./CPS_Obs1a.fits';
    StitchedImage.write1(CPSOutputName); % write the fits image with the WCS data in the header
    
%     % the resulting FITS image is still very weird? 
%     imUtil.util.fits.fitswrite(CPS','!./testoutput_CPS.fits');   
    
    cprintf('hyper','Mosaic constructed \n');
    
    toc
    
end






%     % make a grid of RA, DEC on the merged image
% 
%     [X,Y] = meshgrid(1:NPix1,1:NPix2);
%     [RA,DEC] = AIm.WCS.xy2sky(X,Y);
%     
%     % make a grid of their pixels
%     
%     [Xim1,Yim1]    = meshgrid(1:Xsize(1),1:Ysize(1));
%     [RAim1,DECim1] = AI(1).WCS.xy2sky(Xim1,Yim1);  
%     
%     Image1    = double( reshape(AI(1).ImageData.Image, Xsize(1) * Ysize(1), 1) );
%     
%     RAvec  = reshape(RA',NPix1 * NPix2,1);
%     DECvec = reshape(DEC',NPix1 * NPix2,1);
%     
%     RAim1vec  = reshape(RAim1' ,Xsize(1) * Ysize(1),1);
%     DECim1vec = reshape(DECim1',Xsize(1) * Ysize(1),1);
    
%   All the interpolation methods require that X and Y be monotonic and
%   plaid (as if they were created using MESHGRID).  If you provide two
%   monotonic vectors, interp2 changes them to a plaid internally.
%   X and Y can be non-uniformly spaced.
%    
%   Hence, neither interp2 nor griddedInterpolant will not work, 
%   it looks like the only option is scatteredInterpolant

%     tic
% 
%     int = scatteredInterpolant(RAim1vec,DECim1vec,Image1,'linear','none');
%     
%     toc; tic
%     
%     footpr = zeros(NPix1,NPix2);
%     footpr = int(RA,DEC);
%     footpr(isnan(footpr))=0; % there is no option to make 0 in scatteredInterpolant, so an additional step is needed
%     
%     toc
    
    % find the image most close to the center of the mosaic and copy the
    % WCS from it into the WCS of the mosaic image
        
%     CentNum  = 0;     % initially non-existant central tile number
%     Dist0    = 100;   % initially large central tile distance from the center of the mosaic
%     
%     for Img = 1:1:NImage
%         
%         Dist(Img) = RAD * celestial.coo.sphere_dist(Cent(Img,1),Cent(Img,2),RAcenter,DECcenter,'deg');
%         
%         if Dist(Img) < Dist0
%             
%             CentNum = Img;
%             Dist0   = Dist(Img);
%             
%         end
%         
%     end
%     
%     fprintf('%s%4.0f%s%3.2f%s%4.0f%s\n','The nearest tile center number',CentNum, ' is at ', ...
%            Dist0*60,' arcmin = ',Dist0/PixScale,' pix from the mosaic center');  
%        
%     % read the WCS from the tile number CentNum into an AstroHeader
%        
%     AH = AI(CentNum).WCS.wcs2header; 
%            
%     % determine mosaic pixel coordinates of the reference pixel 
%     % of the tile number CentNum
%     
%     CRVAL1   = AH.getVal('CRVAL1'); CRVAL2   = AH.getVal('CRVAL2');
%     CRPIX1   = AH.getVal('CRPIX1'); CRPIX2   = AH.getVal('CRPIX2');
%     
%     % change the pixel coordinates of the reference point 
%     % NEED TO CHECK THIS!
%      
%     DeltaRA  = CRVAL1 - RAcenter;
%     DeltaDEC = CRVAL2 - DECcenter;
%     DeltaX   = DeltaRA/PixScale;
%     DeltaY   = DeltaDEC/PixScale;
%     CRPIX1_new = CRPIX1 + DeltaX + NPix1/2; 
%     CRPIX2_new = CRPIX2 + DeltaY + NPix2/2;
%     
%     AH.setVal('CRPIX1',CRPIX1_new);
%     AH.setVal('CRPIX2',CRPIX2_new);
%     
%     % create a new WCS from the updated AstroHeader and attach it to the mosaic image
%     
%     AIm.WCS = AstroWCS.header2wcs(AH);
%     
%     [RAref, DECref] = AIm.WCS.xy2sky(CRPIX1_new,CRPIX2_new);
%     
%     % plot the sky regions of the input images and the reference points 
%     
%     figure(2); hold on
%     
%     for Img = 1:1:NImage    
%         plot([Corn(Img,1,1) Corn(Img,2,1) Corn(Img,3,1) Corn(Img,4,1) Corn(Img,1,1)], ...
%              [Corn(Img,1,2) Corn(Img,2,2) Corn(Img,3,2) Corn(Img,4,2) Corn(Img,1,2)]);
%         text(Cent(Img,1),Cent(Img,2), num2str(Img) );
%     end
%     
%     plot(RAcenter,DECcenter,'rd','MarkerSize',10);
%     plot([RA1m  RA2m  RA2m  RA1m  RA1m], ...
%          [DEC1m DEC1m DEC2m DEC2m DEC1m], 'LineWidth',2,'Color',[.6 0 0]);
%     plot(RAref,DECref,'bo','MarkerSize',10);
%     xlabel RA; ylabel DEC;
%     hold off    
%     
%     % fill in the data from the input images
%     
%     for Img = 1:1:24 %NImage
%         
%           fprintf('%s%4.0d%s%4.0d\n','Processing tile ',Img,' out of ',NImage);
% 
%           % determine the position of the first pixel
%           
%           [RA1, DEC1]     = AI(Img).WCS.xy2sky(1,1);
%           [XX1,  YY1]     = AIm.WCS.sky2xy(RA1,DEC1);
%           X1 = round(XX1);        Y1 = round(YY1); 
%           X2 = X1 + Xsize(Img)-1; Y2 = Y1 + Ysize(Img)-1;  
%           
%           if X1 < 1 || Y1 < 1 || X2 > NPix2 || Y2 > NPix1
%               
%               cprintf('err','Out of image borders, exiting..\n');
%               return;
%               
%           end
%           
%           % add image to the mosaic:
%           
%           AIm.ImageData.Data( X1:X2, Y1:Y2 ) = AIm.ImageData.Data( X1:X2, Y1:Y2 ) + ...
%                                                AI(Img).ImageData.Data( 1:Xsize(Img),1:Ysize(Img) );
%                                     
%           % add flat exposure map to the mosaic exposure map:
%           ExposM( X1:X2, Y1:Y2 ) = ExposM( X1:X2, Y1:Y2 ) + ...
%                                    Exptime(Img) * ones( Xsize(Img), Ysize(Img) );
%                             
%     end
%     
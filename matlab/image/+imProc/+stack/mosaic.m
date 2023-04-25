function Mosaic = mosaic(Args)
    % Make a mosaic sky image from a set of input AstroImages
    % Package: imProc.stack
    % Description: Make a mosaic sky image from a set of input AstroImages
    %          - Args.DataDir       : The directory containing the input images
    %          - Args.InputImages   : The mask of the input image filenames
    %          - Args.PixScale      : [arcsec] The pixel scale (LAST by def.)
    %          
    % Output : - Mosaic: a 2D array containing the resulting image in sky (?) coordinates
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    May 2023
    % Example: Mosaic = mosaic();

    arguments
        
        Args.DataDir        =    '/home/sasha/Obs1/';            % The directory containing the input images
        Args.InputImages    =    'LAST*coadd_Image*.fits';       % The mask of the input image filenames
        Args.PixScale       =    1.25;                           % [arcsec] The pixel scale (LAST by def.)
        
    end
    
    % set some image parameters
    
    RAD = 180./pi;                      % the radian
    
    PixScale = Args.PixScale / 3600;    % [deg] pixel scale
    
    % read the input images

    cd(Args.DataDir);
    FN=FileNames.generateFromFileName( Args.InputImages );
    AI=AstroImage.readFileNames(FN);  
    
    NImage = size(AI,2);                % determine the number of images to be merged
    
%     RA1  = zeros(1,NImage);  RA2 = zeros(1,NImage); 
%     DEC1 = zeros(1,NImage); DEC2 = zeros(1,NImage); 

    Corn = zeros(NImage,4,2);
    Cent = zeros(NImage,2);
    
    % read in the borders of the input images
    %
    % NB: in the case of LAST the input images are aligned so that North
    % is up and West is to the right, but with ULTRASAT it would not be so, 
    % and the corners of the image will not determine the border values of
    % RA, DEC !
        
    for Img = 1:1:NImage
        
        Xsize = size(AI(Img).Image,1);
        Ysize = size(AI(Img).Image,2);
        
        % AI.cooImage gets data from the header and not from the
        % WCS with AI.WCS.xy2sky: is it exactly the same?
        Corn(Img,:,:) = AI(Img).cooImage([1 Xsize 1 Ysize]).Corners;
        Cent(Img,:)   = AI(Img).cooImage([1 Xsize 1 Ysize]).Center;
        
%         [RA1(Img), DEC1(Img)]  = AI(Img).WCS.xy2sky(1,1);
%         [RA2(Img), DEC2(Img)]  = AI(Img).WCS.xy2sky(Xsize,Ysize);
        
    end
    
    % determine the sky size of the mosaic 
    
%     RA1m  = min([RA1 RA2]);    RA2m  = max([RA1 RA2]);
%     DEC1m = min([DEC1 DEC2]);  DEC2m = max([DEC1 DEC2]);
    
    RA1m  = min(Corn(:,:,1),[],'all');  RA2m = max(Corn(:,:,1),[],'all');
    DEC1m = min(Corn(:,:,2),[],'all'); DEC2m = max(Corn(:,:,2),[],'all');
    
    RAcenter = (RA2m + RA1m)/2; DECcenter = (DEC2m + DEC1m)/2; 
    
    % plot the sky regions of the input images
    
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

    % determine the pixel size of the mosaic
    
    SizeRA  = RAD * celestial.coo.sphere_dist(RA1m    , DECcenter, RA2m,     DECcenter,'deg');
    SizeDEC = RAD * celestial.coo.sphere_dist(RAcenter, DEC1m,     RAcenter, DEC2m,    'deg');
    
    NPix1 = ceil( SizeRA  / PixScale );
    NPix2 = ceil( SizeDEC / PixScale );   
    
    cprintf('hyper','%s%4.0f%s%4.0f%s\n','The mosaic size is ',NPix1,' x ',NPix2,' pixels');
    % why not 6400 x 9600 pix? is the real LAST pixel size larger than 1.25"?
    
    ImageM = zeros(NPix1, NPix2);
    
    AIm = AstroImage({ImageM'});
    
    % find the image most close to the center of the mosaic and copy the
    % WCS from it into the WCS of the mosaic image
        
    CentNum  = 0;     % initially non-existant central tile number
    Dist0    = 100;   % initially large central tile distance from the center of the mosaic
    
    for Img = 1:1:NImage
        
        Dist(Img) = RAD * celestial.coo.sphere_dist(Cent(Img,1),Cent(Img,2),RAcenter,DECcenter,'deg');
        
        if Dist(Img) < Dist0
            
            CentNum = Img;
            Dist0   = Dist(Img);
            
        end
        
    end
    
    fprintf('%s%4.0f%s%3.2f%s%4.0f%s\n','The nearest tile center number',CentNum, ' is at ', ...
           Dist0*60,' arcmin = ',Dist0/PixScale,' pix from the mosaic center');  
       
    % determine mosaic pixel coordinates of the reference pixel 
    % of the tile number CentNum
    
    % read the WCS from the nearest tile into an AstroHeader
       
    AH = AI(CentNum).WCS.wcs2header; 
    
    % change the pixel coordinates of the reference point
    
    % create a new WCS from the AstroHeader and attach it to the mosaic image
    
    AW = AstroWCS.header2wcs(AH);
    
    AIm = AstroImage({ImageM'},'WCS', AW);
    
end
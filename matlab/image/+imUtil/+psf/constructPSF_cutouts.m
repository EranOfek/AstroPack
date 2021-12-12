function constructPSF_cutouts(Image, XY, Args)
    % Given a background-subtracted image and PSF star positions, construct a PSF stamp from cutouts
    %
    
    arguments
        Image          % 2D image or cube of cutouts
        XY             % XY positions of sources in image
        Args.mexCutout logical     = true;
        Args.Circle logical        = false;
        Args.ReCenter logical      = true;    % call moment2
        Args.MomRadius             = 8;
                
        Args.ShiftMethod           = 'lanczos';   % 'lanczos' | 'fft'
        Args.IsCircFilt logical    = true;
        Args.PadVal                = 0;
    end
            
    
    MaxRadius = Args.MomRadius;
    
    [Cube, RoundX, RoundY, X, Y] = imUtil.image.image2cutouts(Image, X, Y, MaxRadius, 'mexCutout',Args.mexCutout, 'Circle',Args.Circle);
    
    if Args.ReCenter
        M1 = imUtil.image.moment2(Cube, X, Y, 'MomRadius',Args.MomRadius);
    end
    
    SizeCube = size(Cube);
    Ncube    = SizeCube(3);
    Xcen     = SizeCube(2).*0.5;
    Ycen     = SizeCube(1).*0.5;
    
    ShiftXY  = [Xcen - M1.X, Ycen - M1.Y];
    
    switch lower(Args.ShiftMethod)
        case 'lanczos'
            [ShiftedCube] = imUtil.trans.shift_lanczos(Cube, ShiftXY, A, Args.IsCircFilt, Args.PadVal);
            
        case 'fft'
            Icube = 1;
            ShiftedCube = zeros(Cube);
            
            [ShiftedCube(:,:,Icube),NY,NX,Nr,Nc] = imUtil.trans.shift_fft(Cube(:,:,Icube), ShiftXY(Icube,1), ShiftXY(Icube,2));
            for Icube=2:1:Ncube
                ShiftedCube(:,:,Icube) = imUtil.trans.shift_fft(Cube(:,:,Icube), ShiftXY(Icube,1), ShiftXY(Icube,2), NY,NX,Nr,Nc);
            end
            
        otherwise
            error('Unknown ShiftMethod option');
    end
    
    % sum all shifted cutouts
     may need photometry for calibration???
    
end
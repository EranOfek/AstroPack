function processChandraPSF(Args)
    % process the PSF fits files produced with MARX and make a .mat object
    % with all the PSFs for the given chip
    arguments
        Args.WDir   = '~/ChandraPSF/';
        Args.ChipID = 0;
        Args.Energy = [0.2 0.36986 1.2649 4.3259 8.0];     
        Args.ChipX  = linspace(1,1024,16);
        Args.ChipY  = linspace(1,1024,16);
        Args.StampSize = [196 196];
    end
    
    cd(Args.WDir); 
    
    Nx = numel(Args.ChipX); 
    Ny = numel(Args.ChipY); 
    Ne = numel(Args.Energy); 
    
    TabEn = Args.Energy; 
    TabX  = Args.ChipX; 
    TabY  = Args.ChipY; 
    StampNx = Args.StampSize(1); 
    StampNy = Args.StampSize(2); 
    StampX = 1:StampNx; 
    StampY = 1:StampNy; 
    
    MPSF = zeros(StampNx, StampNx, Ne, Nx, Ny, 'single'); % 196 * 196 * 5 * 16 * 16 * 4 / 1024^2 = 188 Mb
    
    % read the data
    Dir = sprintf('ccd%d',Args.ChipID);
    cd(Dir)
    
    for Ien = 1:Ne
        for Ix = 1:Nx
            for Iy = 1:Ny
                if Args.ChipID < 4 % ACIS-I
%                     FileName = sprintf('18073_%d%de%d_ccd%d.psf',ChipX(Ix),ChipY(Iy),Energy(Ien),Args.ChipID); 
                else % ACIS-S
%                     FileName = sprintf('13659_%d%de%d_ccd%d.psf',ChipX(Ix),ChipY(Iy),Energy(Ien),Args.ChipID); 
                end
                FileName = '18073_ra149.1587_dec69.5214_e1.3_ccd0.0.psf';
                Stamp = FITS.read1(FileName);
                % pad if needed:
                if size(Stamp,1) < StampNx || size(Stamp,2) < StampNy
                    Stamp = imUtil.psf.pad_psf(Stamp,[StampNx StampNy]);
                end
                MPSF(:,:,Ien,Ix,Iy) = Stamp;
            end
        end
    end
     
    cd(Args.WDir);
     
    Matname = sprintf('ChandraACISchip%dPSF.mat',Args.ChipID);
    save(Matname,'MPSF','StampX','StampY','TabEn','TabX','TabY');
    
end
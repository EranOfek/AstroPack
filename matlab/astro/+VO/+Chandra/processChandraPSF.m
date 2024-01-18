function processChandraPSF(Args)
    % process the PSF fits files produced with MARX and make a .mat object
    % with all the PSFs for the given chip
    arguments
        Args.WDir   = '~/ChandraPSF/';
        Args.ChipID = 0;
        Args.Energy = [0.4, 1.3, 4.3]; % [0.2 0.36986 1.2649 4.3259 8.0];     
        Args.ChipX  = linspace(32,992,16);
        Args.ChipY  = linspace(32,992,16);
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
    for Ien = 1:Ne
        for Ix = 1:Nx
            for Iy = 1:Ny
                
                FileName = getSimulatedFileName(Args.WDir,TabX(Ix),TabY(Iy),TabEn(Ien),Args.ChipID);                
                
                FN = sprintf('%s/ccd%d/%s',Args.WDir,Args.ChipID,FileName);
                Stamp = FITS.read1(FN);
                % pad if needed:
                if size(Stamp,1) < StampNx || size(Stamp,2) < StampNy
                    Stamp = imUtil.psf.pad_psf(Stamp,[StampNx StampNy]);
                end
                MPSF(:,:,Ien,Ix,Iy) = Stamp;
            end
        end
    end
    
    % save the stamps in a .mat object
    Matname = sprintf('ChandraACISchip%dPSF.mat',Args.ChipID);
    save(Matname,'MPSF','StampX','StampY','TabEn','TabX','TabY');    
end

function FileName = getSimulatedFileName(WDir,ChipX,ChipY,Energy,ChipID)
    % read the name conversion tables and construct the data file name
 
    if ChipID < 4 % ACIS-I
        T = readtable(strcat(WDir,'/Table_coord_acis_I_wChunk.txt'));  
        ObsId = 18073;
    else          % ACIS-S
        T = readtable(strcat(WDir,'/Table_coord_acis_S_wChunk.txt'));
        ObsId = 13659;
    end    
    Line = T(T.ccd_ID == ChipID & T.chipx == ChipX & T.chipy == ChipY, :);
    FileName = sprintf('%d_ra%.4f_dec%.4f_e%.1f_ccd%d.0.psf',ObsId,Line.ra_deg,Line.dec_deg,Energy,ChipID);
end
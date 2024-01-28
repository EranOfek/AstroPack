function processChandraPSF(Args)
    % process the PSF fits files produced with MARX and make a .mat object
    % with all the PSFs for the given chip
    arguments
        Args.WDir   = '~/ChandraPSF/PSF_library_cubicshift/';
        Args.ChipID = 0;
        Args.Energy = [0.4, 1.3, 4.3]; % [0.2 0.36986 1.2649 4.3259 8.0];     
        Args.ChipX  = linspace(32,992,16);
        Args.ChipY  = linspace(32,992,16);
        Args.StampSize = [196 196];
    end
    
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
        
    Dir0 = pwd; cd(Args.WDir);
    
    for Ien = 1:Ne
        for Ix = 1:Nx
            for Iy = 1:Ny
                
                fprintf('E, X, Y: %d %d %d \n', Ien, Ix, Iy);
                  
                FileName = getSimulatedFileName(Args.WDir,TabX(Ix),TabY(Iy),TabEn(Ien),Args.ChipID);                
                
                FN = sprintf('%s/ccd%d/%s',Args.WDir,Args.ChipID,FileName);
              
                try
%                     Stamp = FITS.read1(FN);                    
                    Stamp = readmatrix(FN);
                catch
                    fprintf('%s cannot be read\n', FN);
                end
                % pad if needed:
                if size(Stamp,1) < StampNx || size(Stamp,2) < StampNy
                    Stamp = imUtil.psf.pad_psf(Stamp,[StampNx StampNy]);
                end
                MPSF(:,:,Ien,Ix,Iy) = Stamp;
            end
        end
    end
    
    cd(Dir0);
    % save the stamps in a .mat object
    Matname = sprintf('ChandraACISchip%dPSF.mat',Args.ChipID);
    save(Matname,'MPSF','StampX','StampY','TabEn','TabX','TabY');    
end

function FileName = getSimulatedFileName(WDir,ChipX,ChipY,Energy,ChipID)
    % read the name conversion tables and construct the data file name
 
    if ChipID < 4 % ACIS-I
        T = readtable(strcat(WDir,'/filenames_I.txt'));  
%         ObsId = 18073;
    else          % ACIS-S
        T = readtable(strcat(WDir,'/filenames_S.txt'));
%         ObsId = 13659;
    end    
    FileName = T.Filename(T.CCD == ChipID & T.ChipX == ChipX & T.ChipY == ChipY & T.Energy == Energy, :); 
    FileName = FileName{1};
% %     FileName = sprintf('%d_ra%.4f_dec%.4f_e%.1f_ccd%d.0.psf',ObsId,Line.ra_deg,Line.dec_deg,Energy,ChipID);
    % improve the number of digits in RA, Dec:
%     if rem(round(Line.ra_deg*1e4),10) == 0 && rem(round(Line.dec_deg*1e4),10) == 0
%         FileName = sprintf('%d_ra%.3f_dec%.3f_e%.1f_ccd%d.0_95.txt',ObsId,Line.ra_deg,Line.dec_deg,Energy,ChipID);
%     elseif ~ (rem(round(Line.ra_deg*1e4),10) == 0 ) && rem(round(Line.dec_deg*1e4),10) == 0
%         FileName = sprintf('%d_ra%.4f_dec%.3f_e%.1f_ccd%d.0_95.txt',ObsId,Line.ra_deg,Line.dec_deg,Energy,ChipID);
%     elseif rem(round(Line.ra_deg*1e4),10) == 0 && ~ ( rem(round(Line.dec_deg*1e4),10) == 0 )
%         FileName = sprintf('%d_ra%.3f_dec%.4f_e%.1f_ccd%d.0_95.txt',ObsId,Line.ra_deg,Line.dec_deg,Energy,ChipID);
%     else
%         FileName = sprintf('%d_ra%.4f_dec%.4f_e%.1f_ccd%d.0_95.txt',ObsId,Line.ra_deg,Line.dec_deg,Energy,ChipID);
%     end
end
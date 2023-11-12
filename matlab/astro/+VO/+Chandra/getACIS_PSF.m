function WPSF = getACIS_PSF(Chip, Args)
    % get a MARX-simulated Chandra ACIS PSF
    arguments
        Chip        = 0;   % the chip number can be 0,1,2,3,6,7 only
        Args.Energy = 4;   % in keV, can be in the range of [0.2 -- 8]
        Args.Spec   = [];  % a 2 column table: [keV ; photons s(-1) cm(-2) keV(-1)]
        Args.PosX   = 512; % [ChipX pixel number]
        Args.PosY   = 512; % [ChipY pixel number]
        Args.RollAngle = 0;% roll angle (degrees counterclockwise)
        Args.Normalize = true; % whether to normalize the output stamp
    end    
    
    if ~ismember(Chip,[0 1 2 3 6 7])
        error('Unsupported ACIS chip number: the library contains data on chips 0-3,6,7 only');
    end
    if isempty(Args.Spec) && Args.Energy < 0.2 && Args.Energy > 8.0
        error('The input energy is out of the valid range');
    end
    if Args.PosX > 1024 || Args.PosX < 1 || Args.PosY > 1024 || Args.PosY < 1
        error('The input XY position is out of the valid range');
    end
    
    I = Installer; Dir = I.getDataDir('Chandra_PSF');
    DataFile = sprintf('%s%s%d%s',Dir,'/ChandraACISchip',Chip,'PSF.mat');
    io.files.load1(DataFile); % should contain: MPSF, StampX, StampY, TabEn, TabX, TabY
    
    if isempty(Args.Spec) % produce a monoenergetic PSF
        WPSF = interpn(StampX, StampY, TabEn, TabX, TabY, MPSF, StampX, StampY, Args.Energy, Args.PosX, Args.PosY,...
                       'linear',0); 
    else                  % make a weighted spectrum
%         error('spectral weighting is not implemented yet');    
        Nbin  = size(Args.Spec,1)-1; 
        Wbin  = diff(Args.Spec(:,1)); % Args.Spec(2:Nbin,1)-Args.Spec(1:Nbin-1,1);
        WPSF  = zeros(numel(StampX), numel(StampY),'single');
        for Ibin = 1:Nbin
            P = interpn(StampX, StampY, TabEn, TabX, TabY, MPSF, StampX, StampY, Args.Spec(Ibin,2), Args.PosX, Args.PosY,...
                        'linear',0); 
            WPSF = WPSF + P .* Wbin(Ibin);
        end
        WPSF = WPSF ./ sum(Wbin,'all');
    end
    % rotate
    if abs(Args.RollAngle) > 1e-6 % rotate the output stamp 
        % NB: the size of the stamp may increase by ~ sqrt(2) x sqrt(2) 
        WPSF = imrotate(WPSF, Args.RollAngle, 'bilinear', 'loose'); 
    end
    % normalize
    if Args.Normalize
        WPSF = imUtil.psf.normPSF(WPSF);
    end
end

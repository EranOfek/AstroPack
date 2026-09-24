function [Cat, Mag] = extended2List(Args)
    % convert an extended source into a list of point-likes 
    % Input: -
    %       * ...,key,val,...
    %       'RA0' - RA [deg] of the object's center
    %       'Dec0' - Dec [deg] of the object's center
    %       'SizeRA' - size in RA [arcsec]
    %       'SizeDec' - size in Dec [arcsec]
    %       'Mag'     - magnitude
    %       'Oversampling' - spatial pixel quantization scale
    %       'ProfileType'  - type of spatial profile (same for both axes)
    %       'ProfilePar'   - parameters of the profile (e.g., sigma)
    %       'ProfileMatrix' - the spatial profile given as an empirical matrix 
    %       'PixSizeDeg' - the size of the telescope's pixel [deg]
    %       'Write2File' - whether to write the output table to a text file
    %       'OutputFilename' - the output file name
    % Output: - a table of RA, Dec coordinates of point-like objects [deg]
    %         - a column of point-like objects' magnitudes
    %         - a text file with the three columns listed above (optional)
    % Author: A.M. Krassilchtchikov (Mar 2024)
    % Example: [Cat, Mag] = ultrasat.extended2List('SizeRA',15,'Oversampling',5,'Mag',18);
    arguments
        Args.RA0          = 217;     % [deg]
        Args.Dec0         = 55;      % [deg]
        Args.SizeRA       = 10;      % [arcsec] 
        Args.SizeDec      = 15;      % [arcsec]
        Args.Mag          = 20;      % stellar magnitude of the imput object
        
        Args.Oversampling = 10;      % spatial pixel quantization scale (reasonable to be of the same scale as the PSF?)       
        
        Args.ProfileType   = 'sersic'  % 'gaussian', 'sersic', 'flat', 'matrix'
        Args.ProfilePar    = [10 2 1]; % here sigma=10 corresponds to 1 pixel because Oversampling = 10
        Args.ProfileMatrix = [];       % a full 2D profile (need to be of Nx x Ny or rescale?)
        
%         Args.SpecModel    % for the time being we presume the same spectrum is emitted from all the object's parts
%         Args.SpecProfileX
%         Args.SpecProfileY 
%         Args.Wave         = 2000:11000;

        Args.PixSizeDeg   = 0.001512; % [deg] ULTRASAT
%         Args.CRPIX        = [100 100];        
        
        Args.Write2File   = false;
        Args.OutputFilename = 'extended_object.txt'
    end          
        
    Grain = Args.PixSizeDeg*3600/Args.Oversampling; % [arcsec]
    Nx = ceil(Args.SizeRA /Grain);
    Ny = ceil(Args.SizeDec/Grain);
    Nsrc = Nx * Ny;
    Cat  = zeros(Nsrc,2); % RA, Dec 
    Mag  = zeros(Nsrc,1); % Mag 
    
    switch lower(Args.ProfileType)
        case 'sersic'
            Prof = imUtil.kernel2.sersic(Args.ProfilePar,[Ny Nx]);
        case 'gaussian'
            Prof = imUtil.kernel2.gauss(Args.ProfilePar,[Ny Nx]);
        case 'flat'
            Prof = ones(Nx,Ny);
        case 'matrix'
            Prof = Args.ProfileMatrix;
        otherwise
            error 'unsupported profile type'
    end

    % NB: this is correct only for small values of SizeRA and SizeDec!
    RA1     = Args.RA0  - (Args.SizeRA /3600)/2;
    Dec1    = Args.Dec0 - (Args.SizeDec/3600)/2;
    DeltaRA = (Args.SizeRA /3600)/Nx;
    DeltaDec= (Args.SizeDec/3600)/Ny;
    
%     SimWCS = AstroWCS();
%     SimWCS.ProjType  = 'TAN';
%     SimWCS.ProjClass = 'ZENITHAL';
%     SimWCS.CooName   = {'RA'  'DEC'};
%     SimWCS.CTYPE     = {'RA---TAN','DEC---TAN'};
%     SimWCS.CUNIT     = {'deg', 'deg'};
%     SimWCS.CD(1,1)   = Args.PixSizeDeg;
%     SimWCS.CD(2,2)   = Args.PixSizeDeg;
%     SimWCS.CRVAL(1)  = Args.RA0;
%     SimWCS.CRVAL(2)  = Args.Dec0;
%     SimWCS.CRPIX(1)  = Args.CRPIX(1);
%     SimWCS.CRPIX(2)  = Args.CRPIX(2);
%     SimWCS.populate_projMeta; 

    % the Prof[ile] must be normalized to 1 
    Prof = Prof ./ sum(Prof,'all');
    
    for Ix = 1:Nx        
        for Iy = 1:Ny
            Isrc = (Ix-1)*Ny + Iy;
            DeltaF = Prof(Ix,Iy); 
            DeltaM = -2.5 * log10(DeltaF); 
            Mag(Isrc)   = Args.Mag + DeltaM;        % Mag        
            Cat(Isrc,1) = RA1  + (Ix-1) * DeltaRA;  % RA
            Cat(Isrc,2) = Dec1 + (Iy-1) * DeltaDec; % Dec
        end        
    end
    
    % if needed, make a text file with the input catalog:
    if Args.Write2File
        fileID = fopen(Args.OutputFilename,'w');
        fprintf(fileID,'# RA Dec Mag\n');
        fprintf(fileID,'%.2f %.4f %.4f\n',Cat',Mag');
        fclose(fileID);
    end
end
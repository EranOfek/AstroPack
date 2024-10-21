function [SimAI, InjectedCat] = simulateSkyImage(Args)
        % simulate a sky image from source PSF and source magnitude distribution in the field 
        % Input:  - 
        %         * ...,key,val,... 
        %         'Size' - image size (overriden by an expicit Cat argument!)
        %         'Cat'  - input catalog [X Y] matrix
        %         'Mag'  - input magnitudes (1 value or individual values)  
        %         'Nsrc' - number of objects ([] be def.); if non-empty and numel(Args.Mag)=1, Mag is spawned according to this number
        %         'PSF'  - input PSF (can be a 2D matrix or a stack of 2D stamps with source number in the 3rd dimension)
        %         'MagZP'- photometric zero point
        %         'AddBack' - (logical) whether to add backgorund to the source image
        %         'Back' - image background (in cts)
        %         'AddNoise' - (logical) whether to add noise to the source image
        %         'PixSizeDeg'    - WCS parameters: image pixel size [deg]
        %         'CRVAL'         - WCS parameters: reference coordinates [RA Dec]
        %         'CRPIX'         - WCS parameters: reference pixels [X Y]
        %         'WriteFiles'    - logical (write the output to FITS image and ds9 region files)
        %         'OutImageName'  - output FITS image file name
        %         'OutRegionName' - output ds9 region file name
        %         'OutArchName'   - output .mat archive file name
        % Output: - an AstroImage containing the simulated image 
        %         - the injected source catalog
        %         - (optional) output disk files: FITS image, ds9 region, .mat object 
        % Author: A.M. Krassilchtchikov (Sep 2024)
        % Example: [SimAI, SimCat] = imProc.art.simulateSkyImage('WriteFiles',true);
        % 
        arguments
            Args.Size       = [1700 1700]; % image size [the default size is of a LAST subimage] 
            Args.Cat        = [];          % input catalog (source positions) 
            Args.Mag        = [];          % input magnitudes (1 value or individual values)  
            Args.Nsrc       = [];          % number of objects; if non-empty and numel(Args.Mag)=1, Mag is spawned according to this number
            Args.PSF        = '~/matlab/data/TestImages/unitTest/LAST_PSF.txt';% input PSF: either a file name or stamp
            Args.MagZP      = 25;          % photometric zero point            
            Args.AddBack  logical = true;  % whether to add backgorund to the source image
            Args.Back       = 220;         % [cts] [this default value is for a moderately dense field of LAST]
            Args.DensityFactor = 1;        % source density scaling: 1 corresponds to a moderately dense field of LAST
            Args.AddNoise logical = true;  % whether to add noise to the source image
            Args.PixSizeDeg = 3.4722e-4;   % LAST pixel size [deg]
            Args.CRVAL      = [215 53];    % WCS CRVAL
            Args.CRPIX      = [1 1];       % WCS CRPIX
            Args.WriteFiles = false;       % write the FITS image and a source catalog region file
            Args.OutImageName  = '~/LAST_sim_image.fits'; % image file name
            Args.OutRegionName = '~/LAST_sim.reg';        % region file name            
            Args.OutArchName   = '~/LAST_sim.mat';        % full archive file name
        end
        % make an empty AI
        SimAI = AstroImage;
        
        % add WCS
        SimWCS = AstroWCS();
        SimWCS.ProjType  = 'TAN';
        SimWCS.ProjClass = 'ZENITHAL';
        SimWCS.CooName   = {'RA'  'DEC'};
        SimWCS.CTYPE     = {'RA---TAN','DEC---TAN'};
        SimWCS.CUNIT     = {'deg', 'deg'};
        SimWCS.CD(1,1)   = Args.PixSizeDeg;
        SimWCS.CD(2,2)   = Args.PixSizeDeg;
        SimWCS.CRVAL     = Args.CRVAL;        
        SimWCS.CRPIX     = Args.CRPIX;        
        SimWCS.populate_projMeta;        
        AH = SimWCS.wcs2header;  % make a header from the WCS
        SimAI.HeaderData.Data = AH.Data; % add the WC data to the AI header       
        SimAI.WCS        = SimWCS;                
        
        if numel(Args.Size) > 1
            Nx = Args.Size(1);
            Ny = Args.Size(2);
        else
            Nx = Args.Size; 
            Ny = Nx;
        end
        
        if isempty(Args.Mag)            
            % source distribution by optical magnitude (taken from LAST) 
            MinMag  = 11; MaxMag = 21; DeltaMag = 0.01; % (MaxMag = 21, 19 if the laptop memory is insufficient)
            Mags    = MinMag:DeltaMag:MaxMag;
            Nstars  = round(DeltaMag.*10.^(0.35.*Mags-2.1)); % 0.33 - 1.7 % this empiric dependence has been measured from a LAST subimage of a dense field
            
            Nstars  = Args.DensityFactor .* Nstars;
            
            Nsrc = 0;
            for Imag = 1:numel(Mags)
                for Istar = 1:Nstars(Imag)
                    Nsrc       = Nsrc + 1;
                    Mag(Nsrc)  = Mags(Imag);
                end
            end
        else % read the source magnitudes from the input parameter  
            if isempty(Args.Nsrc)
                Nsrc = numel(Args.Mag);
            else
                Nsrc = Args.Nsrc;
                if numel(Args.Mag) < 2
                    Mag = repmat(Args.Mag,1,Nsrc);
                end
            end
        end
        
        Flux = 10.^(0.4.*(Args.MagZP-Mag));
        
        fprintf('%d objects in the FOV\n', Nsrc);
        
        % simulated source positions
        if isempty(Args.Cat)
            Cat = [Nx.*rand(Nsrc,1), Ny.*rand(Nsrc,1)]; 
        else
            Cat = Args.Cat;
        end
        
        % read an empirical LAST PSF 
        if ischar(Args.PSF)
            PSF = readmatrix(tools.os.relPath2absPath(Args.PSF));
        else
            PSF = Args.PSF; 
        end
                
        % add background with some spatial variations
        Back = Args.Back .* (1 + 0.1*rand(Nx,Ny));  
        
        % need to set up an empty image
        SimAI.Image = repmat(0,Nx,Ny);
        SimAI.Mask  = repmat(uint32(0),Nx,Ny);
        SimAI.setKeyVal('OBJECT','Simulated');
        SimAI.PSF   = PSF;
         
        [SimAI, InjectedCat] = imProc.art.injectSources(SimAI, Cat, PSF, Flux', Mag',... 
                                                        'UpdateCat', false, ... 
                                                        'MagZP',Args.MagZP, ... 
                                                        'PositivePSF', true, ... 
                                                        'AddBackground',Args.AddBack,...
                                                        'Back', Back, ...                                                         
                                                        'AddNoise',Args.AddNoise, ...
                                                        'NoiseModel', 'normal'); 
                                                             
         % write disk files if requested 
         if Args.WriteFiles             
             DS9_new.regionWrite([Cat(:,1) Cat(:,2)],'FileName',Args.OutRegionName,'Color','cyan','Marker','s','Size',1,'Width',4,...
                 'Precision','%.2f','PrintIndividualProp',0);
             
             FITS.write(SimAI.Image', Args.OutImageName,'Header',SimAI.HeaderData.Data,...
                    'DataType','single', 'Append',false,'OverWrite',true,'WriteTime',true);  
                
             save(Args.OutArchName,'SimAI','InjectedCat');                                 
         end                
end
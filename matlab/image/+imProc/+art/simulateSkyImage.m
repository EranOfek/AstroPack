function [SimAI, InjectedCat] = simulateSkyImage(Args)
        % simulate a sky image from source PSF and source magnitude distribution in the field 
        % Input:  - 
        %         * ...,key,val,... 
        %         'Size' - image size (overriden by an expicit Cat argument!)
        %         'Cat'  - input catalog [X Y] matrix
        %         'SkyCat'- (logical) whether the input catalog is in Sky [RA, Dec] (true) or Pixel [pix, pix] (false) coordinates
        %         'Mag'  - input magnitudes (1 value or individual values)  
        %         'Nsrc' - number of objects ([] be def.); if non-empty and numel(Args.Mag)=1, Mag is spawned according to this number
        %         'PSF'  - input PSF (can be a 2D matrix or a stack of 2D stamps with source number in the 3rd dimension)
        %         'MagZP'- photometric zero point
        %         'MaxMag' - lower limit of injected source distribution
        %         'AddBack' - (logical) whether to add backgorund to the source image
        %         'Back' - image background (in cts)
        %         'DensityFactor' - linearly scaled source density: 1 corresponds to a moderately dense field of LAST
        %         'AddNoise' - (logical) whether to add noise to the source image
        %         'PixSizeDeg'    - WCS parameters: image pixel size [deg]
        %         'CRVAL'         - WCS parameters: reference coordinates [RA Dec]
        %         'CRPIX'         - WCS parameters: reference pixels [X Y]
        %         'WriteFITS'     - logical (write the output to FITS image)
        %         'WriteReg'      - logical (write the output ds9 region file)
        %         'WriteMat'      - logical (write the output .mat archive with all the results)
        %         'OutImageName'  - output FITS image file name
        %         'OutRegionName' - output ds9 region file name
        %         'OutArchName'   - output .mat archive file name
        % Output: - an AstroImage containing the simulated image 
        %         - the injected source catalog
        %         - (optional) output disk files: FITS image, ds9 region, .mat object 
        % Author: A.M. Krassilchtchikov (Sep 2024)
        % Example: [SimAI, SimCat] = imProc.art.simulateSkyImage('WriteFITS',true);
        % 
        arguments
            Args.Size       = [1700 1700]; % image size [the default size is of a LAST subimage] 
            Args.Cat        = [];          % input catalog (source positions) 
            Args.SkyCat     = true;        % the input catalog is in Sky [RA, Dec] (true) or Pixel [pix, pix] (false) coordinates
            Args.Mag        = [];          % input magnitudes (1 value or individual values)  
            Args.Nsrc       = [];          % number of objects; if non-empty and numel(Args.Mag)=1, Mag is spawned according to this number
            Args.PSF        = '~/matlab/data/TestImages/unitTest/LAST_PSF.txt';% input PSF: either a file name or a stamp
            Args.MagZP      = 25;          % photometric zero point            
            Args.MaxMag     = 21;          % lower limit of source distribution (influences the number of objects and effective background)
            Args.AddBack  logical = true;  % whether to add backgorund to the source image
            Args.Back       = 220;         % [cts] [this default value is for a moderately dense field of LAST]
            Args.DensityFactor = 1;        % source density scaling: 1 corresponds to a moderately dense field of LAST
            Args.AddNoise logical = true;  % whether to add noise to the source image
            Args.PixSizeDeg = 3.4722e-4;   % LAST pixel size [deg]
            Args.CRVAL      = [215 53];    % WCS CRVAL
            Args.CRPIX      = [1 1];       % WCS CRPIX
            Args.WriteFITS  = false;       % write the FITS image 
            Args.WriteReg   = false;       % write the source catalog region file
            Args.WriteMat   = false;       % write a full .mat archive
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
            MinMag  = 11; MaxMag = Args.MaxMag; DeltaMag = 0.01; % (MaxMag = 21, 19 if the laptop memory is insufficient)
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
                if numel(Args.Mag) < 2
                    Mag = repmat(Args.Mag,1,Nsrc);
                else
                    Mag = Args.Mag;
                end
            else
                Nsrc = Args.Nsrc;
                if numel(Args.Mag) < 2
                    Mag = repmat(Args.Mag,1,Nsrc);
                end
            end
        end
        
        Flux = 10.^(0.4.*(Args.MagZP-Mag));
        
        % simulated source positions
        if isempty(Args.Cat)
            Cat = [Nx.*rand(Nsrc,1), Ny.*rand(Nsrc,1)]; 
        else
            if Args.SkyCat
                [Cat(:,1), Cat(:,2)] = SimWCS.sky2xy(Args.Cat);
            else
                Cat = Args.Cat;
            end
        end
        % calculate the number of sources falling into the FOV:
        NinFOV = sum( (Cat(:,1) > 0) & (Cat(:,1) < Nx+1) & (Cat(:,2) > 0) & (Cat(:,2) < Ny+1) );
        fprintf('%d objects in the FOV\n', NinFOV);
        
                % write disk files if requested 
                 if Args.WriteReg            
                     DS9_new.regionWrite([Cat(:,1) Cat(:,2)],'FileName',Args.OutRegionName,...
                         'Color','cyan','Marker','s','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);
                 end        
        % read an empirical PSF 
        if ischar(Args.PSF)
            PSF = readmatrix(tools.os.relPath2absPath(Args.PSF));
        else
            PSF = Args.PSF; 
        end
%         PSF = imUtil.kernel2.gauss(2,[25 25]); % for tests only
                
        % add background with some spatial variations
        Back = Args.Back .* (1 + 0.1*rand(Nx,Ny));  
        
        % need to set up an empty image
        SimAI.Image = repmat(0,Nx,Ny);
        SimAI.Mask  = repmat(uint32(0),Nx,Ny);
        SimAI.setKeyVal('OBJECT','Simulated');
        SimAI.setKeyVal('JD',celestial.time.date2jd);
        SimAI.PSF   = PSF;
         
        [SimAI, InjectedCat] = imProc.art.injectSources(SimAI, Cat, PSF, Flux', Mag',... 
                                                        'CreateNewObj',true, ...
                                                        'UpdateCat', false, ... 
                                                        'MagZP',Args.MagZP, ... 
                                                        'PositivePSF', true, ... 
                                                        'AddBackground',Args.AddBack,...
                                                        'Back', Back, ...                                                         
                                                        'AddNoise',Args.AddNoise, ...
                                                        'NoiseModel', 'normal'); 
         % add sky coordinates to the InjectedCat
         if ~isempty(Args.Cat) && Args.SkyCat % just use the original input coordinates
             SrcRA  = Args.Cat(:,1);
             SrcDec = Args.Cat(:,2);
         else
             [SrcRA, SrcDec] = SimWCS.xy2sky(InjectedCat.Table.X1, InjectedCat.Table.Y1);
         end
         InjectedCat = insertCol(InjectedCat, [SrcRA, SrcDec], Inf, {'RA', 'Dec'}, {'deg', 'deg'});
                                                             
         % write disk files if requested            
         if Args.WriteFITS
             FITS.write(SimAI.Image', Args.OutImageName,'Header',SimAI.HeaderData.Data,...
                 'DataType','single', 'Append',false,'OverWrite',true,'WriteTime',true);
         end
         if Args.WriteMat
             save(Args.OutArchName,'SimAI','InjectedCat');
         end
end
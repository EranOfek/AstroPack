function [SimAI, InjectedCat] = simulateImage(Args)
        % simulate a sky image from source PSF and source magnitude distribution in the field 
        % Input:  - 
        %         * ...,key,val,... 
        %         'Size' - image size (overriden by an expicit Cat argument!)
        %         'Cat'  - input catalog [X Y] matrix
        %         'Mag'  - input magnitudes (1 value or individual values)  
        %         'Nsrc' - number of objects ([] be def.); if non-empty and numel(Args.Mag)=1, Mag is spawned according to this number
        %         'PSF'  - input PSF (can be a 2D matrix or a stack of 2D stamps with source number in the 3rd dimension)
        %         'MagZP'- photometric zero point
        %         'Back' - image background 
        %         'WriteFiles' - logical (write output also to files)
        % Output: - an AstroImage containing the simulated image 
        %         - the injected source catalog
        % Author: A.M. Krassilchtchikov (Sep 2024)
        % Example: [SimAI, SimCat] = imProc.art.simulateImage('WriteFiles',true);
        % 
        arguments
            Args.Size       = [1700 1700]; % image size [the default size is of a LAST subimage] 
            Args.Cat        = [];          % input catalog (source positions) 
            Args.Mag        = [];          % input magnitudes (1 value or individual values)  
            Args.Nsrc       = [];          % number of objects; if non-empty and numel(Args.Mag)=1, Mag is spawned according to this number
            Args.PSF        = 'LAST_PSF.txt';% input PSF: either a file name or stamp
            Args.MagZP      = 25;          % photometric zero point
            Args.Back       = 220;         % [cts] [the default number is for a dense field of LAST]
            Args.WriteFiles = false;       % write the FITS image and a source catalog region file
            Args.OutRegionName = 'LAST_sim.reg'; % region file name
            Args.OutImageName  = 'LAST_sim_image.fits'; % image file name
            Args.OutArchName   = 'LAST_sim.mat'; % full archive file name
        end
        %
        SimAI = AstroImage;
        
        if numel(Args.Size) > 1
            Nx = Args.Size(1);
            Ny = Args.Size(2);
        else
            Nx = Args.Size; 
            Ny = Nx;
        end
        
        if isempty(Args.Mag)            
            % source distribution by optical magnitude (taken from LAST) 
            MinMag  = 11; MaxMag = 21; DeltaMag = 0.01;
            Mags    = MinMag:DeltaMag:MaxMag;
            Nstars  = round(DeltaMag.*10.^(0.35.*Mags-2.1)); % 0.33 - 1.7 % this empiric dependence has been measured from a LAST subimage of a dense field
            
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
            PSF = readmatrix(Args.PSF);
        else
            PSF = Args.PSF; 
        end
                
        % add background with some spatial variations
        Back = Args.Back .* (1 + 0.1*rand(Nx,Ny));  
        
        % need to set up an empty image
        SimAI.Image = repmat(0,Nx,Ny);
        SimAI.setKeyVal('OBJECT','Simulated');
        
        [SimAI, InjectedCat] = imProc.art.injectSources(SimAI, Cat, PSF, Flux', Mag',... 
                                                        'UpdateCat', false, ... 
                                                        'MagZP',Args.MagZP, ... 
                                                        'PositivePSF', true, ... 
                                                        'Back', Back, ... 
                                                        'AddBackground',true,... 
                                                        'NoiseModel', 'normal'); 
         % write disk files if requested 
         if Args.WriteFiles             
             DS9_new.regionWrite([Cat(:,1) Cat(:,2)],'FileName',Args.OutRegionName,'Color','cyan','Marker','s','Size',1,'Width',4,...
                 'Precision','%.2f','PrintIndividualProp',0);
             
             FITS.write(SimAI.Image, Args.OutImageName,'Header',SimAI.HeaderData.Data,...
                    'DataType','single', 'Append',false,'OverWrite',true,'WriteTime',true);  
                
             save(Args.OutArchName,'SimAI','InjectedCat');                                
         end                
end
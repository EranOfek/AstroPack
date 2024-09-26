function [SimAI, InjectedCat] = simulateImage(Args)
        % simulate a sky image from source PSF and magnitudes distribution  
        % Input: - 
        % Output: - an AstroImage containing the simulated image 
        %         - the injected source catalog
        % Author: A.M. Krassilchtchikov (Sep 2024)
        % Example: SimAI = imProc.art.simulateImage;
        % 
        arguments
            Args.Size       = [1700 1700];% image size [the default size is of a LAST subimage] 
            Args.Cat        = [];          % input catalog (source positions) 
            Args.PSF        = [];          % input PSF stamp
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
        
        % source distribution by magnitude 
        MinMag  = 11; MaxMag = 20; DeltaMag = 0.01;
        Mags    = MinMag:DeltaMag:MaxMag;        
        Nstars  = round(DeltaMag.*10.^(0.35.*Mags-2.1)); % 0.33 - 1.7 % this empiric dependence has been measured from a LAST subimage of a dense field
        
        Nsrc = 0;
        for Imag = 1:numel(Mags)
            for Istar = 1:Nstars(Imag)
                Nsrc       = Nsrc + 1;
                Mag(Nsrc)  = Mags(Imag);
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
        PSF = readmatrix('LAST_PSF.txt');
                
        % add background with some spatial variations
        Back = Args.Back .* (1 + 0.1*rand(Nx,Ny));
        
        [SimAI, InjectedCat] = imProc.art.injectSources(SimAI, Cat, PSF, Flux', ...
                                                        'UpdateCat', false, ...
                                                        'MagZP',Args.MagZP, ...
                                                        'PositivePSF', true, ...
                                                        'Back', Back, ...
                                                        'NoiseModel', 'normal');
         % write disk files if requested 
         if Args.WriteFiles             
             DS9_new.regionWrite([Cat(:,1) Cat(:,2)],'FileName',Args.OutRegionName,'Color','cyan','Marker','s','Size',1,'Width',4,...
                 'Precision','%.2f','PrintIndividualProp',0);
             
             FITS.write(SimAI.Image, Args.OutImageName,... % 'Header',usimImage.HeaderData.Data,...
                    'DataType','single', 'Append',false,'OverWrite',true,'WriteTime',true);  
                
             save(Args.OutArchName,'SimAI','InjectedCat');                                
         end                
end
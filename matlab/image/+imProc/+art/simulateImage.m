function SimAI = simulateImage(Args)
        % simulate a sky image from source PSF and magnitudes distribution  
        % Input: - 
        % Output: - an AstroImage containing the simulated image and catalog
        % Author: A.M. Krassilchtchikov (Sep 2024)
        % Example: SimAI = imProc.art.simulateImage;
        % 
        arguments
            Args.Size       = [1700 1700];% image size [the default size is of a LAST subimage] 
            Args.Cat        = [];          % input catalog (source positions) 
            Args.PSF        = [];          % input PSF stamp
            Args.MagZP      = 25;          % photometric zero point
            Args.Back       = 220;         % [cts] [the default number is for a dense field of LAST]
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
                                                        'MagZP',Args.MagZP, ...
                                                        'PositivePSF', true, ...
                                                        'Back', Back, ...
                                                        'NoiseModel', 'normal');
        
%         SrcTab = table(X1Y1(:,1),X1Y1(:,2),Mag','VariableNames',{'X','Y','Mag'});
%         writetable(SrcTab,'LAST_sim_catalog.txt');
%         OutRegName  = 'LAST_sim.reg';
%         DS9_new.regionWrite([X1Y1(:,1) X1Y1(:,2)],'FileName',OutRegName,'Color','cyan','Marker','s','Size',1,'Width',4,...
%                                  'Precision','%.2f','PrintIndividualProp',0);    
        
%         SimAI.CatData = AstroCatalog(SrcTab);               
        
%         % create a list of shifted and resampled fluxed PSF stamps
%         [CubePSF, XY] = imUtil.art.createSourceCube(PSF, X1Y1, Flux, 'Recenter', true, ...
%             'RecenterMethod','fft','Oversample', [], 'PositivePSF', true);
        
%         % create an empty image
%         Image0 = repmat(0,Nx,Ny);
%         
%         % fill the image with sources
%         ImageSrc = imUtil.art.addSources(Image0,CubePSF,XY,'Oversample',[],'Subtract',false);
%         if sum(ImageSrc < 0) > 0
%             warning('Negative pixels');
%         end
%         
% %         % add background with some spatial variations
%         Back = Back0 .* (1 + 0.1*rand(Nx,Ny));
%         ImageSrcBack = imUtil.art.addBackground(ImageSrc, Back, 'Subtract', false);
%         
%         % add noise
%         Image = imUtil.art.addNoise(ImageSrcBack,'normal');
%         
%         SimAI.Image = Image;
        
%         OutFITSName = 'LAST_sim_image.fits';
%         FITS.write(Image, OutFITSName,... % 'Header',usimImage.HeaderData.Data,...
%                     'DataType','single', 'Append',false,'OverWrite',true,'WriteTime',true);                      
%                 
%         save('LAST_sim.mat','SimAI');
        
end
function simImage = simulateKeplerField(Args)
    % simulate an ULTRASAT image from of the Kepler field from the input star catalog
    % 
    arguments
        Args.Tile    = 'B';    % the tile name
        Args.RA0     = 291;    % the aimpoint
        Args.Dec0    =  44.5;  % the aimpoint
        Args.ExpNum  =   1;    % the number of exposures
        Args.PlaneRotation = 0;
        Args.OutDir  = '.';
        Args.OutName = 'SimKepler'
        Args.Ebv     =   0; % the updated table contains per-source Ebv, so we need this only for tests
        Args.Catalog = 'Kepler_ULTRASAT_all.tbl';
        Args.Dir     = '/home/sasha/KeplerField';
        Args.SNR     = false; % calculate source SNRs with telescope.sn.snr (the existing one is too slow)
        Args.SpecType = 'Pickles'; % 'BB' or 'Pickels'
    end
    
    % TODO:
    %     % 2. based on the grid, make a library of spectrum-integrated ULTRASAT PSFs: 
    % 25 radial points x ~ 108-131 uk Pickles spectra (or less?) [+ a number of
    % BBs?] ~ 3000 PSFs at 1/5 pixel resolution (stamp 108x108 pix ~ 10^4 * 4
    % byte (single precision) ~ 44 kb x 3000 PSFs ~ 120 Mb ?)
    % and implement the possibility to use the library into the simulator
    % 3. according to the request of Yossi add an SNR column to the table so
    % that SNR = 0.8 * count / noise, where noise = avg(noise/pix)*
    % effective number of source pixels in a source PSF (pi*containment*2)
    
    cd(Args.Dir);
    SrcTab  = readtable(Args.Catalog,'FileType','text');
    
    switch Args.Tile 
        case 'A'
            ra1 = 279; ra2 = 291;
            dec1 = 44; dec2 = 52;
        case 'B'
            ra1 = 291; ra2 = 303;
            dec1 = 44; dec2 = 52;
        case 'C'
            ra1 = 291; ra2 = 303;
            dec1 = 36.5; dec2 = 44.5;
        case 'D' 
            ra1 = 280; ra2 = 291;
            dec1 = 36.5; dec2 = 44.5;
        otherwise
            error ('Tile name not correct');
    end
    
    Tab = SrcTab(SrcTab.x_ra > ra1 & SrcTab.x_ra < ra2 & SrcTab.dec > dec1 & SrcTab.dec < dec2,:);

    %%% TEST ONLY!!
%     sortedTable = sortrows(Tab, 'Vmag');
%     Tab = sortedTable(1:10000,:);
    %%% END TEST 
    
    Cat  = [Tab.x_ra Tab.dec];
    Mag0 =  Tab.Vmag;
    Ebv  = Tab.E_B_V_; % individual values 
%     Ebv  = Args.Ebv; % one value for the whole field %% TEST ONLY
    IndNaN = isnan(Ebv);
    Ebv(IndNaN) = 0.1; % change the non-existing Ebv for the mean value of the field (0.1)
    
    % deredden the V magnidues (the simulator deals with dereddened values!)
    Filt = AstFilter.get('Johnson','V');
    deltaMag = astro.spec.extinction(Ebv,Filt.pivot_wl/1e4);
    Mag = Mag0 - deltaMag;    
    % figure(1);hold off; histogram(Mag); hold on; histogram(Mag0)
    
    % build the BB spectra or use Teff and log(g) to employ Pickels' stellar spectra
    switch Args.SpecType         
        case 'BB'
            % make a grid of BB spectra
            Wave = 2000:11000;   % the wavelength band in A
            Temp = 2500:250:12000; % a temperature grid
            S = repmat(AstroSpec,1,numel(Temp));
            for i = 1:numel(Temp)
                S(i) = AstroSpec.blackBody(Wave',Temp(i));
            end
            NSrc = size(Tab,1);
            Spec = repmat(AstroSpec,1,NSrc);
            for ISrc = 1:NSrc
                diff = abs(Tab.Teff(ISrc)-Temp);  % find the nearest neighbour in the spectrum grid
                [~, ind] = min(diff);
                Spec(ISrc) = S(ind);
                % Spec(ISrc)  = AstroSpec.blackBody(Wave',Tab.Teff(ISrc)); % DON't use: this is way to slow and voluminous!
            end            
        case 'Pickles'            
            Spec = [Tab.Teff Tab.logg]; % parameters of the Pickles' spectra            
        otherwise            
            error('Unknown spectral type');
    end
    %%% calculate crude source SNRs from the previous runs and augment the source table:
%     if (Args.SNR)        
%         InCat = readtable('SimKepler_tileB_InCat.txt','FileType','text');
%         CPS = InCat.Var3 * 300 * Args.ExpNum; 
%         io.files.load1('SimKepler_tileB_RadSrc.mat'); % 'RadSrc','InFOV'
%         Tab = addvars(Tab, InFOV, 'NewVariableNames', {'InFOV'});
%         TabFOV = Tab(InFOV > 0,:); % cut the sources which are out of the tile's FOV 
%         io.files.load1('PSFContain50Rad.mat'); % 'Rad50','logT','logg','Rad'
%         NSrc = size(TabFOV); % TEST ONLY
%         NoisePerPix = 75;
%         for Isrc = 1:1:NSrc
%             logTeff = log10(TabFOV(Isrc,:).Teff);
%             logG    = TabFOV(Isrc,:).logg;
%             PSFRad = interpn(logT, logg, Rad, Rad50, logTeff, logG, RadSrc(Isrc));
%             SNR(Isrc) = 0.8 * CPS(Isrc) / sqrt(pi * PSFRad^2 * NoisePerPix);                
%         end
%         TabFOV = addvars(TabFOV, SNR', 'NewVariableNames', {'SNR'});
%         writetable(TabFOV, 'TileB50.tbl', 'Delimiter', '\t','FileType','text'); 
%     end
    
    %%%% run the simulation 
    simImage = ultrasat.usim('Cat', Cat, 'Mag', Mag, 'FiltFam','Johnson', 'Filt','V',...
        'SpecType',Args.SpecType,'Spec', Spec, 'Exposure', [Args.ExpNum 300], 'Ebv', Ebv,...
        'OutDir', Args.OutDir,'SkyCat', 1, 'PlaneRotation', Args.PlaneRotation,...
        'RA0', Args.RA0, 'Dec0', Args.Dec0, 'OutName', Args.OutName, 'Tile', Args.Tile);

end
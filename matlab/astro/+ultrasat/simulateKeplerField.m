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
        Args.Ebv     =   0; % the actual E(B-V) in the Kepler field is about 0.44 ?
        Args.Catalog = 'Kepler_ULTRASAT_all.tbl';
        Args.Dir     = '/home/sasha/KeplerField';
        Args.SNR     = false; % calculate source SNRs with telescope.sn.snr 
        Args.SpecType = 'Pickles'; % 'BB' or 'Pickels'
    end
    
    % TODO:
    % 1. Use Pickles spectra instead of blackbodies, pass to usim Spec = [Tab.teff Tab.logg]
    % 2. based on the grid, make a library of spectrum-integrated ULTRASAT PSFs: 
    % 25 radial points x ~ 108-131 uk Pickles spectra (or less?) [+ a number of
    % BBs?] ~ 3000 PSFs at 1/5 pixel resolution (stamp 108x108 pix ~ 10^4 * 4
    % byte (single precision) ~ 44 kb x 3000 PSFs ~ 120 Mb ?)
    % and implement the possibility to use the library into the simulator
    % 3. according to the request of Yossi add an SNR column to the table so
    % that SNR = 0.8 * count / noise, where noise = avg(noise/pix)*
    % effective number of source pixels in a source PSF (pseudoFWHM?)
    
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

    %%% test: look at brightest objects only:
    %     sortedTable = sortrows(Tab, 'Vmag');
    %     Tab = sortedTable(1:2000,:);
    
%     Tab = Tab(1:10000,:); % TEST ONLY!!!
    
    Cat  = [Tab.x_ra Tab.dec];
    Mag0 =  Tab.Vmag;
%     Ebv  = Args.Ebv; % one value for the whole field
    Ebv  = Tab.E_B_V_; % individual values 
    IndNaN = isnan(Ebv);
    Ebv(IndNaN) = 0.1; % change the non-existing Ebv for the mean value of the field (0.1)
    
    % account for extinction (the simulator deals with dereddened values!)
    Filt = AstFilter.get('Johnson','V');
    deltaMag = astro.spec.extinction(Args.Ebv,Filt.pivot_wl/1e4);
    Mag = Mag0 - deltaMag;
    
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
    %%%%%%%%%%%%%%%%%%%% calculate ULTRASAT magnitudes and SNR for the catalog objects from the table:
    if (Args.SNR)        
        tic       % very slow! takes  ~ 90 sec for 1000 objects => ~ 100 hrs for 4 x 10(6) obj !
        UP_db = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat');
        io.files.load1(UP_db,'UP');
        
        % parameters for telescope.sn.snr:
        Exp = 300; % sec
        NIm = 3;   % number of exposures
        T   = Tab.Teff;
        SN  = 5;  % S/N for detection
        PSFeff = 0.8;
        
        % the extinction curve:
        ExtMag     = astro.spec.extinction(Args.Ebv,(Wave./1e4)');
        Extinction = 10.^(-0.4.*ExtMag);
        
        % set the radial position (in reality it depends on the particular aim point and rotation of the s/c):
        Irad = 1; 
        
        NSrc = 10; % TEST ONLY!
        
        % rescale the spectra, account for the absorption:
        MagU = zeros(NSrc,1);
        for Isrc = 1:1:NSrc
            MagSc = astro.spec.synthetic_phot([Spec(Isrc).Wave Spec(Isrc).Flux],'Johnson','V','AB');
            Factor = 10.^(-0.4.*(MagSc - Tab.Vmag(Isrc)));
            SpecObs = Spec(Isrc).Flux .* Extinction ./ Factor;
            MagU(Isrc) = astro.spec.synthetic_phot([Wave' SpecObs],UP.U_AstFilt(Irad),'R1','AB');
            SNR(Isrc) = telescope.sn.snr('ExpTime',Exp,'Nim',NIm,'TargetSpec',T(Isrc),'PSFeff',PSFeff,'Mag',MagU(Isrc),'CalibFilterFamily',UP.U_AstFilt(Irad),...
                'CalibFilter','','Wave',Wave','SN',SN,'FWHM',UP.EffPSF(Irad));
        end
        
%         fprintf('%s%10.2f\n','SNR: ',SNR.SNR);
        toc
    end
    
    %%%% run the simulation 
    simImage = ultrasat.usim('Cat', Cat, 'Mag', Mag, 'FiltFam','Johnson', 'Filt','V',...
        'SpecType',Args.SpecType,'Spec', Spec, 'Exposure', [Args.ExpNum 300], 'Ebv', Ebv,...
        'OutDir', Args.OutDir,'SkyCat', 1, 'PlaneRotation', Args.PlaneRotation,...
        'RA0', Args.RA0, 'Dec0', Args.Dec0, 'OutName', Args.OutName, 'Tile', Args.Tile);

end
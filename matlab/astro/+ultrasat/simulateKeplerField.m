function simImage = simulateKeplerField(Args)
    % simulate an ULTRASAT image from of the Kepler field from the input star catalog
    % 
    arguments
        Args.Tile    = 'B';    % the tile name
        Args.RA0     = 291;    % the aimpoint (Kepler field -- 291, NEP -- 270, SEP -- 90)
        Args.Dec0    =  44.5;  % the aimpoint (Kepler field -- 44.5, NEP -- +66.560708, SEP -- -66.560708)
        Args.ExpNum  =   1;    % the number of exposures
        Args.PlaneRotation = 0;
        Args.OutDir  = '.';
        Args.OutName = 'SimKepler'
        Args.Ebv     =   0; % the updated table contains per-source Ebv, so we need this only for tests
        Args.Catalog = 'Kepler_ULTRASAT_all.tbl'; % Kepler field: 'Kepler_ULTRASAT_all.tbl'
        Args.Dir     = '/home/sasha/KeplerField';
        Args.SpecType = 'Pickles'; % 'BB' or 'Pickels'
    end
    
    cd(Args.Dir);
    SrcTab  = readtable(Args.Catalog,'FileType','text');
    
    switch Args.Tile 
        case 'A'
            ra1 = 279; ra2 = 291; dec1 = 44; dec2 = 52; % Kepler field
%             ra1 = 71; ra2 = 90; dec1 = -68; dec2 = -58; % SEP
%             ra1 = 244; ra2 = 271; dec1 = 65; dec2 = 74; % NEP
        case 'B'
            ra1 = 291; ra2 = 303; dec1 = 44; dec2 = 52; % Kepler field
%             ra1 = 90; ra2 = 109; dec1 = -68; dec2 = -58; % SEP
%             ra1 = 269; ra2 = 295; dec1 = 65; dec2 = 74; % NEP
        case 'C'
            ra1 = 291; ra2 = 303; dec1 = 36.5; dec2 = 44.5; % Kepler field
%             ra1 = 89; ra2 = 106; dec1 = -74; dec2 = -65; % SEP
%             ra1 = 270; ra2 = 289; dec1 = 58; dec2 = 67; % NEP
        case 'D' 
            ra1 = 280; ra2 = 291; dec1 = 36.5; dec2 = 44.5; % Kepler field
%             ra1 = 64; ra2 = 91; dec1 = -74; dec2 = -65; % SEP
%             ra1 = 251; ra2 = 270; dec1 = 58; dec2 = 67; % NEP
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
    Ebv(IndNaN) = median(Ebv,'omitnan'); % change the non-existing Ebv for the median value of the field
    
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
    
    % run the simulation 
    simImage = ultrasat.usim('Cat', Cat, 'Mag', Mag, 'FiltFam','Johnson', 'Filt','V',...
        'SpecType',Args.SpecType,'Spec', Spec, 'Exposure', [Args.ExpNum 300], 'Ebv', Ebv,...
        'OutDir', Args.OutDir,'SkyCat', 1, 'PlaneRotation', Args.PlaneRotation,...
        'RA0', Args.RA0, 'Dec0', Args.Dec0, 'OutName', Args.OutName, 'Tile', Args.Tile);

end
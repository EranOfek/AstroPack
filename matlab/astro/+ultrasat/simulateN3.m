function simImage = simulateN3(Args)
    % simulate an ULTRASAT image from of the Kepler field from the input star catalog
    % 
    arguments
        Args.Tile    = 'B'    % the tile name
        Args.RA0     = 254    % the aimpoint (Kepler field -- 291,  NEP -- 270, SEP -- 90, N3 = 254)
        Args.Dec0    =  64    % the aimpoint (Kepler field -- 44.5, NEP -- +66.560708, SEP -- -66.560708, N3 = 64)
        Args.ExpNum  =   1    % the number of exposures
        Args.PlaneRotation = 0
        Args.OutDir  = '.'
        Args.OutName = 'ULTRASAT.B_N3'
        Args.FiltFam = 'GAIA'
        Args.Filter  = 'Bp'
        Args.MagColName = 'phot_bp_mean_mag'
        Args.EbvColName = 'ebpminrp_gspphot'
        Args.EbvCoff = 0.7; % https://iopscience.iop.org/article/10.3847/1538-4357/aaf23f
        Args.Ebv     =   [];   % if not empty, use it as a single value
        Args.Catalog = 'N3gaia10deg.fits' 
        Args.Dir     = '/home/ocs/USim/' % '/home/sasha/ULTRASAT/SimImages/KeplerField';
        Args.SpecType = 'Pickles' % 'BB' or 'Pickels'
        Args.SingleType = false % one type of objects (for tests)
        Args.SingleTeff = [] % employed for 'Single' type of objects 
        Args.SingleLogg = [] % employed for 'Single' type of objects
        Args.NoisePoisson = true  % add Poission noise to the final image 
        Args.FilterInputCatalogBox = false
    end
    
    Dir = pwd;
    cd(Args.Dir);
    % SrcTab  = readtable(Args.Catalog,'FileType','text');    
    AC = AstroCatalog(Args.Catalog);
    SrcTab = AC.Table;
    cd(Dir);
    
    if Args.FilterInputCatalogBox
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
    else
        F   = isnan(SrcTab.(Args.MagColName)) | isnan(SrcTab.phot_rp_mean_mag);
        Tab = SrcTab(~F,:);
    end
      
    %%% TEST ONLY!!: cut a small area        
%     Tab = Tab(Tab.x_ra > 294.36 & Tab.x_ra < 294.48 & Tab.dec > 46.52 & Tab.dec < 46.64,:); 
%     fprintf('ATTENSION! ARBITRATRY CUTS APPLIED TO THE SOURCE LIST!\n');    
    %%%

    %%% TEST ONLY!!
%     sortedTable = sortrows(Tab, 'Vmag');
%     Tab = sortedTable(1:10000,:);
%     fprintf('ATTENSION! ARBITRATRY CUTS APPLIED TO THE SOURCE LIST!\n');   
    %%% END TEST 
       
    Mag0 = Tab.(Args.MagColName);   
    Cat  = [Tab.ra Tab.dec];

    if ~isempty(Args.Ebv)
        Ebv  = Args.Ebv; % one value for the whole field %% TEST ONLY
    else 
        Ebv  = Args.EbvCoff .* Tab.(Args.EbvColName); % individual values 
        IndNaN = isnan(Ebv);
        Ebv(IndNaN) = median(Ebv,'omitnan'); % change the non-existing Ebv for the median value of the field
    end

    Tab.Teff = Tab.teff_gspphot;
    Tab.logg = Tab.logg_gspphot;
    BP_RP = Tab.phot_bp_mean_mag-Tab.phot_rp_mean_mag;
    Theta = 0.4929 + 0.5092.*BP_RP - 0.0353.*BP_RP.^2; % https://www.aanda.org/articles/aa/full_html/2021/09/aa40979-21/aa40979-21.html
    
    Ind = isnan(Tab.Teff) | isnan(Tab.logg);
    Tab.Teff(Ind) = 5040./Theta(Ind); % 
    Tab.logg(Ind) = 4.5; % assume for all the cases where logg is not known     
    
    % deredden the magnitudes (the simulator deals with dereddened values!)
    Filt = AstFilter.get(Args.FiltFam,Args.Filter);
    deltaMag = astro.extinction.extinction(Ebv,Filt.pivot_wl/1e4);
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
            if Args.SingleType
                Spec = repmat([Args.SingleTeff Args.SingleLogg],height(Tab),1); % single type for all the objects 
            else
                Spec = [Tab.Teff Tab.logg]; % parameters of the Pickles' spectra from the input table
            end
        otherwise            
            error('Unknown spectral type');
    end
    
    % run the simulation 
    simImage = ultrasat.usim('Cat', Cat, 'Mag', Mag, 'FiltFam',Args.FiltFam, 'Filt',Args.Filter,...
        'SpecType',Args.SpecType,'Spec', Spec, 'Exposure', [Args.ExpNum 300], 'Ebv', Ebv,...
        'OutDir', Args.OutDir,'SkyCat', 1, 'PlaneRotation', Args.PlaneRotation,...
        'RA0', Args.RA0, 'Dec0', Args.Dec0, 'OutName', Args.OutName, 'Tile', Args.Tile,...
        'NoisePoisson',Args.NoisePoisson);

end
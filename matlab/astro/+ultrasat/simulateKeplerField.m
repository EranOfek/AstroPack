function simImage = simulateKeplerField(Args)

    arguments
        Args.Tile    = 'B';    % the tile name
        Args.RA0     = 291;    % the aimpoint
        Args.Dec0    =  44.5;  % the aimpoint
        Args.ExpNum  =   1;  % the number of exposures
        Args.PlaneRotation = 0;
        Args.OutDir  = '.';
        Args.OutName = 'SimKepler'
        Args.Ebv     = 0; % the actual E(B-V) in the Kepler field is about 0.4 ?
        Args.Catalog = 'Kepler_ULTRASAT_all.tbl';
        Args.Dir     = '/home/sasha/KeplerField';
    end
    
    Wave    = 2000:11000;   % the wavelength band in A
    
    % make a grid of BB spectra
    Temp = 2500:250:12000;
    S = repmat(AstroSpec,1,numel(Temp));
    for i = 1:numel(Temp)
        S(i) = AstroSpec.blackBody(Wave',Temp(i));
    end
    
    cd(Args.Dir)
    
%     SrcList = 'Kepler_ULTRASAT_all.tbl'; % 'Kepler_ULTRASAT_all_10lines.tbl';
    
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
    
    % test:
%     sortedTable = sortrows(Tab, 'Vmag');
%     Tab = sortedTable(1:2000,:);
        
    Cat = [Tab.x_ra Tab.dec];
    Mag =  Tab.Vmag;
    
    NSrc    = size(Tab,1);
    Spec    = repmat(AstroSpec,1,NSrc);
    
    for ISrc = 1:NSrc
        
        diff = abs(Tab.Teff(ISrc)-Temp);
        [~, ind] = min(diff);
        Spec(ISrc) = S(ind);
        
%         Spec(ISrc)  = AstroSpec.blackBody(Wave',Tab.Teff(ISrc));
%         % this is way to slow and voluminous!
        
    end
    
    %%%% run the simulation 
    simImage = ultrasat.usim('Cat', Cat, 'Mag', Mag, 'FiltFam','Johnson', 'Filt','V',...
        'Spec', Spec, 'Exposure', [Args.ExpNum 300], 'Ebv',Args.Ebv,...
        'OutDir', Args.OutDir,'SkyCat', 1, 'PlaneRotation', Args.PlaneRotation,...
        'RA0', Args.RA0, 'Dec0', Args.Dec0, 'OutName', Args.OutName, 'Tile', Args.Tile);

end
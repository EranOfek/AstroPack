function [Alam, RA_grid, Dec_grid] = extinctionGrid(SkyGrid, Args)
    % Make a grid of extinction (A_lam) values for a given sky grid and filter (or frequency) 
    % Optionally save the results in a .mat file
    % 
    % The map of Schlegel, Finkbeiner, and Davis was based on the data of IRAS,
    % whose beam was about 5 arcmin ~ 0.083 deg, so there is no sense in making
    % the grid smaller than 0.04 deg. 
    % Indeed, the pixel size of the dust maps used in astro.spec.sky_ebv is
    % 0.04 deg, and the total number of pixels is 4096^2 ~ 17 Mpix
    %
    % Input: - a text file containing pairs of RA, Dec in [deg] or a vector of [RA, Dec] 
    %         * ...,key,val,...
    %         'CooType' - can be 'ec', 'g', or 'j2000.0'
    %         'Filter'  - can be an AstroFilter object, a single wavelength in [mum] or 'ULTRASAT'
    %         'Plot' - whether to plot the map of Ebv and A_lam 
    % Output: - the .mat object containing the (irregular) coordinate grid and the A_lam values 
    % Author: A.M. Krassilchtchikov (Dec 2023)
    % Example: extinctionGrid('healpix215deg.txt','CooType','g','Filter',0.25);
    %          extinctionGrid('healpix215deg.txt','CooType','ec','Filter','ultrasat');
    arguments
        SkyGrid       = '~/matlab/data/ULTRASAT/healpix_grid_nside_512_npix_3145728_pixarea_0.013_deg.txt'; 
        Args.CooType  = 'ec';       % can be 'ec', 'g', 'j2000.0'
        Args.Filter   = 'ULTRASAT'; % or wavelength in [mum]
        Args.Plot logical = false;
        Args.SaveMat logical = false;
        Args.ExtMap = 'SFD98';       
    end
    
    RAD  = 180./pi;
    Tiny = 1e-7;

    if isnumeric(SkyGrid)
        RA_grid  = SkyGrid(:,1);
        Dec_grid = SkyGrid(:,2);
    else
        FN = tools.os.relPath2absPath(SkyGrid);
        Grid = readtable(FN);
        RA_grid  = Grid.Var1; Dec_grid = Grid.Var2;
    end

    % convert to galactic l, b (in radians):
    switch Args.CooType
        case {'eq', 'j2000.0'}
            [gal_lon, gal_lat] = celestial.coo.convert_coo(RA_grid./RAD,Dec_grid./RAD,'j2000.0','g');
        case 'ec'
            [gal_lon, gal_lat] = celestial.coo.convert_coo(RA_grid./RAD,Dec_grid./RAD,'ec','g');
        case 'g'
            gal_lon = RA_grid./RAD; gal_lat = Dec_grid./RAD;
        otherwise
            error('Unknown coordinate type');
    end    

    % calculate E(B-V)
    if strcmpi(Args.ExtMap,'SFD98')
        Ebv = astro.extinction.sky_ebv(gal_lon,gal_lat,'g', 'Map', 'SFD98'); % SFD 1998
    elseif strcmpi(Args.ExtMap,'G24')
        Ebv = astro.extinction.sky_ebv(gal_lon,gal_lat,'g', 'Map', 'G24');   % Gontcharov et al. 2024 
    elseif strcmpi(Args.ExtMap,'CSFD23')        
        Ebv = astro.extinction.sky_ebv(gal_lon,gal_lat,'g', 'Map', 'CSFD23'); % Chiang 2023 
    else
        error('Unknown extinction map');
    end
    
    % treating small or negative E(B-V) [some of the maps contain such artifacts]
    Ebv(Ebv<=0) = Tiny;

    % calculate A_lam
    if strcmpi(Args.Filter,'ultrasat')
        I = Installer;
        UP_db = sprintf('%s%s',I.getDataDir('ULTRASAT_Properties'),'/P90_UP_test_60_ZP_Var_Cern_21.mat');
        io.files.load1(UP_db,'UP');
        Filter = UP.U_AstFilt(1);
    else
        Filter = Args.Filter;
    end
    
    Alam = astro.extinction.extinction(Ebv,Filter);
    
    % plot E(B-V) and A_lam
    if Args.Plot
        figure(1); subplot(2,1,1)
        plot.ungridded_image(RA_grid,Dec_grid,log10(Ebv)); caxis([-2.5, 1.5]);
        xlabel 'RA, deg.'; ylabel 'Dec, deg.'; title 'lg(E_{B-V})'
        subplot(2,1,2)
        plot.ungridded_image(RA_grid,Dec_grid,Alam); caxis([0, 1]);
        xlabel 'RA, deg.'; ylabel 'Dec, deg.'
        title 'A_{USat} (R1)'
    end
    
    % save a mat object with the sky grid and A_lam
    if Args.SaveMat
        if strcmp(Args.ExtMap,'old')
            Fname = sprintf('extinction_grid_%s_%s_AbsMapSFD98.mat',Args.CooType,Args.Filter); 
        else
            Fname = sprintf('extinction_grid_%s_%s_AbsMapGont24.mat',Args.CooType,Args.Filter);
        end
        save(Fname,'Alam','RA_grid','Dec_grid');
    end
end



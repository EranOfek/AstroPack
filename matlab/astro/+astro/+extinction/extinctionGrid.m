function [Alam, RA_grid, Dec_grid] = extinctionGrid(SkyGrid, Args)
    % Make a .mat object containing extinction (A_lam) for a given sky grid and filter (or frequency) 
    % 
    % The map of Schlegel, Finkbeiner, and Davis was based on the data of IRAS,
    % whose beam was about 5 arcmin ~ 0.083 deg, so there is no sense in making
    % the grid smaller than 0.04 deg. 
    % Indeed, the pixel size of the dust maps used in astro.spec.sky_ebv is
    % 0.04 deg, and the total number of pixels is 4096^2 ~ 17 Mpix
    %
    % Input: - a text file containing pairs of RA, Dec in [deg]
    %         * ...,key,val,...
    %         'CooType' - can be 'ec', 'g', or 'j2000.0'
    %         'Filter'  - can be an AstroFilter object, a single wavelength in [mum] or 'ULTRASAT'
    %         'Plot' - whether to plot the map of Ebv and A_lam 
    % Output: - the .mat object containing the (irregular) coordinate grid and the A_lam values 
    % Author: A.M. Krassilchtchikov (Dec 2023)
    % Example: extinctionGrid('healpix215deg.txt','CooType','g','Filter',0.25);
    %          extinctionGrid('healpix215deg.txt','CooType','ec','Filter','ultrasat');
    arguments
        SkyGrid       = '~/matlab/data/ULTRASAT/healpix0.003deg.txt'; % healpix0.2deg.txt
        Args.CooType  = 'ec';       % can be 'ec', 'g', 'j2000.0'
        Args.Filter   = 'ULTRASAT'; % or wavelength in [mum]
        Args.Plot logical = false;
    end
    
    RAD = 180./pi;

    FN = tools.os.relPath2absPath(SkyGrid);
    Grid = readtable(FN); 
    RA_grid  = Grid.Var1; Dec_grid = Grid.Var2;

    % convert to galactic l, b (in radians):
    switch Args.CooType
        case 'eq'
            [gal_lon, gal_lat] = celestial.coo.convert_coo(RA_grid./RAD,Dec_grid./RAD,'j2000.0','g');
        case 'ec'
            [gal_lon, gal_lat] = celestial.coo.convert_coo(RA_grid./RAD,Dec_grid./RAD,'ec','g');
        case 'g'
            gal_lon = RA_grid./RAD; gal_lat = Dec_grid./RAD;
        otherwise
            error('Unknown coordinate type');
    end    

    % calculate E(B-V)
    Ebv = astro.extinction.sky_ebv(gal_lon,gal_lat,'g');

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
        RA0 = linspace(min(RA_grid),max(RA_grid),300);
        Dec0 = linspace(min(Dec_grid),max(Dec_grid),300);
        [lq, bq] = meshgrid(RA0, Dec0);
        Ebv_gr = griddata(RA_grid, Dec_grid, Ebv, lq, bq);
        imagesc(RA0, Dec0, log10(Ebv_gr)); colorbar;
        xlabel 'RA, deg.'; ylabel 'Dec, deg.'
        title 'lg(E_{B-V})'
        subplot(2,1,2)
        Alam_gr = griddata(RA_grid, Dec_grid, Alam, lq, bq);
        imagesc(RA0, Dec0, Alam_gr); caxis([0, 1]); colorbar;
        xlabel 'RA, deg.'; ylabel 'Dec, deg.'
        title 'A_{USat} (R1)'
    end
    
    % save a mat object with the sky grid and A_lam
    Fname = sprintf('extinction_grid_%s_%s',Args.CooType,Args.Filter);
    save(Fname,'Alam','RA_grid','Dec_grid');
end



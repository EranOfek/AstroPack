function Stat = statSkyGrid(GridData, StatGridFile, Args)
    % calculate statistics of a gridded value for a given sky position and radius
    % NOTE: the input sky position(s) should belong to the same coordinate
    % system as those in the input GridData! 
    % NOTE2: for the statistics be sensible, the StatGrid should consist of
    % equal area pixels, e.g. HEALPix
    % 
    % Input: - a matrix of precalculated (fine) grid values: RA, Dec, Value
    %         OR a .mat object containing variables named RA_grid, Dec_grid, Alam
    %        - a text file with a grid of equal area sky pixels: RA, Dec (in [deg])
    %         * ...,key,val,...
    %         'SkyPos' - [RA, Dec] coordinates of the points arount which the statistics is measured
    %          NOTE: the input sky position(s) should belong to the same coordinate
    %          system as those in the input GridData!
    %         'Rad' - (in deg.) the radius of the area where the statistics is measured
    %         'StatFunc' - a handle of the statistical function of interest (e.g., @mean or @std)
    %         'Plot' - whether to plot the map of Ebv and A_lam 
    %         'Save' - whether to save the results in a .mat object
    %         'InterpMethod' - the interp. method to be used 
    % Output: - a vector of the measured statistical property
    %         - a .mat object containing the input coordinates of the measured statistical property
    % Author: A.M. Krassilchtchikov (Dec 2023) 
    % Example: RA = 0:5:360;
    %          Dec = -90:5:90;
    %          [XX, YY] = meshgrid(RA, Dec); Coo = [XX(:) YY(:)];
    %          Res = statSkyGrid('SkyPos',Coo);
    %          imagesc(RA,Dec,Res.mean'); caxis([0,1]); colorbar; 
    %          xlabel 'RA_{ec}, deg'; ylabel 'Dec_{ec}, deg'; title 'A_{USat} averaged over R = 7 deg' 
    arguments
        GridData  = '~/matlab/data/ULTRASAT/extinction_grid_ec_ULTRASAT.mat';
        StatGridFile = '~/matlab/data/ULTRASAT/healpix0.2deg.txt';
        Args.SkyPos  = [120 7]; % deg
        Args.Rad     = 7;       % deg
        Args.StatFunc = @mean;  % or any other function with vector output
        Args.Plot logical = false;
        Args.Save logical = false;
        Args.InterpMethod = 'nearest';
    end

    RAD = 180./pi;

    RA0  = Args.SkyPos(:,1)/RAD;
    Dec0 = Args.SkyPos(:,2)/RAD;
    Rad  = Args.Rad/RAD; % in radians

    nPos = numel(RA0);
    Stat = zeros(nPos,1);
    
    % read the data on measured on a fine grid RA_grid, Dec_grid
    % and interpolate them to a regular grid
    if ~isobject(GridData)
        FN = tools.os.relPath2absPath(GridData);
        io.files.load1(FN);
        Val = Alam;
    elseif ~ismatrix(GridData)
        RA_grid  = GridData(:,1);
        Dec_grid = GridData(:,2);
        Val      = GridData(:,3);
    else
        error('Unknown GridData type');
    end
    F = scatteredInterpolant(RA_grid, Dec_grid, Val, Args.InterpMethod);

    % read the sky grid of equal area zones (pixels) to be used for statistical purposes:
    % it should be pre-chosen such that a reasonable number of pixels (~ 30-1000) 
    % fall within Args.Rad for each of the input [Args.RA, Args.Dec]
    FN = tools.os.relPath2absPath(StatGridFile);
    Stattab  = readtable(FN);
    Statgrid = [Stattab.Var1./RAD, Stattab.Var2./RAD]; 
   
    for iPos = 1:nPos
        % measure the distance of the current point to all the points of the coarse statgrid:
        Dist_sky = celestial.coo.sphere_dist_fast(RA0(iPos),Dec0(iPos),Statgrid(:,1),Statgrid(:,2));
        % and select those falling within Rad:
        Subgrid = Statgrid(Dist_sky < Rad,:);
        % interpolate the values measured on the fine grid to the subgrid points:
        GridVals = F(Subgrid(:,1)*RAD, Subgrid(:,2)*RAD);
        % measure the desired statistical characteristsics of the selected set of Alam:
        Stat(iPos) = Args.StatFunc(GridVals);
    end

    if Args.Plot        
        RAp  = linspace(min(RA0),max(RA0),300);
        Decp = linspace(min(Dec0),max(Dec0),300);
        [RAq, Decq] = meshgrid(RAp, Decp);
        Statp = griddata(RA0, Dec0, Stat, RAq, Decq);
        imagesc(RAp*RAD, Decp*RAD, Statp); colorbar; caxis([0, 1]);
    end

    if Args.Save
        Fname = sprintf('Stat_rad%d',Args.Rad);
        RA_st = Args.SkyPos(:,1); Dec_st = Args.SkyPos(:,2);
        save(Fname,'Stat','RA_st','Dec_st');
    end

end

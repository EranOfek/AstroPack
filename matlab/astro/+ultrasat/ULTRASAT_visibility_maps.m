function ULTRASAT_visibility_maps(Args)
    % calculate and plot uninterrupted ULTRASAT visibilities for the whole sky
    
    arguments
        Args.GridFile = '~/matlab/data/ULTRASAT/healpix_grid_nside_64_npix_49152_pixarea_0.839_deg.txt' 
        % 'healpix_grid_nside_32_npix_12288_pixarea_3.357_deg.txt'; 'healpix_grid_nside_64_npix_49152_pixarea_0.839_deg.txt';
        Args.AllSky   = '~/matlab/data/ULTRASAT/charged_particles_350_rep1.txt'; 
        Args.StartDate = '2027-01-01 00:00:00';
        Args.NumDays   = 540;  % [days]
        Args.TimeBin   = 0.01; % [days] 0.01 day = 864 s ~ 3 x 300 s  
        Args.SaveMat logical = false; 
    end
    
    RAD  = 180/pi;
    Tiny = 1e-6;
    
    LimitType = {'SunLimits','EarthLimits','MoonLimits','PowerLimits'};
    NType = numel(LimitType);
    
    % read a reasonable dense equiareal grid in the equatorial coordinates
    Grid = readmatrix(Args.GridFile); RA = Grid(:,1); Dec = Grid(:,2);
    Np   = length(Grid);
    % convert the grid to the ecliptic coordinates
    [lambda,beta] = celestial.coo.convert_coo(RA./RAD,Dec./RAD,'j2000.0','e');
    lambda = lambda .* RAD; beta = beta .* RAD;
    
    % make pole marks
    Poles = [0, -90; 0, 90];
    [RA0,Dec0]  = celestial.coo.convert_coo(Poles(:,1)./RAD,Poles(:,2)./RAD,'e','j2000.0');
    [lam0,bet0] = celestial.coo.convert_coo(Poles(:,1)./RAD,Poles(:,2)./RAD,'j2000.0','e');   
    
    % probe visibility for Args.TimeBin bins during Args.NumDays:
    JD = celestial.time.julday(Args.StartDate) + (0:Args.TimeBin:Args.NumDays)';
    Nt = length(JD);
    
    Vis = ultrasat.ULTRASAT_restricted_visibility(JD,Grid./RAD);

    for IType = 1:NType     
        Limits = Vis.(LimitType{IType});
        MaxLen.(LimitType{IType}) = uninterruptedLength(Limits, Np, Nt).* Args.TimeBin; % convert to [days]    
    end
   
                %%%%%%% PLOTTING BLOCK   
                Fig = figure('visible', 'off');

                plot.ungridded_image(RA, Dec, MaxLen.SunLimits); caxis([0, 360]);
                hold on; plot(RA0*RAD,Dec0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel 'RA, deg'; ylabel 'Dec, deg'; title 'Sun Limits: max(uninterrupted days)'; 
                saveas(gcf, 'SunLimitsJ2000.jpg');
                plot.ungridded_image(lambda, beta, MaxLen.SunLimits); caxis([0, 360]);
                hold on; plot(lam0*RAD,bet0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel '\lambda, deg'; ylabel '\beta, deg'; title 'Sun Limits: max(uninterrupted days)'; 
                saveas(gcf, 'SunLimitsEcl.jpg');

                plot.ungridded_image(RA, Dec, MaxLen.EarthLimits); caxis([0, 360]);
                hold on; plot(RA0*RAD,Dec0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel 'RA, deg'; ylabel 'Dec, deg'; title 'Earth Limits: max(uninterrupted days)'; 
                saveas(gcf, 'EarthLimitsJ2000.jpg');
                plot.ungridded_image(lambda, beta, MaxLen.EarthLimits); caxis([0, 360]);
                hold on; plot(lam0*RAD,bet0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel '\lambda, deg'; ylabel '\beta, deg'; title 'Earth Limits: max(uninterrupted days)'; 
                saveas(gcf, 'EarthLimitsEcl.jpg');

                plot.ungridded_image(RA, Dec, MaxLen.MoonLimits); caxis([0, 360]);
                hold on; plot(RA0*RAD,Dec0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel 'RA, deg'; ylabel 'Dec, deg'; title 'Moon Limits: max(uninterrupted days)'; 
                saveas(gcf, 'MoonLimitsJ2000.jpg');
                plot.ungridded_image(lambda, beta, MaxLen.MoonLimits); caxis([0, 360]);
                hold on; plot(lam0*RAD,bet0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel '\lambda, deg'; ylabel '\beta, deg'; title 'Moon Limits: max(uninterrupted days)'; 
                saveas(gcf, 'MoonLimitsEcl.jpg');

                plot.ungridded_image(RA, Dec, MaxLen.PowerLimits); caxis([0, 360]);
                hold on; plot(RA0*RAD,Dec0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel 'RA, deg'; ylabel 'Dec, deg'; title 'Power Limits: max(uninterrupted days)'; 
                saveas(gcf, 'PowerLimitsJ2000.jpg');
                plot.ungridded_image(lambda, beta, MaxLen.PowerLimits); caxis([0, 360]);
                hold on; plot(lam0*RAD,bet0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel '\lambda, deg'; ylabel '\beta, deg'; title 'Power Limits: max(uninterrupted days)'; 
                saveas(gcf, 'PowerLimitsEcl.jpg');    
    
    % combined constraints for the HCS:
    
    Limits = Vis.PowerLimits .* Vis.SunLimits .* Vis.MoonLimits .* Vis.EarthLimits;
    MaxLen.Combined = uninterruptedLength(Limits, Np, Nt).* Args.TimeBin; % convert to [days]
        
                %%%%%%% PLOTTING BLOCK
                plot.ungridded_image(RA, Dec, MaxLen.Combined); caxis([0, 360]);
                hold on; plot(RA0*RAD,Dec0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel 'RA, deg'; ylabel 'Dec, deg'; title 'Combined Limits: max(uninterrupted days)'; 
                saveas(gcf, 'CombinedLimitsJ2000.jpg');
                plot.ungridded_image(lambda, beta, MaxLen.Combined); caxis([0, 360]);
                hold on; plot(lam0*RAD,bet0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel '\lambda, deg'; ylabel '\beta, deg'; title 'Combined Limits: max(uninterrupted days)'; 
                saveas(gcf, 'CombinedLimitsEcl.jpg');

    % account for the extinction limits 
    
    Alam = astro.extinction.extinctionGrid(Args.GridFile,'CooType','j2000.0','Filter','ultrasat');
    
    Averaged_extinction = celestial.grid.statSkyGrid('SkyPos',[lambda beta]);
    
    % exclude points where the extinction is above 1 
    ExtinctionFlag = ones(Np,1); ExtinctionFlag( Alam > 1 ) = Tiny;  
    MaxLen.CombinedLocalExtinct = MaxLen.Combined .* ExtinctionFlag;
    
    ExtinctionFlag = ones(Np,1); ExtinctionFlag( Averaged_extinction > 1 ) = Tiny;  
    MaxLen.CombinedAverExtinct = MaxLen.Combined .* ExtinctionFlag;
    
                %%%%%%% PLOTTING BLOCK
                plot.ungridded_image(RA, Dec, MaxLen.CombinedLocalExtinct); caxis([0, 360]);
                hold on; plot(RA0*RAD,Dec0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel 'RA, deg'; ylabel 'Dec, deg'; title 'Combined + Extinction: max(uninterrupted days)'; 
                saveas(gcf, 'CombinedLimitsLocalExtinctionJ2000.jpg');
                plot.ungridded_image(lambda, beta, MaxLen.CombinedLocalExtinct); caxis([0, 360]);
                hold on; plot(lam0*RAD,bet0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel '\lambda, deg'; ylabel '\beta, deg'; title 'Combined + Extinction: max(uninterrupted days)'; 
                saveas(gcf, 'CombinedLimitsLocalExtinctionEcl.jpg');

                plot.ungridded_image(RA, Dec, MaxLen.CombinedAverExtinct); caxis([0, 360]);
                hold on; plot(RA0*RAD,Dec0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel 'RA, deg'; ylabel 'Dec, deg'; title 'Combined + Extinction: max(uninterrupted days)'; 
                saveas(gcf, 'CombinedLimitsAverExtinctionJ2000.jpg');
                plot.ungridded_image(lambda, beta, MaxLen.CombinedAverExtinct); caxis([0, 360]);
                hold on; plot(lam0*RAD,bet0*RAD,'ro', 'MarkerSize', 10); hold off
                xlabel '\lambda, deg'; ylabel '\beta, deg'; title 'Combined + Extinction: max(uninterrupted days)'; 
                saveas(gcf, 'CombinedLimitsAverExtinctionEcl.jpg');
    
    % make a table of HCS-approved positions and read in the all-sky pointing map:
    AllSky = readtable(Args.AllSky);
    
    Tab = table(Grid(:,1),Grid(:,2),MaxLen.CombinedAverExtinct,'VariableNames', {'RA', 'Dec', 'MaxLen'});
    Tab100 = Tab(Tab.MaxLen > 100,:);
    Tab360 = Tab(Tab.MaxLen > 360,:);
    
                figure(1); clf; hold on
                plot(Tab100.RA,Tab100.Dec,'*','Color','blue')
                plot(Tab360.RA,Tab360.Dec,'o','Color','green')        
                plot(AllSky.Var1,AllSky.Var2,'*','Color','red')
                hold off    
                xlabel 'RA, deg'; ylabel 'Dec, deg';
                title 'HCS constraints, Blue > 100 days, Green > 365 days';
                
    % save the MaxLen structure and the equatorial grid in a matlab object
    if Args.SaveMat
        save('uninterruptedULTRASATvisibility.mat','MaxLen','Grid');
    end
   
end

function MaxLen = uninterruptedLength(Limits, Npos, Ntime)
    % internal function to calulate the maximal uninterrupted length of
    % true period (in time bins) of a logical function for a set of sky
    % positions
    MaxLen = zeros(Npos,1);
    for Ip = 1:Npos      % sky positions
        Len = 0;
        for It = 1:Ntime % time bins
            if Limits(It,Ip) == 0
                Len = 0;
            else
                Len = Len + 1;
            end
            MaxLen(Ip) = max(MaxLen(Ip),Len);
        end
    end
end
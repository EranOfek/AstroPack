function ULTRASAT_visibility_maps_LCS(Args)
    % calculate and plot uninterrupted ULTRASAT visibilities for the whole sky
    % Input: -
    %         * ...,key,val,...
    %        'GridFile' - the primary sky grid as [RA, Dec] (equatorial) to used for all the maps
    %        'AllSky'   - a grid of all-sky ULTRSAT pointings as [RA, Dec] (equatorial) to overlay on the maps
    %        'StartDate' - the start time of the period of interest
    %        'NumDays'   - the number of days in the period of interest
    %        'TimeBin'   - time bin [days]: the function checks uninterrupted visibility for consequent bins of this size
    %        'SaveMat'   - whether the visibilities are saved in a .mat object
    %        'LimitingAlambda' - the limiting upper value of A_lambda to cut the maps  
    % Output: - the function produces multiple plots of ULTRASAT visibility
    %           maps with various sets of limits applied (see the LimitType
    %           list + extinction limits)
    % Author: A.M. Krassilchtchikov (Sep 2024)
    % Example: ultrasat.ULTRASAT_visibility_maps('LimitingAlambda',0.5,'SaveMat',1); 
    %          ultrasat.ULTRASAT_visibility_maps_LCS('AllSky','~/matlab/data/ULTRASAT/all_sky_grid_charged_particles_240_nonoverlapping.txt')
    arguments
        Args.GridFile = '~/matlab/data/ULTRASAT/healpix_grid_nside_64_npix_49152_pixarea_0.839_deg.txt' 
        % 'healpix_grid_nside_32_npix_12288_pixarea_3.357_deg.txt'; 'healpix_grid_nside_64_npix_49152_pixarea_0.839_deg.txt';
        %  can be produced localy by > celestial.grid.make_healpix_grid(64)
        Args.AllSky   = '~/matlab/data/ULTRASAT/all_sky_grid_charged_particles_240_nonoverlapping.txt'; % '~/matlab/data/ULTRASAT/all_sky_grid_charged_particles_350_rep1.txt'; 
        Args.StartDate = '2028-01-01 00:00:00';
        Args.NumDays   =  540; % [days] % 540
        Args.TimeBin   = 0.01; % [days] 0.01 day = 864 s ~ 3 x 300 s  
        Args.SaveMat logical = true; 
        Args.LimitingAlambda = 1; % the limiting value of A_lambda (for the ULTRASAT band)
    end
    
    RAD  = 180/pi;
    
    % High-cadence survey areas
    HCS = [215, 60; 254, 64; 67, -59];
   
    % read a reasonably dense equiareal grid in the equatorial coordinates
    Grid = readmatrix(Args.GridFile); RA = Grid(:,1); Dec = Grid(:,2); Np   = length(Grid);
    % convert the grid to the ecliptic coordinates
    [lambda,beta] = celestial.coo.convert_coo(RA./RAD,Dec./RAD,'j2000.0','e');
    lambda = lambda .* RAD; beta = beta .* RAD;
    
    % read the All Sky grid of pointings
    AllSky = readtable(Args.AllSky); 
    
    % add some shift by by RA:
    ShiftRA = 40; % [deg]
    for i=1:size(AllSky,1)
        if AllSky.Var1(i) > ShiftRA
            AllSky.Var1(i) = AllSky.Var1(i)-ShiftRA;
        else
            AllSky.Var1(i) = 360+AllSky.Var1(i)-ShiftRA;
        end 
    end
    
    % make ecliptic pole marks
    Poles = [0, -90; 0, 90];
    [RA0,Dec0]  = celestial.coo.convert_coo(Poles(:,1)./RAD,Poles(:,2)./RAD,'e','j2000.0');
%     [lam0,bet0] = celestial.coo.convert_coo(Poles(:,1)./RAD,Poles(:,2)./RAD,'j2000.0','e');   
    
    % build a list of night-time bins: NightBins bins every night from 0 to 3 UTC
    NightBins = 12; % 12 x 0.01 day = 0.12 x 24 = 2.88 hr
    l = zeros(1,NightBins*Args.NumDays);
    for i=1:Args.NumDays
        for j=1:NightBins
            k = (i-1)*NightBins+j;
            l(k) = (i-1)+(j-1).*Args.TimeBin;
        end
    end
    
    ConstShift = -4/360-1.44/24; % accounting for the 4W slot and centering the 2.88 hours' period
    
    JD = celestial.time.julday(Args.StartDate) + l - ConstShift;
    
    Vis = ultrasat.ULTRASAT_restricted_visibility(JD',Grid./RAD,'MinSunDist',(70)./RAD,'MinMoonDist',(34)./RAD,'MinEarthDist',(56)./RAD);
    Lim = Vis.PowerLimits & Vis.SunLimits & Vis.MoonLimits & Vis.EarthLimits; 
    L2 = reshape(Lim,[NightBins,Args.NumDays,Np]); 
    L3 = squeeze(prod(L2,1));                                % L3 is a whole-night scale list of visibility bins
    MaxLen = uninterruptedLength(L3, Np, Args.NumDays); 
    F = scatteredInterpolant(RA, Dec, MaxLen, 'linear', 'none');
  
%             % now show fields available for each of the four 45-day periods:
%             % products of 1-45, 46-90, 91-135, 136-180 
%             Q(1,:) = prod(L3(1:45,:),1);   Q(2,:) = prod(L3(46:90,:),1); 
%             Q(3,:) = prod(L3(91:135,:),1); Q(4,:) = prod(L3(136:180,:),1); 
%             figure(2); subplot(2,2,1); plot.ungridded_image(RA, Dec, Q(1,:));
%             subplot(2,2,2); plot.ungridded_image(RA, Dec, Q(2,:));
%             subplot(2,2,3); plot.ungridded_image(RA, Dec, Q(3,:));
%             subplot(2,2,4); plot.ungridded_image(RA, Dec, Q(4,:));
% 
            figure(1); clf; plot.ungridded_image(RA, Dec, MaxLen); caxis([0, 180]);
            title 'max uninterruped visibility of the 22:17-01:10 GMT window, days'    

    % find a sublist of AllSS pointings visible > 45 (180) days 
    Lenp   = F(AllSky.Var1,AllSky.Var2);
    List45 = AllSky(Lenp>45,:); List180 = AllSky(Lenp>180,:);
    
            fprintf('Pointings visible > 45  days: %d\n',size(List45,1));
            fprintf('Pointings visible > 180 days: %d\n',size(List180,1));
    
            figure(1); hold on
            plot(AllSky.Var1,  AllSky.Var2,'*','Color','black');
            for i=1:numel(AllSky.Var1)
                plot.skyCircles(AllSky.Var1(i), AllSky.Var2(i), 'Rad', 7, 'Color','black');
            end
            plot(List45.Var1,List45.Var2,'*','Color','black');
            plot(List180.Var1,List180.Var2,'*','Color','red');
            
            for i=1:3
                plot.skyCircles(HCS(i,1), HCS(i,2), 'Rad', 7, 'Color','red');                
            end
            plot.skyCircles(RA0(1).*RAD,Dec0(1).*RAD,'Rad', 1,'Color','green')
            plot.skyCircles(RA0(2).*RAD,Dec0(2).*RAD,'Rad', 1,'Color','green')

    % now we turn on averaged extinction:        
    Averaged_extinction = celestial.grid.statSkyGrid('SkyPos',[lambda beta]);         
    Fext = scatteredInterpolant(RA, Dec, Averaged_extinction, 'linear', 'none');
      
    Extp   = Fext(AllSky.Var1,AllSky.Var2);
    List45e = AllSky(Lenp>45 & Extp < 1,:); List180e = AllSky(Lenp>180 & Extp <1,:);
    
            fprintf('Low extinction pointings visible > 45  days: %d\n',size(List45e,1));
            fprintf('Low extinction pointings visible > 180 days: %d\n',size(List180e,1));
    
            figure(2); plot.ungridded_image(RA, Dec, MaxLen .* (Averaged_extinction < 1)); caxis([0, 180]);
            hold on
            plot(AllSky.Var1,  AllSky.Var2,'*','Color','black');
            for i=1:numel(AllSky.Var1)
                plot.skyCircles(AllSky.Var1(i), AllSky.Var2(i), 'Rad', 7, 'Color','black');
            end
            plot(List45e.Var1,  List45e.Var2,'*','Color','green');
            plot(List180e.Var1,List180e.Var2,'*','Color','red');
            title 'max uninterruped visibility of the 22:17-01:10 GMT window, days'
            
            for i=1:3
                plot.skyCircles(HCS(i,1), HCS(i,2), 'Rad', 7, 'Color','red');
            end
            plot.skyCircles(RA0(1).*RAD,Dec0(1).*RAD,'Rad', 1,'Color','green')
            plot.skyCircles(RA0(2).*RAD,Dec0(2).*RAD,'Rad', 1,'Color','green')
            
%             cd ~/'Dropbox (Weizmann Institute)'/Observation_planning/Field_selection/WGs_maps/
%             load('WG6/WG6_HETDEX_spring_contour.mat')
%             plot(WG6_HETDEX_spring_contour(:,1),WG6_HETDEX_spring_contour(:,2),'black');
%             cd ~/
            xlabel '38/76 non-overlapping positions of 180/45 days visibility'
                        
                       
    % save the MaxLen structure and the equatorial grid in a matlab object
    if Args.SaveMat
        save('LCS_visibility.mat','AllSky', 'Grid', 'MaxLen', 'Averaged_extinction','Extp','Lenp','L2');
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

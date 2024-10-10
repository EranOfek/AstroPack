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
        Args.AveragedExt ='~/matlab/data/ULTRASAT/aver_ext_hp49152.mat'; % averaged extinction on a 49152 healpix grid 
        Args.StartDate = '2028-01-01 00:00:00';
        Args.NumDays   = 1279; % [days] % 540 % 1279 -- up to 01/07/2031 
        Args.TimeBin   = 0.01; % [days] 0.01 day = 864 s ~ 3 x 300 s  
        Args.SaveMat logical = false; 
        Args.LimitingAlambda = 1; % the limiting value of A_lambda (for the ULTRASAT band)
        Args.MakePlots = false; 
    end
    %
    RAD  = 180/pi; 
    
    % High-cadence survey areas
    HCS = [215, 60; 254, 64; 67, -59]; 
   
    % read a reasonably dense equiareal grid in the equatorial coordinates
    Grid = readmatrix(Args.GridFile); RA = Grid(:,1); Dec = Grid(:,2); Np = length(Grid);
    % convert the grid to the ecliptic coordinates
    [lambda,beta] = celestial.coo.convert_coo(RA./RAD,Dec./RAD,'j2000.0','e');
    lambda = lambda .* RAD; beta = beta .* RAD;
    
    % read the All Sky grid of pointings
    AllSky = readtable(Args.AllSky); AllSky = sortrows(AllSky,2);
    % convert the grid to the ecliptic coordinates
    [l_all,b_all] = celestial.coo.convert_coo(AllSky.Var1./RAD,AllSky.Var2./RAD,'j2000.0','e');
    l_all = l_all .* RAD; b_all = b_all .* RAD;
    AllSkyEc = table(l_all,b_all,'VariableNames',{'Var1','Var2'});
    
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
    
    % find a sublist of AllSS pointings visible > 45 (180) days 
    Lenp   = F(AllSky.Var1,AllSky.Var2);
    List45 = AllSky(Lenp>45,:); List180 = AllSky(Lenp>180,:);    
        fprintf('Pointings visible > 45  days: %d\n',size(List45,1));
        fprintf('Pointings visible > 180 days: %d\n',size(List180,1));
        
    % now we turn on averaged extinction (with R_aver = 7 deg)
    
    % recalculation takes much time:
%     Averaged_extinction = celestial.grid.statSkyGrid('SkyPos',[lambda beta]);         
    % so we read it from a prepared object:
    load(Args.AveragedExt);
    Fext    = scatteredInterpolant(RA, Dec, Averaged_extinction, 'linear', 'none');      
    Extp    = Fext(AllSky.Var1,AllSky.Var2);
    List45e = AllSky(Lenp>45 & Extp < 1,:); List180e = AllSky(Lenp>180 & Extp <1,:);
        fprintf('Low extinction pointings visible > 45  days: %d\n',size(List45e,1));
        fprintf('Low extinction pointings visible > 180 days: %d\n',size(List180e,1));

    % a smaller subset 
    Grid240 = [AllSky.Var1 AllSky.Var2];
    Vis240  = ultrasat.ULTRASAT_restricted_visibility(JD',Grid240./RAD,'MinSunDist',(70)./RAD,'MinMoonDist',(34)./RAD,'MinEarthDist',(56)./RAD);
    Lim240  = Vis240.PowerLimits & Vis240.SunLimits & Vis240.MoonLimits & Vis240.EarthLimits; 
    L2_240  = reshape(Lim240,[NightBins,Args.NumDays,240]); 
    L3_240  = squeeze(prod(L2_240,1));                       % L3 is a whole-night scale list of visibility bins
    SunAng     = RAD.*reshape(Vis240.SunAngDist,[NightBins,Args.NumDays,240]);
    MeanSunAng = squeeze(mean(SunAng,1));
          
            if Args.MakePlots
                
                figure(1); clf; hold on
                
                subplot(1,2,1)
                plot.ungridded_image(lambda, beta, MaxLen.* (Averaged_extinction < 1)); caxis([0, 180]);  % plot in ecliptic coordinates
                set(gca, 'Position', [0.05, 0.06, 0.4, 0.9]);
                xlabel '\lambda, deg'; ylabel '\beta, deg'
                title 'max uninterruped visibility of the 22:17-01:10 GMT window, days'
                for i=1:numel(AllSky.Var1)
                    plot.skyCircles(AllSkyEc.Var1(i), AllSkyEc.Var2(i), 'Rad', 7, 'Color','black');
                    text(AllSkyEc.Var1(i), AllSkyEc.Var2(i), num2str(i), 'VerticalAlignment', 'middle', 'HorizontalAlignment', 'center');
                end
                
                subplot(1,2,2)
                plot.ungridded_image(RA, Dec, MaxLen.* (Averaged_extinction < 1)); caxis([0, 180]);
                set(gca, 'Position', [0.55, 0.06, 0.4, 0.9]);
                
                xlabel 'RA, deg'; ylabel 'Dec, deg'
                for i=1:numel(AllSky.Var1)
                    plot.skyCircles(AllSky.Var1(i), AllSky.Var2(i), 'Rad', 7, 'Color','black');
                    text(AllSky.Var1(i), AllSky.Var2(i), num2str(i), 'VerticalAlignment', 'middle', 'HorizontalAlignment', 'center');
                end

%             plot(AllSky.Var1,  AllSky.Var2,'*','Color','black');   

%             plot(List45.Var1,List45.Var2,'*','Color','black');
%             plot(List180.Var1,List180.Var2,'*','Color','red');
%             
%             for i=1:3
%                 plot.skyCircles(HCS(i,1), HCS(i,2), 'Rad', 7, 'Color','red');                
%             end
%             plot.skyCircles(RA0(1).*RAD,Dec0(1).*RAD,'Rad', 1,'Color','green')
%             plot.skyCircles(RA0(2).*RAD,Dec0(2).*RAD,'Rad', 1,'Color','green')   
                            
%             cd ~/'Dropbox (Weizmann Institute)'/Observation_planning/Field_selection/WGs_maps/
%             load('WG6/WG6_HETDEX_spring_contour.mat')
%             plot(WG6_HETDEX_spring_contour(:,1),WG6_HETDEX_spring_contour(:,2),'black');
%             cd ~/
            end

    cprintf('blue','8 x 45 days (= 360 days) from day 1 x 10 objects, unique over the 360 days period:\n');
    [Route0,AvDist0,TargetLists0] = select_LCS_list(L3_240,Extp,AllSky,'NumPeriods',8,'UniqueSetArgs',...
                 {'StartDay',1,'PeriodLength',45,'FieldsPerPeriod',10,'AvLimit',1,'MeanSunAng',MeanSunAng,'Unique',1});  % MeanSunAng
             for i=1:8
                 fprintf('period %d: %d targets: ',i,numel(Route0{i})); fprintf('%g ',Route0{i}); fprintf('\n')                 
             end     
             
    cprintf('blue','2 x 180 days (= 360 days) from day 1 x 40 objects, unique over the 360 days period:\n');
    [Route00,AvDist00,TargetLists00] = select_LCS_list(L3_240,Extp,AllSky,'NumPeriods',2,'UniqueSetArgs',...
                 {'StartDay',1,'PeriodLength',180,'FieldsPerPeriod',40,'AvLimit',1,'MeanSunAng',MeanSunAng,'Unique',1});  % MeanSunAng
             for i=1:2
                 fprintf('period %d: %d targets: ',i,numel(Route00{i})); fprintf('%g ',Route00{i}); fprintf('\n')                 
             end     
             
%     cprintf('blue','4 x 45 days (= 180 days) from day 1 x 10 objects, unique over the 180 days period:\n');
%     [Route1,AvDist1,TargetLists1] = select_LCS_list(L3_240,Extp,AllSky,'NumPeriods',4,'UniqueSetArgs',...
%                 {'StartDay',1,'PeriodLength',45,'FieldsPerPeriod',10,'AvLimit',1,'MeanSunAng',MeanSunAng,'Unique',1});  % MeanSunAng
%             for i=1:4
%                 fprintf('period %d: %d targets: ',i,numel(Route1{i})); fprintf('%g ',Route1{i}); fprintf('\n')
%             end
% 
%     cprintf('blue','4 x 45 days (= 180 days) from day 181 x 10 objects, unique over the 180 days period:\n');
%     [Route2,AvDist2,TargetLists2] = select_LCS_list(L3_240,Extp,AllSky,'NumPeriods',4,'UniqueSetArgs',...
%                  {'StartDay',181,'PeriodLength',45,'FieldsPerPeriod',10,'AvLimit',1,'MeanSunAng',MeanSunAng,'Unique',1});  
%              for i=1:4
%                  fprintf('period %d: %d targets: ',i,numel(Route2{i})); fprintf('%g ',Route2{i}); fprintf('\n')                 
%              end       
%     
%     fprintf('Total unique fields in 360 days: %d \n',numel(unique([Route1{:} Route2{:}])))
%     fprintf('Average slewing distances (deg): '); fprintf('%.1f ',[AvDist1 AvDist2]); fprintf('\n');    
%     fprintf('Overall average slewing distance (deg): %.2f \n', mean([AvDist1 AvDist2]));
% %     fprintf('Mean Sun angle (deg): %.1f \n',mean(MeanSunAng(1:45,[Route1{1}]),[1 2]));
% %     
% %     cprintf('blue','2 x 180 days (= 360 days) from day 1 x 40 objects, non-unique:\n');
% %     [Route3,AvDist3,TargetLists3] = select_LCS_list(L3_240,Extp,AllSky,'NumPeriods',2,'UniqueSetArgs',...
% %                  {'StartDay',1,'PeriodLength',180,'FieldsPerPeriod',40,'AvLimit',1,'MeanSunAng',[],'Unique',0});                                      
% %              for i=1:2
% %                  fprintf('period %d: %d targets: ',i,numel(Route3{i})); fprintf('%g ',Route3{i}); fprintf('\n')                 
% %              end       
% %     
% %     cprintf('blue','2 x 180 days (= 360 days) from day 90 x 40 objects, non-unique:\n');
% %     [Route4,AvDist4,TargetLists4] = select_LCS_list(L3_240,Extp,AllSky,'NumPeriods',2,'UniqueSetArgs',...
% %                  {'StartDay',90,'PeriodLength',180,'FieldsPerPeriod',40,'AvLimit',1,'MeanSunAng',[],'Unique',0});                                      
% %              for i=1:2
% %                  fprintf('period %d: %d targets: ',i,numel(Route4{i})); fprintf('%g ',Route4{i}); fprintf('\n')                 
% %              end       
% %              
% %      % the number of 180-day long fields with a 360-days period:
% %      VisAbs = L3_240 .* (Extp' < 1);
% %      M2 = uninterruptedLength(VisAbs,240,360);
% %      A180 = find(M2>179); 
% %      cprintf('blue','%d fields observable for 180 days within 360 days\n',numel(A180));
% %      Vis180 = L3_240(1:360,A180);
% %      % number of such fields seen in each of the days:
% %      Ob180  = sum(Vis180,2); 
% %                  if Args.MakePlots
% %                      figure(2); plot(Ob180); ylabel 'Number of fields'
% %                  end

    % the number of 180-day-long fields observable on each of the days of a
    % 2.5 year period from start + 180 days until end-180 days: 
    cprintf('blue','2.5 years period from start+180 days until end-180 days, non-unique:\n');
    VisAbs = L3_240 .* (Extp' < 1);
    List180_240 = uninterruptedLength(VisAbs, 240, Args.NumDays) > 180;    
    fprintf('Total number of 180-day fields in the list: %d \n',sum(List180_240));
    Nfields     = sum(VisAbs(:,List180_240),2);
    fprintf('Average daily number of observable 180-day fields: %.0f \n',mean(Nfields));
                if Args.MakePlots
                    figure(3); plot(181:Args.NumDays-180,Nfields(181:Args.NumDays-180),'*');
                    xlabel 'Days from 01.01.2028'; ylabel 'Number of available 180-day long fields'
                end    
             
    % save the MaxLen structure and the equatorial grid in a matlab object
    if Args.SaveMat
        save('LCS_visibility.mat','AllSky', 'Grid', 'MaxLen', 'Averaged_extinction','Extp','Lenp','L2','L2_240','L3_240');
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

function [Route, AvDist, TargetLists, NotUsed] = select_LCS_list(VisTable,Av_ext,AllSky,Args)    
    % VisTable: M epochs x Np sky points (logical matrix)
    % Av_ext: Np values
    % AllSKY: Np points [RA, Dec] 

%   RAD   = 180/pi;
%   [l,b] = celestial.coo.convert_coo(AllSky.Var1./RAD,AllSky.Var2./RAD,'j2000.0','e');
%   l = l .* RAD; b = b .* RAD;
%   Av_ext = celestial.grid.statSkyGrid('SkyPos',[l b]);   
    arguments
        VisTable
        Av_ext
        AllSky
        Args.NumPeriods     = 1;
        Args.UniqueSetArgs  = {};
        Args.ShowPlots      = false        
    end

    % select fields
%     [Selected, NotUsed] = find_unique_set(VisTable,Av_ext,'NumPeriods',Args.NumPeriods,Args.UniqueSetArgs{:});
    [Selected, NotUsed] = find_unique_set2(VisTable,Av_ext,'NumPeriods',Args.NumPeriods,Args.UniqueSetArgs{:});
    
    % find optimal route for each set of fields
    for i = 1:Args.NumPeriods        
        [OptRoute, MinDist] = telescope.obs.optimal_route(AllSky.Var1(Selected{i}), AllSky.Var2(Selected{i}),...
                                'NIt',50,'ShowResult',Args.ShowPlots);
        Route{i} = Selected{i}(OptRoute);
        AvDist(i)= MinDist/numel(Selected{i}); % average distance per retargeting [deg]
        TargetLists{i} = [AllSky.Var1(Route{i}) AllSky.Var2(Route{i})];
    end
    
    Nmissed = numel(NotUsed);
    if Nmissed > 0
        fprintf('NB: %d fields were not used: %g',Nmissed);
        fprintf('%g ',NotUsed');
        fprintf('\n');
    end
    
end

function [Selected, NotUsed] = find_unique_set(VisTable,Av_ext,Args)
    % select unique fields for NumPeriods of PeriodLength-day long epochs starting from StartDay
    % according to the VisTable and Av < AvLimit, prioritize fields with large Sun angles   
    arguments
        VisTable
        Av_ext
        Args.StartDay        = 1;
        Args.NumPeriods      = 4;
        Args.PeriodLength    = 45;
        Args.FieldsPerPeriod = 10;
        Args.AvLimit         = 1;
        Args.Unique          = true;
        Args.MeanSunAng      = [];
    end
    LowExt = Av_ext < Args.AvLimit;
    Np     = size(VisTable,2);

    HCS_fields = zeros(1,Np); HCS_fields(223:224) = 1; HCS_fields(19) = 1; % NB: these numbers will change with rotation along RA

    Selected = cell(1,Args.NumPeriods);
    for Iper = 1:Args.NumPeriods   % e.g, 4 periods of 45 days within a single 180 days period 
        Day1 = (Iper-1)*Args.PeriodLength+Args.StartDay;
        DayN =     Iper*Args.PeriodLength+Args.StartDay-1;
        VisPeriod = prod(VisTable(Day1:DayN,:));        
        % for each period find fields of visibility + low extinction, avoiding the HCS fields 
        Flds{Iper} = find( VisPeriod .* LowExt' .* (1 - HCS_fields) > 0); 
        % sort fields according to the SunAngle averaged over the whole period Day1:DayN 
        if isempty(Args.MeanSunAng)
            SortedFlds = Flds{Iper}; % do not sort 
        else
            SunAng = Args.MeanSunAng(Day1:DayN,Flds{Iper});
            AvSunAng = squeeze(mean(SunAng,1));
            [~, idx] = sort(AvSunAng,'descend');
            SortedFlds = Flds{Iper}(idx);
        end
        % apply a greedy algorithm to build lists of globaly unique FieldsPerPeriod fields for each period        
        for i = 1:numel(Flds{Iper})
            Fld = SortedFlds(i);
            if Args.Unique
                if all(~cellfun(@(c) any(c == Fld), Selected)) && numel(Selected{Iper}) < Args.FieldsPerPeriod
                    Selected{Iper} = [Selected{Iper} Fld];
                end
            else
                if numel(Selected{Iper}) < Args.FieldsPerPeriod
                    Selected{Iper} = [Selected{Iper} Fld];
                end
            end
        end
    end

    NotUsed = setdiff([Flds{:}], [Selected{:}]);
end

function [Selected, NotUsed] = find_unique_set2(VisTable,Av_ext,Args)
    % select unique fields for NumPeriods of PeriodLength-day long epochs starting from StartDay
    % according to the VisTable and Av < AvLimit, prioritize fields with large Sun angles   
    arguments
        VisTable
        Av_ext
        Args.StartDay        = 1;
        Args.NumPeriods      = 8;
        Args.PeriodLength    = 45;
        Args.FieldsPerPeriod = 10;
        Args.AvLimit         = 1;
        Args.Unique          = true;
        Args.MeanSunAng      = [];
    end
    LowExt = Av_ext < Args.AvLimit;
    Np     = size(VisTable,2);

    HCS_fields = zeros(1,Np); HCS_fields(223:224) = 1; HCS_fields(19) = 1; % NB: these numbers will change with rotation along RA

    Selected = cell(1,Args.NumPeriods);
    
    for Iper = 1:Args.NumPeriods   % e.g, 8 periods of 45 days within a single 360 days period 
        Day1 = (Iper-1)*Args.PeriodLength+Args.StartDay;
        DayN =     Iper*Args.PeriodLength+Args.StartDay-1;
        Avail(Iper,:)  = prod(VisTable(Day1:DayN,:)) .* LowExt' .* (1 - HCS_fields) > 0;          
    end
    
    % measure availability throughout all the epochs    
    Weight = sum(Avail,1); 
    fprintf('Total number of unique fields: %d\n', numel(Weight(Weight>0)));
   
    for W = 1:Args.NumPeriods % distribute the fields according to the net availability
        
        Flds = find(Weight == W);
        
        for Iper = 1:Args.NumPeriods
            
            Ind     = find(Avail(Iper,Flds) > 0); % find fields of availability W accesible in periof Iper
            FldsPer = Flds(Ind);
            
            for iF = 1:numel(FldsPer)
                if all(~cellfun(@(c) any(c == FldsPer(iF)), Selected)) && numel(Selected{Iper}) < Args.FieldsPerPeriod
                    Selected{Iper} = [Selected{Iper}, FldsPer(iF)];
                end
            end
            
        end
    end
    
    NotUsed = setdiff(find(Weight>0), [Selected{:}]);
end



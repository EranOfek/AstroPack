classdef ExtinctionMaps < PlannerToolBase  
    % the class provides access to extinction maps
    % main functions:
    % -- extinctionMap (constructor)
    % -- buildMap (filling the map properties of an object)
    % -- plotMap (plotting the maps of an object)
    properties (Access = public) 
        Grid        
        Map         
        AveragedMap 
        MapOrigin = 'SFD98';
        CooType   = 'j2000.0';
        Filter    = 'ultrasat';
        AveragingRadius = 7;     % [deg] 
    end
    %
    properties(Access = private)        
    end
    % 
    methods % Constructor      
        function Obj = ExtinctionMaps(Args)
            % One line description
            %     Optional detailed description
            % Input  : -
            %          -
            %          * ...,key,val,...
            % Output : -
            % Author : A.M. Krassilchtchikov (2024 Aug)
            % Example: E = ultrasat.planner.extinctionMap         
            %          E.buildMap
            %          E.plotMap 
            arguments  
                Args.MapOrigin         = 'SFD98';
                Args.CooType           = 'j2000.0';
                Args.Filter            = 'ultrasat';  
                Args.AveragingRadius   = []; 
            end
            %
            if ~isempty(Args.MapOrigin)
                Obj.MapOrigin = Args.MapOrigin;
            end
            if ~isempty(Args.CooType)
                Obj.CooType = Args.CooType;
            end
            if ~isempty(Args.Filter)
                Obj.Filter = Args.Filter;
            end
            if ~isempty(Args.AveragingRadius)
                Obj.AveragingRadius = Args.AveragingRadius;
            end            
        end        
    end
    %     
    methods %Map building and plotting 
        %
        function Result = buildMap(Obj, CooGrid) 
            % get an extinction map on some grid
            arguments
                Obj
                CooGrid = '~/matlab/data/ULTRASAT/healpix_grid_nside_64_npix_49152_pixarea_0.839_deg.txt';                 
            end
                        
            [Obj.Map, RA_grid, Dec_grid] = astro.extinction.extinctionGrid(CooGrid,'CooType',Obj.CooType,...
                                                            'Filter',Obj.Filter, 'ExtMap',Obj.MapOrigin);
            Obj.Grid = [RA_grid Dec_grid];

            Result.Status = true;
        end
        %         
        function Result = buildAveragedMap(Obj, Args) 
            % build an averaged extinction map
            % NB: this is a costly function, may take several minutes !! 
            arguments
                Obj
                Args.Rad % [deg] averaging radius
            end
            %
            if ~isempty(Args.Rad)
                Rad = Args.Rad;
            else
                Rad = Obj.AveragingRadius;
            end
            % convert the grid to the ecliptic coordinates
            RAD = 180/pi;
            [lambda, beta] = celestial.coo.convert_coo(Obj.Grid(:,1)/RAD,Obj.Grid(:,2)/RAD,'j2000.0','e');
            lambda = lambda .* RAD; beta = beta .* RAD;            
            % build the map
            fprintf('Please, be patient, averaging may take several minutes...\n')
            Obj.AveragedMap = celestial.grid.statSkyGrid('SkyPos',[lambda beta],'Rad',Rad);
            %
            Result.Status = true;
        end
        %
        function Result = plotMap(Obj, Args)
            % plot the map built before  
            arguments
                Obj
                Args.Averaged = false;
                Args.Limits   = [0, 1];
                Args.FigureNum= 10;
%                 Args.FromApp  = false;
            end
            %
            figure(Args.FigureNum)             
            if Args.Averaged
                plot.ungridded_image(Obj.Grid(:,1), Obj.Grid(:,2), Obj.AveragedMap); caxis(Args.Limits);
                title 'A_{ULTRASAT} (averaged over the given Rad)'
            else
                plot.ungridded_image(Obj.Grid(:,1), Obj.Grid(:,2), Obj.Map); caxis(Args.Limits);
                title 'A_{ULTRASAT}'
            end
            xlabel 'RA, deg'; ylabel 'Dec, deg'
            %
            Result.Status = true;
%             Result.Map = [Obj.Grid Obj.Map]; 
        end
    end    
    %
    methods(Static)
        Result = debug()
            % unitTest

        Result = unitTest()
            % unitTest
    end
end

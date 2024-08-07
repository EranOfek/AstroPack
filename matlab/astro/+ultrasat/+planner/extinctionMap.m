classdef extinctionMap < Component
    % 
    properties (SetAccess = public)
        Grid        
        Map         
        AveragedMap 
        MapOrigin = 'SFD98';
        CooType   = 'j2000.0';
        Filter    = 'ultrasat';
        AveragingRadius = [];    % [deg] (if empty -- do not average) 
    end
    % 
    methods % Obj constructor      
        function Obj = extinctionMap(Args)
            % One line description
            %     Optional detailed description
            % Input  : -
            %          -
            %          * ...,key,val,...
            % Output : -
            % Author : A.M. Krassilchtchikov (2024 Aug)
            % Example:             
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
    methods
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

            Result = true;
        end
        %         
        function Result = buildAveragedMap(Obj, Rad) 
            % get an averaged extinction map
            % NB: this is a costly function, may take several minutes 
            arguments
                Obj
                Rad = 7; % [deg] averaging radius
            end
            % convert the grid to the ecliptic coordinates
            RAD = 180/pi;
            [lambda, beta] = celestial.coo.convert_coo(Obj.Grid(:,1)/RAD,Obj.Grid(:,2)/RAD,'j2000.0','e');
            lambda = lambda .* RAD; beta = beta .* RAD;
            Obj.AveragedMap = celestial.grid.statSkyGrid('SkyPos',[lambda beta],'Rad',Rad);
            %
            Result = true;
        end
        %
        function Result = plotMap(Obj, Args)
            % plot the map built 
            arguments
                Obj
                Args.Averaged = false;
                Args.Limits   = [0, 1];
                Args.FigureNum= 10;
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
            Result = true;
        end
    end    
    %
end

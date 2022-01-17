


classdef Targets < Component
    properties
        Index
        TargetName cell
        IsSolarSystem logical      = false;
        IsTOO logical              = false;
        IsManual logical           = false;
        RA
        Dec
                
        LastJD
        GlobalCounter
        NightCounter
        
        GeoPos                     = [35.041201 30.053014 400];  %
       
        DecRange                   = [-90 90];
        AltLimit                   = 30;
        AzAltLimit                 = [0 30; 90 30; 180 30; 270 30; 360 30];
        SunAltLimit                = -11.5;    % deg
        MoonDistLimit              = [0 0; 0.1 1; 0.2 1; 0.3 1; 0.4 2; 0.5 3; 0.6 5;0.7 10;0.8 15; 0.9 30; 1.0 30];
    end
    
    methods % constructor
        
    end
    
    
    
    
    methods % read/write        
        function Obj = generateTargetList(Obj, List)
            % Generate a Targets object with target list, including LAST default.
            % Input  : - A Targets object.
            %          - Either a table with [RA(deg), Dec(deg), [Name]]
            %            or a character array.
            %            'last' - Default LAST target list.
            % Output : - A populated Targets object.
            % Author : Eran Ofek (Jan 2022)
            % Example: T = celestial.scheduling.Targets
            %          T.generateTargetList('last')
            
            RAD = 180./pi;
            
            if ischar(List)
                % pre defined list
                
                switch lower(List)
                    case 'last'
                        Obj.DecRange        = [-90 90];
                        
                        [TileList,TileArea] = celestial.coo.tile_the_sky(56,42);
                        Obj.RA  = TileList(:,1).*RAD;
                        Obj.Dec = TileList(:,2).*RAD;
                        Ntarget = numel(Obj.RA);
                        
                        Obj.Index = (1:1:Ntarget).';
                        Obj.TargetName = celestial.scheduling.Targets.radec2name(Obj.RA, Obj.Dec);
                        
                        Obj.LastJD         = zeros(Ntarget,1);
                        Obj.GlobalCounter  = zeros(Ntarget,1);
                        Obj.NightCounter   = zeros(Ntarget,1);
                        
                    otherwise
                        error('Unknown List name');
                end
            else
                % List is table of [RA(deg), Dec(deg), [Name]]
               
                Obj.RA  = table2array(List(:,1));
                Obj.Dec  = table2array(List(:,2));
                Ntarget = numel(Obj.RA);
                        
                Obj.Index = (1:1:Ntarget).';
                
                if size(List,2)>2
                    % name is in 3rd column
                    Obj.TargetName = List(:,3);
                else
                    % default names
                    Obj.TargetName = celestial.scheduling.Targets.radec2name(Obj.RA, Obj.Dec);
                end
                
            end
        end
        

    end
    
    methods % visibility
        function [Sun] = sunCoo(Obj, JD)
            %
            % Example: T.generateTargetList('last');
            %          [Sun] = T.sunCoo
            
            arguments
                Obj
                JD       = celestial.time.julday;
            end
            
            RAD = 180./pi;
            
            [RA, Dec,R,SL,EquationTime] = celestial.SolarSys.suncoo(JD, 'a');
            Sun.RA  = RA.*RAD;
            Sun.Dec = Dec.*RAD;
            LST     = celestial.time.lst(JD, Obj.GeoPos(1)./RAD, 'a').*360;  % [deg]
            HA      = LST - RA;
            [Az,Alt]= celestial.coo.hadec2azalt(HA./RAD, Sun.Dec./RAD, Obj.GeoPos(2)./RAD);
            Sun.Az  = Az.*RAD;
            Sun.Alt = Alt.*RAD;
        end
            
        
        function isVisible(Obj, Args)
            %
            
            arguments
                Obj
                Args.JD     = celestial.time.julday;
            end
            
            RAD = 180./pi;
            
            Flag.DecRange = Obj.Dec>=Obj.DecRange(1) & Obj.Dec<=Obj.DecRange(2);
            
            
            
            
            
        end
        
    end
    
    methods (Static)  % static utilities
        function TargetName = radec2name(RA,Dec)
            % given RA/Dec [deg] generate names in cell array %03d+%02d
            
            Ntarget      = numel(RA);
             TargetName  = cell(Ntarget,1);
             for Itarget=1:1:Ntarget
                TargetName{Itarget} = sprintf('%03d%+02d', RA(Itarget), Dec(Itarget));
             end
        end
    end

end
    

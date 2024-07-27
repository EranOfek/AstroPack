% celestial.KDTreeCoo class
%       KDTree for spherical coordinates
%       
% Examples:
% K=celestial.KDTreeCoo;
% VLA=cats.radio.VLASS1;
% K=K.populate(VLA.Catalog(:,1:2))
% [ID,D] = K.coneSearch(1,1,1000);


classdef KDTreeCoo < matlab.mixin.Copyable
    properties 
        
        TreeM   = [];
        TreeK   = [];
        
    end
    
 
    methods
        function Obj = populate(Obj, RA, Dec, Args)
            % populate a cosine direction in KT-Tree
            % Input  : - A celestial.KDTreeCoo object.
            %          - RA or [RA, Dec]
            %          - Dec. If empty, then treat RA as [RA, Dec].
            %          * ...,key,val,...
            %            'InUnits' - Input coordinates units.
            %                   Default is 'rad'.
            %            'Type' - KDTree to populate: 'M'|'K'
            %                   M for matlab built in - will be saved in
            %                   the TreeM property.
            %                   K for KDTree - will be saved in the TreeK
            %                   property.
            %                   If empty, populate both.
            %                   Default is [].
            % Output : - A populated celestial.KDTreeCoo object.
            % Author : Eran Ofek (Jul 2023)
            % Example: K=celestial.KDTreeCoo;          
            %          VLA=cats.radio.VLASS1;
            %          K=K.populate(VLA.Catalog(:,1:2))
            
            arguments
                Obj
                RA
                Dec          = [];
                Args.Type    = []; % empty for both
                Args.InUnits = 'rad';
            end
            
            if isempty(Args.Type)
                Args.Type = {'M','K'};
            end
            
            if isempty(Dec)
                if size(RA,2)==2
                    Dec = RA(:,2);
                    RA  = RA(:,1);
                else
                    error('If Dec is empty, then RA must contain 2 columns');
                end
            end
            
            Conv = convert.angular(Args.InUnits, 'rad');
            RA   = RA.*Conv;
            Dec  = Dec.*Conv;
            
            [CosD1,CosD2,CosD3] = celestial.coo.coo2cosined(RA, Dec);
            CosD = [CosD1(:), CosD2(:), CosD3(:)];
            
            if any(strcmp(Args.Type, 'M'))
                % MATLAB built in KD-Tree
                 Obj.TreeM = KDTreeSearcher(CosD);
            end
            
            if any(strcmp(Args.Type, 'K'))
                % KDTree 
                Obj.TreeK = KDTree(CosD);
            end
            
        end
       
        function [ID, Dist] = coneSearch(Obj, RA, Dec, Radius, Args)
            % Cone search using predefined KDTreeCoo object.
            % Input  : - A celestial.KDTreeCoo object with populated
            %            KDTrees.
            %          - Vector of RA to search.
            %          - Vector of Dec to search.
            %          - Search radius.
            %          * ...,key,val,...
            %            'Type' - search type:
            %                   'M' - use matlab KT tree.
            %                       faster for lare number of coordinate to
            %                       search.
            %                   'K' - use KDTree
            %                   If empty, choose method automatically.
            %                   Default is [].
            %            'RadiusUnits' - Search radius units.
            %                   Default is 'arcsec'.
            %            'InUnits' - RA/Dec coordinate units.
            %                   Default is 'deg'.
            %            'TypeSwitchN' - If auto type, then this is the
            %                   number of coo above to use matlab KD tree.
            %                   Default is 1000.
            % Output : - A cell array of indices of entries found. Each
            %            cell element corresponds to a RA, Dec coordinate.
            %          - A cell array of distances (in cosine direction).
            % Author : Eran Ofek (Jul 2023)
            % Example: K=celestial.KDTreeCoo;          
            %          VLA=cats.radio.VLASS1;
            %          K=K.populate(VLA.Catalog(:,1:2))
            %          [ID,D] = K.coneSearch(1,1,1000);
            
            
            arguments
                Obj
                RA
                Dec                = [];
                Radius             = 10;
                Args.Type          = [];  % use best
                Args.RadiusUnits   = 'arcsec';
                Args.InUnits       = 'deg';
                Args.TypeSwitchN   = 1000;
            end
            
            Ncoo = numel(RA);
            if isempty(Args.Type)
                if Ncoo>Args.TypeSwitchN
                    Args.Type = 'M';
                else
                    Args.Type = 'K';
                end
            end
            
            Conv = convert.angular(Args.InUnits,'rad');
            RA   = RA.*Conv;
            Dec  = Dec.*Conv;
            
            [CosD1, CosD2, CosD3] = celestial.coo.coo2cosined(RA, Dec);
            CosD = [CosD1(:), CosD2(:), CosD3(:)];
            
            RadiusRAD = convert.angular(Args.RadiusUnits, 'rad', Radius);
            
            
            switch lower(Args.Type)
                case 'k'
                    Ncoo = size(CosD,1);
                    ID   = cell(Ncoo,1);
                    Dist = cell(Ncoo,1);
                    for Icoo=1:1:Ncoo
                        [ID{Icoo}, Dist{Icoo}] = ball(Obj.TreeK, CosD(Icoo,:), RadiusRAD);
                    end
                case 'm'
                    [ID, Dist] = rangesearch(Obj.TreeM, CosD, RadiusRAD);                                                     
                
                otherwise
                    error('Unknown KDTree option');
            end
            
        end
    end
    
    
    methods (Static)  % in other files / unitTest
        Result = unitTest
        
        Result = perfTest
    end 

end
    

% ***************************************************************************
% Project     : AstroPack
% Filename    : Backend.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : HEALPix backend wrapper — mirrors Python _HealpyBackend / _AstropyBackend.
%               Wraps celestial.healpix (MEX-accelerated when available).
% ***************************************************************************
classdef Backend < handle
    % Backend  Platform-aware HEALPix backend (MATLAB: celestial.healpix only).
    %
    %   Mirrors Python get_backend() / _HealpyBackend / _AstropyBackend.
    %   All operations use NESTED ordering.

    properties (Constant)
        Name = 'celestial.healpix'
    end

    methods (Static)
        function B = getBackend()
            % Return singleton backend instance (loaded once, reused).
            persistent BackendInstance
            if isempty(BackendInstance)
                BackendInstance = Backend();
            end
            B = BackendInstance;
        end
    end

    methods
        function Pix = ang2pixNested(~, Nside, RaDeg, DecDeg)
            % RA/Dec degrees -> nested pixel index.
            % healpy uses physics convention: theta = colatitude from north pole,
            % phi = azimuth (= RA).  Dec = 90° - theta, so theta = 90° - Dec.
            Pix = celestial.healpix.ang2pix(Nside, RaDeg, DecDeg, ...
                'Type', 'nested', 'CooUnits', 'deg');
            Pix = int64(Pix);
        end

        function PixList = queryDiscNested(~, Nside, RaDeg, DecDeg, RadiusDeg)
            % Return nested pixel indices within RadiusDeg of (RaDeg, DecDeg).
            % inclusive=False equivalent: only pixels whose *centres* lie
            % strictly inside the cone (exclusive mode in MEX coneSearch).
            try
                PixList = celestial.healpix.mex.coneSearch(Nside, RaDeg, DecDeg, ...
                    RadiusDeg, 'exclusive');
            catch
                % MEX not built — use recursive search, then keep only pixels
                % whose centres lie inside the cone (matches inclusive=False).
                Candidates = celestial.healpix.coneSearchRecur(Nside, RaDeg, DecDeg, ...
                    RadiusDeg, 'Type', 'nested', 'CooUnits', 'deg', ...
                    'RadiusUnits', 'deg');
                Candidates = unique(int64(Candidates(:)));
                if isempty(Candidates)
                    PixList = Candidates;
                    return;
                end
                [PixRa, PixDec] = celestial.healpix.pix2ang(Nside, Candidates, ...
                    'Type', 'nested', 'CooUnits', 'deg');
                DistDeg = Backend.sphereDistDeg(RaDeg, DecDeg, PixRa, PixDec);
                PixList = Candidates(DistDeg <= RadiusDeg);
            end
            PixList = int64(PixList(:));
        end

        function PixList = neighboursNested(~, Nside, Pix)
            % Return 8 neighbours + self (nested).  -1 entries removed.
            Neighb = [];
            try
                Neighb = celestial.healpix.mex.neighbors_nested(Nside, int64(Pix));
                Neighb = Neighb(:);
            catch
                try
                    % Pure-MATLAB fallback if MEX not built
                    Neighb = celestial.healpix.findNeighbors(Nside, Pix, ...
                        'IncludeSelf', true);
                    Neighb = Neighb(:);
                catch
                    % findNeighbors can fail at pole/boundary pixels — approximate
                    % the 3x3 neighbour block with a small cone at pixel circumradius.
                    [RaDeg, DecDeg] = celestial.healpix.pix2ang(Nside, Pix, ...
                        'Type', 'nested', 'CooUnits', 'deg');
                    RadiusDeg = rad2deg(sqrt(3) / Nside) * 1.01;
                    Neighb = celestial.healpix.coneSearchRecur(Nside, RaDeg, DecDeg, ...
                        RadiusDeg, 'Type', 'nested', 'CooUnits', 'deg', ...
                        'RadiusUnits', 'deg');
                    Neighb = Neighb(:);
                end
            end
            % healpy uses -1 as a sentinel for missing neighbours (e.g. at poles
            % where a pixel has fewer than 8 neighbours)
            Valid = Neighb(Neighb >= 0);
            % always include the centre pixel itself so the search area is fully covered
            PixList = unique([Valid; int64(Pix)]);
        end

        function [RaDeg, DecDeg] = pix2angNested(~, Nside, Pix)
            % nested pixel -> (ra_deg, dec_deg).
            [RaDeg, DecDeg] = celestial.healpix.pix2ang(Nside, Pix, ...
                'Type', 'nested', 'CooUnits', 'deg');
            RaDeg = double(RaDeg);
            DecDeg = double(DecDeg);
        end
    end

    methods (Static, Access = private)
        function DistDeg = sphereDistDeg(Ra1Deg, Dec1Deg, Ra2Deg, Dec2Deg)
            % Great-circle distance in degrees (haversine, vectorized over Ra2/Dec2).
            Ra1 = deg2rad(Ra1Deg);
            Dec1 = deg2rad(Dec1Deg);
            Ra2 = deg2rad(Ra2Deg);
            Dec2 = deg2rad(Dec2Deg);
            Dlat = Dec2 - Dec1;
            Dlon = Ra2 - Ra1;
            A = sin(Dlat ./ 2).^2 + cos(Dec1) .* cos(Dec2) .* sin(Dlon ./ 2).^2;
            DistDeg = rad2deg(2 .* asin(min(1, sqrt(A))));
        end
    end
end

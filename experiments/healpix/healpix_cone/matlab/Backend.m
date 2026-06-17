% ***************************************************************************
% Project     : AstroPack
% Filename    : Backend.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : HEALPix backend wrapper — mirrors Python _HealpyBackend / _AstropyBackend.
%               Wraps celestial.healpix (MEX-accelerated).
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
            PixList = celestial.healpix.mex.coneSearch(Nside, RaDeg, DecDeg, ...
                RadiusDeg, 'exclusive');
            PixList = int64(PixList(:));
        end

        function PixList = neighboursNested(~, Nside, Pix)
            % Return 8 neighbours + self (nested).  -1 entries removed.
            try
                Neighb = celestial.healpix.mex.neighbors_nested(Nside, int64(Pix));
                Neighb = Neighb(:);
            catch
                % Pure-MATLAB fallback if MEX not built
                Neighb = celestial.healpix.findNeighbors(Nside, Pix, ...
                    'IncludeSelf', true);
                Neighb = Neighb(:);
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
end

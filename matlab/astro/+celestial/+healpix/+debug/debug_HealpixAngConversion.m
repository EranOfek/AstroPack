%==========================================================================
% Project     : AstroPack
% File        : +celestial/+healpix/+debug/debug_HealpixAngConversion.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : Debug mex-backed coordinate conversion in celestial.healpix:
%               ang2pix and pix2ang (nested/ring, deg/rad, UniqueID).
%==========================================================================

function debug_HealpixAngConversion()
    % Smoke-test ang2pix and pix2ang (mex-backed).
    fprintf('\n========== DEBUG HEALPIX ANG CONVERSION ==========\n');

    NSide = 16;
    Lon = [1, 45, 200];
    Lat = [0.5, 30, -10];
    Pix = [0; 197; 500];

    debug_ang2pix(NSide, Lon, Lat);
    debug_pix2ang(NSide, Pix);

    fprintf('========== DEBUG HEALPIX ANG CONVERSION DONE ==========\n');
end


function debug_ang2pix(NSide, Lon, Lat)
    fprintf('\n--- ang2pix ---\n');

    debug_ang2pixCase('nested/rad', NSide, Lon, Lat, 'Type', 'nested', 'CooUnits', 'rad');
    debug_ang2pixCase('ring/rad', NSide, Lon, Lat, 'Type', 'ring', 'CooUnits', 'rad');
    debug_ang2pixCase('nested/deg', NSide, Lon, Lat, 'Type', 'nested', 'CooUnits', 'deg');
    debug_ang2pixCase('nested/UniqueID', NSide, Lon(1), Lat(1), ...
        'Type', 'nested', 'CooUnits', 'rad', 'UniqueID', true);
end


function debug_ang2pixCase(Label, NSide, Lon, Lat, varargin)
    fprintf('  %s: ', Label);
    try
        Pix = celestial.healpix.ang2pix(NSide, Lon, Lat, varargin{:});
        fprintf('ok, numel(Pix)=%d\n', numel(Pix));
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_pix2ang(NSide, Pix)
    fprintf('\n--- pix2ang ---\n');

    debug_pix2angCase('nested/rad', NSide, Pix, 'Type', 'nested', 'CooUnits', 'rad');
    debug_pix2angCase('ring/rad', NSide, Pix, 'Type', 'ring', 'CooUnits', 'rad');
    debug_pix2angCase('nested/deg', NSide, Pix, 'Type', 'nested', 'CooUnits', 'deg');

    try
        UniqueId = celestial.healpix.pix2uniqueId(NSide, Pix(1));
        debug_pix2angCase('nested/UniqueID', NSide, UniqueId, ...
            'Type', 'nested', 'CooUnits', 'rad', 'UniqueID', true);
    catch ME
        fprintf('  nested/UniqueID: failed: %s\n', ME.message);
    end
end


function debug_pix2angCase(Label, NSide, Pix, varargin)
    fprintf('  %s: ', Label);
    try
        [PixLon, PixLat] = celestial.healpix.pix2ang(NSide, Pix, varargin{:});
        fprintf('ok, numel(PixLon)=%d\n', numel(PixLon));
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end

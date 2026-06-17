%==========================================================================
% Project     : AstroPack
% File        : +celestial/+healpix/+debug/debug_HealpixBasics.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : Debug small scalar utilities in celestial.healpix:
%               nPix, nRing, pixRadius, radius2NSide, latitudeRings,
%               pix2uniqueId, uniqueId2pix, pixelSons_nested.
%==========================================================================

function debug_HealpixBasics()
    % Smoke-test basic healpix scalar utilities.
    fprintf('\n========== DEBUG HEALPIX BASICS ==========\n');

    NSide = 16;

    debug_nPix(NSide);
    debug_nRing(NSide);
    debug_pixRadius(NSide);
    debug_radius2NSide();
    debug_latitudeRings(NSide);
    debug_uniqueIdRoundTrip(NSide);
    debug_pixelSons_nested(NSide);

    fprintf('========== DEBUG HEALPIX BASICS DONE ==========\n');
end


function debug_nPix(NSide)
    fprintf('\n--- nPix ---\n');
    try
        Npix = celestial.healpix.nPix(NSide);
        fprintf('ok, Npix=%d\n', Npix);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_nRing(NSide)
    fprintf('\n--- nRing ---\n');
    try
        Nring = celestial.healpix.nRing(NSide);
        fprintf('ok, Nring=%d\n', Nring);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_pixRadius(NSide)
    fprintf('\n--- pixRadius ---\n');
    try
        [PixelRadius, MaxPixRadius] = celestial.healpix.pixRadius(NSide);
        fprintf('ok, PixelRadius=%.6g, MaxPixRadius=%.6g\n', PixelRadius, MaxPixRadius);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_radius2NSide()
    fprintf('\n--- radius2NSide ---\n');
    try
        Radius = 1./206000;
        NSide = celestial.healpix.radius2NSide(Radius);
        fprintf('ok, NSide=%d\n', NSide);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_latitudeRings(NSide)
    fprintf('\n--- latitudeRings ---\n');
    try
        [Lat, NpixPerRing] = celestial.healpix.latitudeRings(NSide);
        fprintf('ok, numel(Lat)=%d, numel(NpixPerRing)=%d\n', numel(Lat), numel(NpixPerRing));
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_uniqueIdRoundTrip(NSide)
    fprintf('\n--- pix2uniqueId / uniqueId2pix ---\n');
    try
        Pix = 42;
        UniqueId = celestial.healpix.pix2uniqueId(NSide, Pix);
        [NsideOut, PixOut] = celestial.healpix.uniqueId2pix(NSide, UniqueId);
        fprintf('ok, UniqueId=%d, NsideOut=%d, PixOut=%d\n', UniqueId, NsideOut, PixOut);

        [NsideAuto, PixAuto] = celestial.healpix.uniqueId2pix([], UniqueId);
        fprintf('ok (auto Nside), NsideAuto=%d, PixAuto=%d\n', NsideAuto, PixAuto);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_pixelSons_nested(NSide)
    fprintf('\n--- pixelSons_nested ---\n');
    try
        PixInd = [0; 1; 100];
        Sons = celestial.healpix.pixelSons_nested(NSide, PixInd);
        fprintf('ok, size(Sons)=[%d %d]\n', size(Sons, 1), size(Sons, 2));
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end

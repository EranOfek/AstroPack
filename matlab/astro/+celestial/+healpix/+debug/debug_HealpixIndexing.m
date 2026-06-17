%==========================================================================
% Project     : AstroPack
% File        : +celestial/+healpix/+debug/debug_HealpixIndexing.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : Debug pure-MATLAB nested indexing and resolution conversion:
%               nest2xyf, xyf2nest, increasePixelResolution,
%               decreasePixelResolution, convertHealPixNsideNested,
%               convertHealPix2highNsideNested.
%==========================================================================

function debug_HealpixIndexing()
    % Smoke-test healpix nested indexing and NSide conversion.
    fprintf('\n========== DEBUG HEALPIX INDEXING ==========\n');

    debug_nest2xyf_xyf2nest();
    debug_increaseDecreasePixelResolution();
    debug_convertHealPixNsideNested();
    debug_convertHealPix2highNsideNested();

    fprintf('========== DEBUG HEALPIX INDEXING DONE ==========\n');
end


function debug_nest2xyf_xyf2nest()
    fprintf('\n--- nest2xyf / xyf2nest ---\n');
    try
        NSide = 8;
        Pix = (0:767).';
        [X, Y, Face] = celestial.healpix.nest2xyf(NSide, Pix);
        PixBack = celestial.healpix.xyf2nest(NSide, X, Y, Face);
        MaxDiff = max(abs(double(Pix) - double(PixBack)));
        fprintf('ok, numel(Pix)=%d, max round-trip diff=%g\n', numel(Pix), MaxDiff);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_increaseDecreasePixelResolution()
    fprintf('\n--- increasePixelResolution / decreasePixelResolution ---\n');
    try
        Ipix0 = [36136; 100];
        Nside0 = 2^7;
        Nside1 = 2^8;

        IpixHigh = celestial.healpix.increasePixelResolution(Ipix0, Nside0, Nside1);
        IpixLow = celestial.healpix.decreasePixelResolution(IpixHigh, Nside1, Nside0);
        fprintf('ok, numel(IpixHigh)=%d, numel(IpixLow)=%d\n', numel(IpixHigh), numel(IpixLow));
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_convertHealPixNsideNested()
    fprintf('\n--- convertHealPixNsideNested ---\n');
    try
        NSide = 16;
        PixID = 1234;
        NewNSide = 4;

        NewPixID = celestial.healpix.convertHealPixNsideNested(NSide, PixID, NewNSide);
        fprintf('ok, NewPixID=%d\n', NewPixID);

        FullID = 4.*NSide.^2 + PixID;
        NewFullID = celestial.healpix.convertHealPixNsideNested([], FullID, NewNSide);
        fprintf('ok (full ID), NewFullID=%d\n', NewFullID);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_convertHealPix2highNsideNested()
    fprintf('\n--- convertHealPix2highNsideNested ---\n');
    try
        NSide = 2^8;
        PixID = 0;
        NewNSide = 2^16;

        [Low, High] = celestial.healpix.convertHealPix2highNsideNested(NSide, PixID, NewNSide);
        fprintf('ok, Low=%d, High=%d\n', Low, High);

        FullID = 4.*NSide.^2 + PixID;
        [LowFull, HighFull] = celestial.healpix.convertHealPix2highNsideNested([], FullID, NewNSide);
        fprintf('ok (full ID), LowFull=%d, HighFull=%d\n', LowFull, HighFull);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end

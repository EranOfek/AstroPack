function tests = test_moments_secondMoment
    % imUtil.sources.moments must return second moments of the SOURCE.
    %   mom2_cube takes the centre as a stamp coordinate, while moment1_cube
    %   is called with RelToCenter=true and returns an offset from the stamp
    %   centre. Passing the offset straight through centres the MaxRadiusM2
    %   disc on the stamp corner, so the moments describe the empty corner
    %   instead of the star and come out at the disc's own value, R^2/4,
    %   for every source (issue #1304).
    tests = functiontests(localfunctions);
end

function [Cube, TrueX2] = makeStamps(Sigma, Nsrc, HalfSize)
    % Gaussian stars centred in odd-sized stamps, no noise.
    N = 2.*HalfSize + 1;
    Cen = (N+1)./2;
    [X, Y] = meshgrid((1:N)-Cen, (1:N)-Cen);
    G = exp(-(X.^2 + Y.^2)./(2.*Sigma.^2));
    G = G./sum(G(:));
    Cube = repmat(single(G), 1, 1, Nsrc);
    TrueX2 = Sigma.^2;
end

function testMomentTracksTheSource(testCase)
    % the returned X2 must follow the width it was given
    HalfSize = 12;
    Sigmas = [1.0 1.4 1.8];
    Got = zeros(size(Sigmas));
    for I = 1:numel(Sigmas)
        [Cube, ~] = makeStamps(Sigmas(I), 4, HalfSize);
        [~, M2] = imUtil.sources.moments(Cube, 'SN',1000, 'Annulus',[10 12], 'AperRadius',[2 4 6]);
        Got(I) = median(M2.X2, 'omitnan');
    end
    % monotonic in the input width, and not pinned to the disc value
    verifyTrue(testCase, all(diff(Got) > 0.1), ...
        sprintf('X2 does not grow with source width: %s', mat2str(Got,3)));
    verifyGreaterThan(testCase, Got(end)./Got(1), 1.5);
end

function testMomentIsNotTheDiscItself(testCase)
    % the failure signature was X2 == MaxRadiusM2^2/4 regardless of the source
    HalfSize = 12;
    MaxRadiusM2 = 6;
    [Cube, TrueX2] = makeStamps(1.2, 4, HalfSize);
    [~, M2] = imUtil.sources.moments(Cube, 'SN',1000, 'Annulus',[10 12], ...
                                     'AperRadius',[2 4 6], 'MaxRadiusM2',MaxRadiusM2);
    X2 = median(M2.X2, 'omitnan');
    verifyLessThan(testCase, X2, 0.5.*MaxRadiusM2.^2./4);       % nowhere near the disc value
    verifyEqual(testCase, X2, TrueX2, 'RelTol',0.15);           % close to the truth
end

function testAgreesWithDirectMexCall(testCase)
    % moments() must place the disc where a direct, correctly-centred
    % mom2_cube call would place it
    HalfSize = 12;
    [Cube, ~] = makeStamps(1.5, 6, HalfSize);
    [M1, M2] = imUtil.sources.moments(Cube, 'SN',1000, 'Annulus',[10 12], 'AperRadius',[2 4 6]);
    % moments() works on the annulus-subtracted cube, so subtract it here too
    [CubeBS,~,~,~] = imUtil.sources.mex.annulus_median(Cube, single([10 12]), 0);
    Cen = (size(CubeBS,1)+1)./2;
    Z = zeros(size(CubeBS,3),1,'single');
    [Direct,~,~] = imUtil.sources.mex.mom2_cube(CubeBS, Z, ...
                       single(M1.StampX1+Cen), single(M1.StampY1+Cen), single(6));
    % tolerance is 1e-3, not exact: moments() lets the mex receive a double
    % centre while this call casts to single first, which can move a pixel on
    % the disc boundary and shifts X2 by ~3e-4 relative.
    verifyEqual(testCase, median(M2.X2,'omitnan'), median(double(Direct),'omitnan'), 'RelTol',1e-3);
end

function testNoNaNOnCleanStamps(testCase)
    % centring on the empty corner produced NaN whenever it held no positive
    % residual; clean centred stamps must never do that
    [Cube, ~] = makeStamps(1.3, 20, 12);
    [~, M2] = imUtil.sources.moments(Cube, 'SN',1000, 'Annulus',[10 12], 'AperRadius',[2 4 6]);
    verifyEqual(testCase, sum(isnan(M2.X2)), 0);
end

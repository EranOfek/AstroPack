function tests = test_radius2NSide
    % Unit tests for celestial.healpix.radius2NSide.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testReturnsPowerOfTwo(testCase)
    % radius2NSide always returns a power of two.
    RadiusList = [1e-6, 1e-4, 0.01, 0.1, 1.0];
    for Radius = RadiusList
        NSide = celestial.healpix.radius2NSide(Radius);
        testCase.verifyEqual(NSide, 2.^round(log2(NSide)), ...
            'AbsTol', 1e-10, ...
            sprintf('NSide=%g is not a power of two for radius=%g', NSide, Radius));
    end
end

function testPixelEnclosesRadius(testCase)
    % Chosen NSide satisfies 1/NSide >= Radius (coarse enclosing bound).
    Radius = 0.05;
    NSide = celestial.healpix.radius2NSide(Radius);
    testCase.verifyGreaterThanOrEqual(1 / NSide, Radius);
end

function testMonotonicWithRadius(testCase)
    % Larger search radius yields smaller (or equal) NSide.
    R1 = 0.001;
    R2 = 0.01;
    NSide1 = celestial.healpix.radius2NSide(R1);
    NSide2 = celestial.healpix.radius2NSide(R2);
    testCase.verifyGreaterThanOrEqual(NSide1, NSide2);
end

function testDocExample(testCase)
    % Doc example: radius2NSide(1/206000) returns a valid power of two.
    NSide = celestial.healpix.radius2NSide(1 / 206000);
    testCase.verifyGreaterThan(NSide, 0);
    testCase.verifyEqual(NSide, 2.^round(log2(NSide)));
end

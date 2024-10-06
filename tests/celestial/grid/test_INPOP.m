function tests = TestINPOP
    % TestINPOP
    % Function-based unit tests for the celestial.INPOP class.
    % This function returns a test suite containing multiple test functions,
    % each of which tests a different aspect of the INPOP ephemeris handling.
    %
    % Author: Yarin Shani
    % Date: 2024-09-07
    %
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testDownloadConstants(testCase)
    % Test downloading and loading constants for the INPOP class.
    I = celestial.INPOP();
    
    % Verify constants are loaded correctly
    verifyNotEmpty(testCase, I.Constant, 'Could not load constants.');
end

function testPopulateTables(testCase)
    % Test populating the position tables for Sun and Earth.
    I = celestial.INPOP();
    I = I.populateTables({'Sun', 'Ear'}, 'FileType', 'asc');
    
    % Verify the tables for Sun and Earth are populated
    verifyNotEmpty(testCase, I.PosTables.Sun, 'Sun position table is empty.');
    verifyNotEmpty(testCase, I.PosTables.Ear, 'Earth position table is empty.');
end

function testPositionComparisonWithVSOP87(testCase)
    % Test comparing INPOP position with VSOP87 for Earth.
    I = celestial.INPOP();
    JD = mean(I.RangeShort); % Mean Julian Date
    
    % Calculate positions from INPOP and VSOP87
    PosINPOP = getPos(I, 'Ear', JD);
    PosVSOP87 = celestial.SolarSys.calc_vsop87(JD, 'Earth', 'e', 'E');
    
    % Verify the positions are similar within tolerance
    tolerance = 1e-3;
    verifyLessThan(testCase, mean(abs(PosINPOP - PosVSOP87)), tolerance, ...
        'INPOP and VSOP87 positions do not agree.');
end

function testVelocityComparisonWithVSOP87(testCase)
    % Test comparing INPOP velocity with VSOP87 for Jupiter.
    I = celestial.INPOP();
    JD = mean(I.RangeShort); % Mean Julian Date
    
    % Calculate velocities from INPOP and VSOP87
    VelINPOP = getPos(I, 'Jup', JD, 'IsPos', false);
    [~, VelVSOP87] = celestial.SolarSys.calc_vsop87(JD, 'Jupiter', 'e', 'E');
    
    % Verify the velocities are similar within tolerance
    tolerance = 1e-3;
    verifyLessThan(testCase, mean(abs(VelINPOP - VelVSOP87)), tolerance, ...
        'INPOP and VSOP87 velocities do not agree.');
end

function testPopulateAllObjects(testCase)
    % Test populating tables and calculating positions for all celestial objects.
    I = celestial.INPOP();
    Objects = {'Sun', 'Mer', 'Ven', 'Ear', 'EMB', 'Moo', 'Mar', 'Jup', 'Sat', 'Ura', 'Nep', 'Plu', 'Lib'};
    JD = mean(I.RangeShort); % Mean Julian Date
    Passed = true;
    
    % Loop through all objects and attempt to populate and calculate position
    for i = 1:numel(Objects)
        try
            I.populateTables(Objects{i});
            try
                PosINPOP = getPos(I, Objects{i}, JD); %#ok<NASGU>
            catch
                disp(['Did not calculate position of ', Objects{i}]);
                Passed = false;
            end
        catch
            disp(['Did not populate ', Objects{i}]);
            Passed = false;
        end
    end
    
    % Verify all objects were successfully populated
    verifyTrue(testCase, Passed, 'Failed in populating and calculating objects.');
end

function testTimeSpanOption(testCase)
    % Test INPOP ephemeris calculation using the TimeSpan option.
    JD = 2460000.1 + (0:0.1:100)'; % Generate Julian Dates
    I = celestial.INPOP();
    
    % Populate ephemeris over a specified time span
    I.populateAll('TimeSpan', [2459000, 2461000], 'PopForce', true);
    xx = I.getPos('Ear', JD);
    
    % Populate all positions and check consistency
    I.populateAll('PopForce', true);
    xn = I.getPos('Ear', JD);
    
    % Verify the positions are consistent within machine precision
    verifyLessThan(testCase, max(abs(xx - xn), [], 'all'), eps, ...
        'INPOP failed to evaluate ephemeris when TimeSpan option is used.');
end

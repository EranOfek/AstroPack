function tests = test_plot
    % Smoke test for celestial.healpix.plot (graphics).

    tests = functiontests(localfunctions);
end

%% Test Functions

function testPlotSmokeInvisibleFigure(testCase)
    % plot() runs without error and closes the figure it creates.
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);
    celestial.healpix.HealpixTestHelper.assumeFunctionExists(testCase, 'plot.skyCircles');

    % Mapping Toolbox axesm is required by plot when PlotOnMap=true.
    if exist('axesm', 'file') ~= 2
        testCase.assumeFail('Skipping: axesm (Mapping Toolbox) is not available.');
    end

    Pix = [181313, 181316, 133256];
    NSide = 128;
    Fig = figure('Visible', 'off');
    try
        celestial.healpix.plot(Pix, 'Nside', NSide, 'PlotOnMap', true);
        testCase.verifyTrue(isvalid(Fig));
    finally
        if isvalid(Fig)
            close(Fig);
        end
    end
end

function testPlotFlatMode(testCase)
    % PlotOnMap=false uses flat axes (no axesm).
    celestial.healpix.HealpixTestHelper.assumeCoreAngPixMex(testCase);
    celestial.healpix.HealpixTestHelper.assumeFunctionExists(testCase, 'plot.skyCircles');

    Pix = 100;
    NSide = 64;
    Fig = figure('Visible', 'off');
    try
        celestial.healpix.plot(Pix, 'Nside', NSide, 'PlotOnMap', false);
        testCase.verifyTrue(isvalid(Fig));
    finally
        if isvalid(Fig)
            close(Fig);
        end
    end
end

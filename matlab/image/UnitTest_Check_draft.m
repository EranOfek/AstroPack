% Eran - if you use this file, please write here a comment
%
%

% Unit testing using matlab plugins
% Produces code coverage reports
% 
% Usage:
% cd $ASTROPACK/matlab/image/
% import matlab.unittest.TestSuite
% import matlab.unittest.TestRunner
% import matlab.unittest.plugins.CodeCoveragePlugin
% suite=TestSuite.fromFile("UnitTest.m")
% runner = TestRunner.withTextOutput;
% runner.addPlugin(CodeCoveragePlugin.forFolder(pwd))
% result = runner.run(suite);
%
% Or:
% cd $ASTROPACK/matlab/image
% UnitTest.unitTest
% commented lines mean unit tests do not run under certain circumstances

classdef UnitTest < matlab.unittest.TestCase
    methods (Test)
        function Result = unit_Test(testCase)
%             AstroCatalog.unitTest
%             AstroDb.unitTest  
            AstroHeader.unitTest
            AstroImage.unitTest
%             AstroPSF.unitTest
            AstroTable.unitTest
%             AstroWCS.unitTest
%             BackImage.unitTest
            BaseAlgo.unitTest
%             BaseImage.unitTest
            CatAlgo.unitTest
            DbInfo.unitTest
%             ds9.unitTest
            FITS.unitTest
            ImageAlgo.unitTest
            ImageComponent.unitTest
%             ImageIO.unitTest
            ImagePath.unitTest
            ImageProc.unitTest
            MaskImage.unitTest
            MatchedSources.unitTest
            PhotonsList.unitTest
            Rect.unitTest
            SciImage.unitTest
            Tran2D.unitTest
            VarImage.unitTest
            VirtImage.unitTest
            VirtImageManager.unitTest
            
            Result = true;
            testCase.verifyEqual(Result, true)
        end
    end
    
    methods(Static) 
        function unitTest()
            %set your own directory
            cd ~/software/matlab/AstroPack/matlab/image/
            import matlab.unittest.TestSuite
            import matlab.unittest.TestRunner
            import matlab.unittest.plugins.CodeCoveragePlugin
            suite=TestSuite.fromFile("UnitTest.m")
            runner = TestRunner.withTextOutput;
            runner.addPlugin(CodeCoveragePlugin.forFolder(pwd))
            runner.run(suite);
        end
    end
end


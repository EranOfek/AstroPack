% Top-Level Unit Tester
%
% Usage:
%
%
%

% #functions
% UnitTester -
% beforePush (Static) - Call to perform tests before git push - PUSH ONLY IF ALL TESTS PASS
% doBeforePush - Run all required tests before 'git push' command DO NOT PUSH if there are failed tests!
% doPerfTest - Run all Performance tests
% doStressTest - Run all Stbveress tests
% doTest - Run all unit-tests and show report
% getTestFits (Static) - Return FITS file name and size from our test data folder
% isTested - Check if already tested
% msgLog - Write message to log
% msgStyle - Write message to log
% perfTest (Static) - Run all Performance tests
% report - Show report of all performed tests
% runFile -
% runFolder - Load specify folder to properties Note: Recursive function
% runTest - Run single unit test
% setup - Class setup
% stressTest (Static) - Run all Stress tests
% test (Static) -
% testAll - Run all unit-test functions found in MATLAB source files
% testCore - Run core unit-tests, required before we can run any other
% testImage - Run image related unit-tests
% #/functions
%
classdef UnitTester < handle

    % Properties
    properties (SetAccess = public)
        PassCount = 0       % Number of passed tests
        FailCount = 0       % Number of failed tests
        TestList = {}       % List of all tests performed
        TestResult = {}     % List of tests results
        MyFileName = ''     % Name of this source file
        AstroPackPath = ''  % Git repository root folder
        SourcePath = ''     % Source path
    end

    %--------------------------------------------------------
    methods % Constructor
        function Obj = UnitTester()
            Obj.setup();
        end
    end


    methods % Main functions

        function Result = setup(Obj)
            % Class setup

            % Clear all persistent objecst and java spaces, requried
            % to the the tests with clean workspace
            % Clear java is required because we use jave classes
            % for yaml, databases, etc.
            
            % This cause problems, should be moved to static function
            %clear all;
            %clear java;

            % Get full path and name of the file in which the call occurs,
            % not including the filename extension
            Obj.MyFileName = mfilename('fullpath');
            [MyPath, ~, ~] = fileparts(Obj.MyFileName);
            Obj.SourcePath = fullfile(MyPath, '..');

            % Get MATLAB source code root folder
            Obj.AstroPackPath = getenv('ASTROPACK_PATH');
            io.msgLog(LogLevel.Test, 'ASTROPACK_PATH: %s', Obj.AstroPackPath);
            if tools.os.iswindows()
                Obj.SourcePath = fullfile(Obj.AstroPackPath, 'matlab');

                % Temporary for backwards compatibility
                if ~isfolder(Obj.SourcePath)
                    Obj.SourcePath = 'D:\Ultrasat\AstroPack.git\matlab\';
                end
            else
                Obj.SourcePath = fullfile(Obj.AstroPackPath, 'matlab');

                % Temporary for backwards compatibility
                if ~isfolder(Obj.SourcePath)
                    Obj.SourcePath = '/home/eran/matlab/AstroPack/matlab/';
                end
            end

            io.msgLog(LogLevel.Test, 'SourcePath: %s', Obj.SourcePath);
            Result = true;
        end


        function Result = doTest(Obj)
            % Run all unit-tests and show report

            Obj.msgLog(LogLevel.Test, 'Started\n');
            try
                Result = Obj.testAll();
            catch
                Result = false;
                Obj.msgStyle(LogLevel.Error, '@error', 'unitTest: Exception');
            end
            Obj.report();
            Obj.msgLog(LogLevel.Test, 'Done\n');
        end


        function Result = report(Obj)
            % Show report of all performed tests

            % Print passed tests
            Obj.msgLog(LogLevel.Test, '\n');
            Obj.msgLog(LogLevel.Test, 'Passed:');
            for i = 1:numel(Obj.TestList)
                Passed = Obj.TestResult{i};
                if Passed
                    Obj.msgStyle(LogLevel.Test, 'blue', 'Test: %s - PASSED', Obj.TestList{i});
                else
                    %Obj.msgStyle(LogLevel.Test, 'red', 'Test: %s - FAILED', Obj.TestList{i});
                end
            end

            % Print failed tests
            Obj.msgLog(LogLevel.Test, '\n');
            Obj.msgLog(LogLevel.Test, 'Failed:');
            for i = 1:numel(Obj.TestList)
                Passed = Obj.TestResult{i};
                if Passed
                    %Obj.msgStyle(LogLevel.Test, 'blue', 'Test: %s - PASSED', Obj.TestList{i});
                else
                    Obj.msgStyle(LogLevel.Test, 'red', 'Test: %s - FAILED', Obj.TestList{i});
                end
            end

            % Totals
            Obj.msgLog(LogLevel.Test, '\n');
            Obj.msgLog(LogLevel.Test, 'Passed: %d', Obj.PassCount);
            Obj.msgLog(LogLevel.Test, 'Failed: %d', Obj.FailCount);
            Result = true;
        end


        function Result = doBeforePush(Obj)
            % Run all required tests before 'git push' command
            % DO NOT PUSH if there are failed tests!

            % Here for Debug only!
            % Result = Obj.processFolder(Obj.SourcePath);
            
            % Run core unit-tests
            Result = Obj.testCore();

            % Run image-related unit-tests
            Result = Obj.testImage();

            % Run all other tests
            % Result = Obj.processFolder(Obj.SourcePath);
            
            % Show report
            Obj.report();

            if Obj.FailCount == 0
                Obj.msgStyle(LogLevel.Test, '@pass', 'BeforePush passed, ready to git push');
            else
                Obj.msgStyle(LogLevel.Test, '@failed', 'BeforePush FAILED - DO NOT PUSH !!!');
            end
        end


        function Result = doPerfTest(Obj)
            % Run all Performance tests

            % Image
            AstroImage.perfTest();
            AstroCatalog.perfTest();

            % Database/Files
            db.DbQuery.perfTest();
            db.AstroDb.perfTest();
            db.AstroStore.perfTest();

            Result = true;
        end


        function Result = doStressTest(Obj)
            % Run all Stress tests

            db.DbQuery.stressTest();
            db.AstroDb.stressTest();
            db.AstroStore.stressTest();

            Result = true;
        end

    end


    methods % Test functions

        function Result = testAll(Obj)
            % Run all unit-test functions found in MATLAB source files

            % First run core unit-tests, required before any other tests
            Result = Obj.testCore();

            % Run unit-tests in .m files
            Result = Obj.processFolder(Obj.SourcePath);

            % Show report
            Obj.report();
        end


        function Result = testCore(Obj)
            % Run core unit-tests, required before we can run any other
            Obj.msgLog(LogLevel.Test, 'UnitTester.testCore started\n');
            Result = false;

            % Set maximum log level
            MsgLogger.setLogLevel(LogLevel.All);

            % Must be tested in THIS SPECIFIC ORDER!

            % Test classes that does not require Configuration
            Obj.runTest('LogFile');
            Obj.runTest('MsgLogger');
            Obj.runTest('Base');

            % Test Configuration, it will be loaded
            Obj.runTest('Configuration');

            % From this point Configuration is loaded
            Obj.runTest('ComponentMap');
            Obj.runTest('Component');

            % Database
            TestDb = false;
            if TestDb
                Obj.runTest('io.db.DbDriver');
                Obj.runTest('io.db.DbConnection');
                Obj.runTest('io.db.DbRecord');
                Obj.runTest('io.db.DbQuery');
            else
                Obj.msgLog(LogLevel.Test, '\nSkipped Database testing\n');
            end

            Obj.msgLog(LogLevel.Test, '\nUnitTester.testCore done');
            Result = true;
        end


        function Result = testImage(Obj)
            % Run image related unit-tests

            Obj.msgLog(LogLevel.Test, 'UnitTester.testImage started\n');
            Result = false;

            Obj.runTest('VirtImage');
            Obj.runTest('VirtImageManager');
            Obj.runTest('ImageComponent');
            Obj.runTest('Dictionary');
            Obj.runTest('BitDictionary');

            Obj.runTest('AstroHeader');
            Obj.runTest('AstroCatalog');
            Obj.runTest('AstroTable');
            Obj.runTest('AstroImage');
            Obj.runTest('AstroPSF');
            Obj.runTest('AstroWCS');
            Obj.runTest('Tran2D');
            Obj.runTest('VarImage');
            Obj.runTest('BackImage');
            %Obj.runTest('DbInfo');			% Chen: There is no such file @Eran - Do we need it?
            Obj.runTest('ds9');
            Obj.runTest('FITS');
            Obj.runTest('ImageComponent');
            Obj.runTest('ImageIO');
            Obj.runTest('ImagePath');
            Obj.runTest('ImageProc');
            Obj.runTest('MaskImage');
            Obj.runTest('MatchedSources');
            Obj.runTest('PhotonsList');
            Obj.runTest('Rect');
            Obj.runTest('SciImage');

            TestDb = false;
            if TestDb
                Obj.runTest('AstroCatalogDb');
                Obj.runTest('AstroImageDb');
            end

            Obj.msgLog(LogLevel.Test, '\nUnitTester.testImage done');
            Result = true;
        end


        function Result = runTest(Obj, Target, Args)            
            % Run single unit test, Target is class name
            % Example: runTest('ImagePath')
            arguments
                Obj
                Target
                Args.FuncName = '';
            end
            
            Result = false;

            if isempty(Args.FuncName)
                UnitTestFunc = [Target, '.unitTest()'];
            else
                Target = Args.FuncName;
                UnitTestFunc = [Args.FuncName, '()'];
            end
            
            % Check if already tested
            if any(strcmp(Obj.TestList, Target))
                Obj.msgLog(LogLevel.Test', 'already tested: %s', Target);
                Result = false;
                return
            end

            % Save current dir
            PWD = pwd;
            try
                % Call function
                Obj.msgStyle(LogLevel.Info, 'blue', 'runTest: %s', UnitTestFunc);
                Result = eval(UnitTestFunc);
            catch
                Obj.msgStyle(LogLevel.Error, '@error', 'runTest exception: ');
            end

            % Update lists
            Obj.TestList{end+1} = Target;
            Obj.TestResult{end+1} = Result;

            % Update totals
            if Result
                Obj.PassCount = Obj.PassCount + 1;
                Obj.msgStyle(LogLevel.Test, '@passed', 'runTest PASSED: %s', UnitTestFunc);
            else
                Obj.msgStyle(LogLevel.Error, '@error', 'runTest FAILED: %s', UnitTestFunc);
                Obj.FailCount = Obj.FailCount + 1;
            end

            % Restore current dir
            cd(PWD);
        end


        function Result = processFolder(Obj, Path)
            % Recursivly call processFile()

            %Obj.msgLog(LogLevel.Test, 'UnitTester.processFolder: %s', Path);

            % Scan all .m files in folder
            List = dir(fullfile(Path, '*'));
            for i = 1:length(List)
                fname = List(i).name;

                % Folder recursion
                if List(i).isdir
                    if fname ~= "." && fname ~= ".."
                        FolderName = fullfile(List(i).folder, List(i).name);
                        Obj.processFolder(FolderName);
                    end

                % Process .m file
                else
                    [path, name, ext] = fileparts(fname);
                    if ext == ".m"
                        FileName = fullfile(List(i).folder, List(i).name);
                        Obj.processFile(FileName);
                    end
                end
            end

            Result = true;
        end



        function Result = processFile(Obj, FileName)

            %Obj.msgLog(LogLevel.Test, 'UnitTester.processFile: %s', FileName);

            fn = lower(FileName);
            if contains(fn, 'obsolete') || contains(fn, 'unused')
                Result = false;
                return;
            end
                
            % Skip self
            %MyFileName = mfilename('fullpath');                        
            [filepath, name, ext] = fileparts(FileName);
            [filepath, name, ext] = fileparts(filepath);
            
            % Read file to Lines{}
            Lines = Obj.readFile(FileName);
            
            if startsWith(name, '@')
                ClassName = name(2:end);
                fname = fullfile(filepath, ClassName, 'unitTest.m');
                if isfile(fname)
                    %Result = Obj.processClassFile(Obj, FileName);
                    Result = Obj.runTest(ClassName);
                end
            else
                ClassName = Obj.getClassName(Lines);
                if ~isempty(ClassName)
                    Result = Obj.runTest(ClassName);
                else
                    Result = Obj.processNonClassFile(FileName);                
                end
            end
            
            return;

            % Check if class folder
            fname = split(FileName);
            
            % Read file to Lines{}
            Lines = Obj.readFile(FileName);

            % Search classdef
            ClassName = '';
            for i=1:length(Lines)
                % Check that it is not a comment
                Line = Lines{i};

                items = split(Line, '%');
                items = split(items{1}, 'classdef ');
                if length(items) > 1
                    Obj.msgLog(LogLevel.Test, 'Found classdef: %s', Line);
                    items = split(items{2}, ' ');
                    %if items{1} == "classdef "
                        ClassName = items{1};
                        break;
                    %end
                end
            end

            % Found classdef
            if ~isempty(ClassName)

                % Search unitTest() function
                haveUnitTest = false;
                for i=1:length(Lines)

                    % Check that it is not a comment
                    Line = Lines{i};

                    items = split(Line, '%');
                    items = split(items{1}, 'function Result = unitTest()');
                    
                    % unitTest in separate file
                    items2 = split(items{1}, 'Result = unitTest()');
                    
                    if length(items) > 1 || length(items2) > 1
                        %if lower(items{1}) == 'unitTest('
                            Obj.msgLog(LogLevel.Test, 'Found unitTest(): %s', Line);
                            haveUnitTest = true;
                            break;
                        %end
                    end
                end

                % unitTest() function found
                if haveUnitTest
                    % Call unitTest
                    [MyPath, ~, ~] = fileparts(FileName);
                    MyPath = [MyPath, filesep];

                    ClassName = strrep(FileName, '.m', '');
                    ClassName = strrep(ClassName, MyPath, '');
                    ClassName = strrep(ClassName, '+', '.');

                    if ~strcmp(ClassName, 'UnitTester')
                        Result = Obj.runTest(ClassName);
                    end
                else
                end

            % classdef not found, single function file?
            else

                % Search unitTest() function
                haveUnitTest = false;
                haveFunc = false;
                for i=1:length(Lines)

                    % Check that it is not a comment
                    Line = Lines{i};

                    items = split(Line, '%');
                    items = split(items{1}, 'function Result = unitTest()');
                    if length(items) > 1 || length(items2) > 1
                        %if lower(items{1}) == 'unitTest('
                            Obj.msgLog(LogLevel.Test, 'Found unitTest(): %s', Line);
                            haveFunc = true;
                            haveUnitTest = true;
                            break;
                        %end
                    end
                end

                
                if haveFunc && haveUnitTest
                    % Call unitTest
                    Func = PackageName + ".unitTest();"';
                    %Result = eval(Func);
                    %Result = (ClassName).unitTest();
                else
                end

            end
        end
        
        
        function Result = processClassFile(Obj, FileName)
            % Process .m file which is a class file
            Result = false;
        end
        
        
        function Result = processNonClassFile(Obj, FileName)
            % Process .m file which is not part of class (i.e. package)            

            Result = false;
            PackageName = Obj.getPackageName(FileName);
            
            % Read file to Lines{}
            Lines = Obj.readFile(FileName);

            % Search unitTest() function
            haveUnitTest = false;
            FuncName = '';
            for i=1:length(Lines)

                % Check that it is not a comment
                Line = Lines{i};
                FuncName = Obj.getFunctionName(Line);
                if strcmp(FuncName, 'unitTest')
                    haveUnitTest = true;
                    break;
                end
            end


            if haveUnitTest
                % Call unitTest
                if isempty(PackageName)
                    Obj.runTest('', 'FuncName', FuncName);
                else
                    Func = [PackageName, '.', FuncName];
                    Obj.runTest('', 'FuncName', Func);
                end
            else
            end
            
        end
       
        
        function Result = readFile(Obj, FileName)
            % Read file to Lines{}
            
            fid = fopen(FileName);
            Line = fgetl(fid);
            Lines = cell(0, 1);
            while ischar(Line)
                Lines{end+1, 1} = Line;
                Line = fgetl(fid);
            end
            fclose(fid);
            Result = Lines;
        end
        
        
        function Result = getClassName(Obj, Lines)
            % Search classdef
            
            ClassName = '';
            for i=1:length(Lines)
                % Check that it is not a comment
                Line = Lines{i};

                items = split(Line, '%');
                items = split(items{1}, 'classdef ');
                if length(items) > 1
                    Obj.msgLog(LogLevel.Test, 'Found classdef: %s', Line);
                    items = split(items{2}, ' ');
                    %if items{1} == "classdef "
                        ClassName = items{1};
                        break;
                    %end
                end
            end
            Result = ClassName;
        end
        
        
        function Result = getPackageName(Obj, FileName)
            % Get package name from file name
            Result = '';            
            if (contains(FileName, '+'))
                FileName = strrep(FileName, '\', '/');
                List = split(FileName, '/');
                for i=1:numel(List)
                    if startsWith(List{i}, '+')
                        if isempty(Result)
                            Result = List{i}(2:end);
                        else
                            Result = [Result, '.', List{i}(2:end)];
                        end
                    end
                end
            end
        end
        
        
        function Result = getFunctionName(Obj, Line)
            % 'function Result = unitTest()'
            
            Result = '';
            Items = split(Line, '%');
            Items = split(Items{1}, 'function');
            if numel(Items) > 1
                Items = split(Items{2}, '=');
                if numel(Items) > 1
                    Items = split(strip(Items{2}), '(');
                    Result = strip(Items{1});
                end
            end
        end
        
        
        function Result = isTested(Obj, Target)
            % Check if already tested by inspecting at Obj.TestList

            if any(strcmp(Obj.TestList, Target))
                Result = true;
            else
                Result = false;
            end
        end


        function msgLog(Obj, Level, varargin)
            % Write message to log
            io.msgLog(Level, varargin{:});
        end


        function msgStyle(Obj, Level, Style, varargin)
            % Write message to log
            io.msgStyle(Level, Style, varargin{:});
        end
    end


    %----------------------------------------------------------------------
    % Unit test
    methods (Static)

        function [FileName, FileSize] = getTestFits(Index)
            % Return FITS file name and size from our test data folder
            DataSampleDir = tools.os.getTestDataDir;
            List = dir(fullfile(DataSampleDir, '*.fits'));
            assert(~isempty(List));
            [~, IndexList] = sort(string({List.name}), 2, 'ascend');
            FileName = '';
            FileSize = 0;
            Idx = 0;
            for i = IndexList % 1:length(List)
                if ~List(i).isdir
                    Idx = Idx + 1;
                    if Idx == Index
                        FileName = fullfile(List(i).folder, List(i).name);
                        FileSize = List(i).bytes;
                        break;
                    end
                end
            end
        end
    end

    %----------------------------------------------------------------------
    % Unit test
    methods(Static)

        function Result = unitTest()
            Result = true;
        end
        
        function Result = test()
            Tester = UnitTester;
            Result = Tester.doTest();
        end


        function Result = beforePush()
            % Call to perform tests before git push - PUSH ONLY IF ALL TESTS PASS
            
            %
            clear all;
            clear java;
            
            Tester = UnitTester;
            Result = Tester.doBeforePush();
        end


        function Result = perfTest()
            % Run all Performance tests
            Tester = UnitTester;
            Result = Tester.doPerfTest();
        end


        function Result = stressTest()
            % Run all Stress tests
            Tester = UnitTester;
            Result = Tester.doStressTest();
        end

    end

end

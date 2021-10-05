% Top-Level Unit Tester
%
% Usage:
%

classdef UnitTester < handle

    % Properties
    properties (SetAccess = public)
        PassCount = 0       % Number of passed tests
        FailCount = 0       % Number of failed tests
        TestList = {}       % List of all tests performed
        TestResult = {}     % List of tests results
        WarnList = {}       % Warnings list
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
            % Class setup, prepare path to source code

            % Clear all persistent objecst and java spaces, requried
            % to the the tests with clean workspace
            % Clear java is required because we use jave classes
            % for yaml, databases, etc.
            
            % clear(s) moved to a static function, doing it here removes
            % self object from the workspace
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
            % Print report of all performed tests

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
           
            % Warnings
            Obj.msgLog(LogLevel.Test, '\n');
            Obj.msgLog(LogLevel.Test, 'Warnings: %d', numel(Obj.WarnList));
            for i = 1:numel(Obj.WarnList)
                Obj.msgStyle(LogLevel.Warning, 'red', '%s', Obj.WarnList{i});                
            end

            Result = true;
        end


        function Result = doBeforePush(Obj)
            % Run all required tests before 'git push' command
            % WARNING: DO NOT PUSH to common branch (currently: 'dev1') if there are failed tests!

            % Here for Debug only!
            % Result = Obj.processFolder(Obj.SourcePath);
            
            % Run core unit-tests
            Result = Obj.testCore();

            % Run image-related unit-tests
            Result = Obj.testImage();

            % Run tests by scanning source files
            Result = Obj.processFolder(Obj.SourcePath);
            
            % Print report
            Obj.report();

            % Print final summary
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

            % Run tests by scanning source files
            Result = Obj.processFolder(Obj.SourcePath, 'FuncName', 'perfTest');
        end


        function Result = doStressTest(Obj)
            % Run all Stress tests

            db.DbQuery.stressTest();
            db.AstroDb.stressTest();
            db.AstroStore.stressTest();

            % Run tests by scanning source files
            Result = Obj.processFolder(Obj.SourcePath, 'FuncName', 'stressTest');
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


        function Result = runTest(Obj, ClassName, Args)            
            % Run single unit test, Target is class name
            % Example: runTest('ImagePath')
            arguments
                Obj
                ClassName                          %
                Args.FuncName = 'unitTest';     % Function name
            end
            
            Result = false;

            if ~isempty(ClassName) && ~isempty(Args.FuncName)
                Func = [ClassName, '.', Args.FuncName, '()'];
            else
                Func =  [Args.FuncName, '()'];
            end
            
            % Check if already tested
            if any(strcmp(Obj.TestList, Func))
                Obj.msgLog(LogLevel.Test', 'already tested: %s', Func);
                Result = false;
                return
            end

            % Save current dir
            PWD = pwd;
            try
                % Call function
                Obj.msgStyle(LogLevel.Info, 'blue', 'runTest: %s', Func);
                Result = eval(Func);
            catch
                Obj.msgStyle(LogLevel.Error, '@error', 'runTest exception: ');
            end

            % Update lists
            Obj.TestList{end+1} = Func;
            Obj.TestResult{end+1} = Result;

            % Update totals
            if Result
                Obj.PassCount = Obj.PassCount + 1;
                Obj.msgStyle(LogLevel.Test, '@passed', 'runTest PASSED: %s', Func);
            else
                Obj.msgStyle(LogLevel.Error, '@error', 'runTest FAILED: %s', Func);
                Obj.FailCount = Obj.FailCount + 1;
            end

            % Restore current dir
            cd(PWD);
        end


        function Result = processFolder(Obj, Path, Args)
            % Recursivly call processFile()
            arguments
                Obj
                Path                            %
                Args.FuncName = 'unitTest';     % Function name
                Args.Required = true;
            end
            

            %Obj.msgLog(LogLevel.Test, 'UnitTester.processFolder: %s', Path);

            % Scan all .m files in folder
            List = dir(fullfile(Path, '*'));
            for i = 1:length(List)
                fname = List(i).name;

                % Folder recursion
                if List(i).isdir
                    if fname ~= "." && fname ~= ".."
                        Name = List(i).name;
                        FolderName = fullfile(List(i).folder, Name);
            
                        % Skip some folders
                        if ~Obj.shouldProcessFile(FolderName)
                            continue;
                        end
                        
                        if startsWith(Name, '@')
                            FileName = fullfile(FolderName, [Name(2:end), '.m']);
                            if isfile(FileName)
                                Obj.processFile(FileName, 'FuncName', Args.FuncName, 'Required', Args.Required);
                            else
                                Obj.msgStyle(LogLevel.Warning, 'red', 'Empty class folder: %s', FolderName);
                                Obj.Warn(['Empty class folder: ', FolderName]);
                            end
                        else
                            Obj.processFolder(FolderName, 'FuncName', Args.FuncName, 'Required', Args.Required);
                        end
                    end

                % Process .m file
                else
                    [path, name, ext] = fileparts(fname);
                    if ext == ".m"
                        FileName = fullfile(List(i).folder, List(i).name);
                        Obj.processFile(FileName, 'FuncName', Args.FuncName, 'Required', Args.Required);
                    end
                end
            end

            Result = true;
        end


        function Result = processFile(Obj, FileName, Args)
            arguments
                Obj
                FileName                        %
                Args.FuncName = 'unitTest';     % Function name
                Args.Required = true;
            end
            
            %Obj.msgLog(LogLevel.Test, 'UnitTester.processFile: %s', FileName);
            
            % Skip non-active files
            if ~Obj.shouldProcessFile(FileName)
                Result = false;
                return;
            end
                
            % 
            PackageName = Obj.getPackageName(FileName);
            [ClassName, ClassFolder] = Obj.getClassName(FileName);
            
            % If this is a class folder, look for file with the specified function name
            if ~isempty(ClassName)
                FuncFileName = fullfile(ClassFolder, [Args.FuncName, '.m']);
                if isfile(FuncFileName)
                    ClassName = Obj.fullName(PackageName, ClassName);
                    Result = Obj.runTest(ClassName,  'FuncName', Args.FuncName);
                else                    
                    % No specific file, we may have the function in the main class file
                    Lines = Obj.readFile(FileName);
                    FuncName = Obj.findFunction(Lines, Args.FuncName);
                    ClassName = Obj.fullName(PackageName, ClassName);
                    if ~isempty(FuncName)                        
                        Result = Obj.runTest(ClassName, 'FuncName', FuncName);
                    elseif Args.Required
                        Obj.msgStyle(LogLevel.Warning, 'red', 'Missing class function: %s - %s()', ClassName, Args.FuncName);
                        Obj.Warn(['Missing class function: ', ClassName, ' - ', Args.FuncName, '()']);
                    end                    
                end
            else
                % Read file to Lines{}
                Lines = Obj.readFile(FileName);          
                ClassName = Obj.findClassDef(Lines);
                
                if ~isempty(ClassName)
                    ClassName = Obj.fullName(PackageName, ClassName);                    
                    FuncName = Obj.findFunction(Lines, Args.FuncName);                    
                    if ~isempty(FuncName)                                           
                        Result = Obj.runTest(ClassName,  'FuncName', Args.FuncName);
                    elseif Args.Required
                        Obj.msgStyle(LogLevel.Warning, 'red', 'Missing class function: %s - %s()', ClassName, Args.FuncName);                        
                        Obj.Warn(['Missing class function: ', ClassName, ' - ', Args.FuncName, '()']);
                    end
                else                                       
                    FuncName = Obj.findFunction(Lines, Args.FuncName);
                    if ~isempty(FuncName)
                        FuncName = Obj.fullName(PackageName, FuncName);
                        Result = Obj.runTest('', 'FuncName', FuncName);
                    end
                end
            end            
        end       
                
        
        function Result = shouldProcessFile(Obj, FileName)
            %
            Result = true;
            fn = lower(FileName);
            if contains(fn, 'obsolete') || contains(fn, 'unused') || contains(fn, 'testing') || contains(fn, 'draft')
                Result = false;                
            end
        end
        
        
        function Result = Warn(Obj, Text)
            if ~any(find(strcmp(Obj.WarnList, Text)))
                Obj.WarnList{end+1} = Text;
            end
            Result = true;
        end
        
        
        function Result = fullName(Obj, PackageName, ClassOrFunc)
            %
            Result = ClassOrFunc;
            if ~isempty(PackageName)
                Result = [PackageName, '.', ClassOrFunc];
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
        
        
        function Result = findClassDef(Obj, Lines)
            % Search classdef
            
            ClassName = '';
            for i=1:length(Lines)
                % Check that it is not a comment
                Line = Lines{i};

                % Line must start with 'classdef'
                if ~startsWith(split(Line), 'classdef')
                    continue
                end
                
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
        

        function Result = findFunction(Obj, Lines, FuncName)
            % Search unitTest() function
            Result = '';
            for i=1:length(Lines)

                % Check that it is not a comment
                Line = Lines{i};
                Func = Obj.getFunctionName(Line);
                if strcmp(Func, FuncName)
                    Result = FuncName;
                    break;
                end
            end
        end
        
        
        function [ClassName, ClassFolder] = getClassName(Obj, FileName)
            % Get class name from folder name that starts with '@'
            
            [Path, Name, Ext] = fileparts(FileName);
            [Path, Name, Ext] = fileparts(Path);
            ClassName = '';
            ClassFolder = '';
            if startsWith(Name, '@')
                ClassName = Name(2:end);
                ClassFolder = fullfile(Path, Name);
            end
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
            
            %
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

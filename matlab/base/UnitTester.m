% Top-Level Unit Tester
% Author: Chen Tishler, 
%

% Usage:
%

% #functions (autogen)
% UnitTester -
% Warn -
% beforePush - Call to perform tests before git push - PUSH ONLY IF ALL TESTS PASS
% doBeforePush - Run all required tests before 'git push' command WARNING: DO NOT PUSH to common branch (currently: 'dev1') if there are failed tests!
% doPerfTest - Run all Performance tests
% doStressTest - Run all Stress tests
% doTest - Run all unit-tests and show report
% findClassDef - Search classdef
% findFunction - Search unitTest() function
% fullName -
% getClassName - Get class name from folder name that starts with '@'
% getFunctionName - 'function Result = unitTest()'
% getPackageName - Get package name from file name
% getTestFits - Return FITS file name and size from our test data folder
% isTested - Check if already tested by inspecting at Obj.TestList
% msgLog - Write message to log
% msgStyle - Write message to log
% perfTest - Run all Performance tests
% processFile -
% processFolder - Recursivly call processFile()
% readFile - Read file to Lines{}
% report - Print report of all performed tests
% runTest - Run single unit test, Target is class name Example: runTest('ImagePath')
% setup - Class setup, prepare path to source code
% shouldProcessFile -
% stressTest - Run all Stress tests
% test -
% testAll - Run all unit-test functions found in MATLAB source files
% testCore - Run core unit-tests, required before we can run any other
% testImage - Run image related unit-tests
% unitTest -
% #/functions (autogen)
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
            % [Internal use, called from constructor] Class setup, prepare path to source code 
            % Output:  true on success
            % Example: Obj.setup()

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
            % Output:  true if all tests passed
            % Example: Obj.doTest()

            Obj.msgLog(LogLevel.Test, 'Started\n');
            try
                Result = Obj.testAll();
            catch Ex
                Result = false;
                io.msgLogEx(LogLevel.Error, Ex, 'unitTest: Exception');
            end
            Obj.report();
            Obj.msgLog(LogLevel.Test, 'Done\n');
        end


        function report(Obj)
            % Print report of all performed tests
            % Output:  Display report to console & log file using Obj.msgLog()
            %          Returns none
            % Example: Obj.report()
            
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
        end


        function Result = doBeforePush(Obj)
            % Run all required tests before 'git push' command
            % WARNING: DO NOT PUSH to common branch (currently: 'dev1') if there are failed tests!
            % Output:  true on success
            % Example: Obj.doBeforePush()
            
            % Here for Debug only!
            % Result = Obj.processFolder(Obj.SourcePath);
            
            % Run core unit-tests
            Obj.testCore();

            % Run image-related unit-tests
            Obj.testImage();

            % Run tests by scanning source files
            Obj.processFolder(Obj.SourcePath);
            
            % Print report
            Obj.report();

            % Print final summary
            if Obj.FailCount == 0
                Obj.msgStyle(LogLevel.Test, '@pass', 'BeforePush passed, ready to git push');
            else
                Obj.msgStyle(LogLevel.Test, '@failed', 'BeforePush FAILED - DO NOT PUSH !!!');
            end
            
            Result = (Obj.FailCount == 0);
        end


        function Result = doPerfTest(Obj)
            % Run all Performance tests
            % Output:  true on success
            % Example: Obj.doPerfTest()
            
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
            % Output:  true on success
            % Example: Obj.doStressTest()
            
            db.DbQuery.stressTest();
            db.AstroDb.stressTest();
            db.AstroStore.stressTest();

            % Run tests by scanning source files
            Result = Obj.processFolder(Obj.SourcePath, 'FuncName', 'stressTest');
        end

    end

    %----------------------------------------------------------------------
    methods % Test functions

        function Result = testAll(Obj)
            % Run all unit-test functions found in MATLAB source files
            % Output:  true on success
            % Example: Obj.testAll()
            
            % First, run core unit-tests, required before any other tests
            Result = Obj.testCore();

            % Run unit-tests in .m files
            Result = Obj.processFolder(Obj.SourcePath);

            % Show report
            Obj.report();
        end


        function Result = testCore(Obj)
            % Run core unit-tests, required before we can run any other
            % Output:  true on success
            % Example: Obj.testCore()
            
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
            % Output:  true on success
            % Example: Obj.testImage()

            Obj.msgLog(LogLevel.Test, 'UnitTester.testImage started\n');
            Result = false;

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

            Obj.msgLog(LogLevel.Test, '\nUnitTester.testImage done');
            Result = true;
        end
    end
    
    %----------------------------------------------------------------------
    methods % 
    
        function Result = runTest(Obj, ClassName, Args)
            % Run single unit test, Target is class name
            % Input:    ClassName  - Class name to run test for
            %           'FuncName' - Test function name to call, default is 'unitTest'
            % Output:  true on success
            % Example: runTest('ImagePath')
            arguments
                Obj
                ClassName                       %
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
            catch Ex
                io.msgLogEx(LogLevel.Error, Ex, 'runTest exception: ');
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
            % Recursivly call processFile() for all files in given folder name
            % Input:    Path       - Path to source files folder
            %           'FuncName' - Test function name, default is 'unitTest'
            %           'Required' - true if FuncName is required to be defined in the
            %           class
            % Output: 
            % Example:            

            arguments
                Obj
                Path                            %
                Args.FuncName = 'unitTest';     % Function name
                Args.Required = true;           % true if FuncName is required to be in all files
            end
            

            %Obj.msgLog(LogLevel.Test, 'UnitTester.processFolder: %s', Path);

            % Scan all .m files in folder
            List = dir(fullfile(Path, '*'));
            for i = 1:length(List)
                fname = List(i).name;

                % Folder
                if List(i).isdir
                    if fname ~= "." && fname ~= ".."
                        Name = List(i).name;
                        FolderName = fullfile(List(i).folder, Name);
            
                        % Skip some folders
                        if ~Obj.shouldProcessFile(FolderName)
                            continue;
                        end
                        
                        % Class folder - call processFile() only for main
                        % class file (i.e. 'MyClassName.m')
                        if startsWith(Name, '@')
                            FileName = fullfile(FolderName, [Name(2:end), '.m']);
                            if isfile(FileName)
                                Obj.processFile(FileName, 'FuncName', Args.FuncName, 'Required', Args.Required);
                            else
                                Obj.msgStyle(LogLevel.Warning, 'red', 'Empty class folder: %s', FolderName);
                                Obj.Warn(['Empty class folder: ', FolderName]);
                            end
                            
                        % Non-class folder, recursivly call processFolder()
                        else
                            Obj.processFolder(FolderName, 'FuncName', Args.FuncName, 'Required', Args.Required);
                        end
                    end

                % File
                else
                    [path, name, ext] = fileparts(fname);
                    
                    % Process .m file
                    if ext == ".m"
                        FileName = fullfile(List(i).folder, List(i).name);
                        Obj.processFile(FileName, 'FuncName', Args.FuncName, 'Required', Args.Required);
                    end
                end
            end

            Result = true;
        end


        function Result = processFile(Obj, FileName, Args)
            % Process file
            % Input:    FileName   - 
            %           'FuncName' - 
            %           'Required' - 
            % Output:   true on success
            % Example:  processFile('~/AstroPack/matlab/@Base/Base.m')
            
            arguments
                Obj
                FileName                        %
                Args.FuncName = 'unitTest';     % Function name
                Args.Required = true;           %
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
            % Return true if file should be procssed, check for special
            % folders (i.e. 'unused')
            % Input:   FileName - 
            % Output:  true if given file should be processed
            % Example: shouldProcessFile('~/AstroPack/matlab/obsolete/hello.m')
            Result = true;
            fn = lower(FileName);
            if contains(fn, 'obsolete') || contains(fn, 'unused') || contains(fn, 'testing') || ...
                    contains(fn, 'draft') || contains(fn, 'external') || contains(fn, 'temp') || contains(fn, 'tmp')
                Result = false;
            end
        end
        
        
        function Warn(Obj, Text)
            % Add text to warnings list stored in Obj.WarnList
            % Input:   Text - any text to add to warnings list
            % Output:  -
            % Example: Obj.Warn('New warning text')
            if ~any(find(strcmp(Obj.WarnList, Text)))
                Obj.WarnList{end+1} = Text;
            end
        end
        
        
        function Result = fullName(Obj, PackageName, ClassOrFunc)
            % Return full file name including package
            % Input:   PackageName - Package name
            %          ClassOrFunc - Class name or function name
            % Output:  
            % Example: Full = fullName('io', 'msgLog')
            Result = ClassOrFunc;
            if ~isempty(PackageName)
                Result = [PackageName, '.', ClassOrFunc];
            end
        end
        
                    
        function Result = readFile(Obj, FileName)
            % Read file to Lines{}
            % Input:   FileName - file name to read
            % Output:  Cell array 
            % Example: Lines = readFile('~/AstroPack/matlab/Base/LogLevel.m')
            
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
            % Search 'classdef' in text lines
            % Input:   Lines - 
            % Output:  Given class name
            % Example: ClassName = findClassDef(TextLines)
            
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
            % Search specified function name in text lines
            % Input:   Lines - cell array of char
            %          FuncName - function name to find
            % Output:  FuncName if found, '' if not found
            % Example: Found = findFunction(Lines, 'MyFunc')
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
            % Input:   FileName - file name of .m source code
            % Output:  [Class name (char), Class folder (char)]
            % Example: [TheClass, Folder] = getClassName('matlab/@Base/Base.m');
            
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
            % Input:   FileName - file name of .m source code
            % Output:  Package name
            % Example: PkgName = getPackageName('matlab/+io/Test.m')
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
            % Extract function name from given source code line
            % 'function Result = unitTest()'
            % Input:   Line - source code line that should include 'function'
            % Output:  Function name if found in Line
            % Example: Func = getFunctionName(''function Result = unitTest()')
            
            Result = '';
            Items = split(Line, '%');
            Items = split(Items{1}, 'function');
            if numel(Items) > 1
                Items2 = split(Items{2}, '=');
                if numel(Items2) > 1
                    Items2 = split(strip(Items2{2}), '(');
                    Result = strip(Items2{1});
                else
                    Items2 = split(strip(Items{2}), '(');
                    Result = strip(Items2{1});
                end
            end
        end
        
        
        function Result = isTested(Obj, Target)
            % Check if already tested by inspecting Obj.TestList
            % Input:   Target - file name, function name, etc.
            % Output:  True if already tested (found in Obj.TestList)
            % Example: Tested = Obj.isTested('Base.m')
            
            if any(strcmp(Obj.TestList, Target))
                Result = true;
            else
                Result = false;
            end
        end


        function msgLog(Obj, Level, varargin)
            % Write message to log, see io.msgLog()
            io.msgLog(Level, varargin{:});
        end


        function msgStyle(Obj, Level, Style, varargin)
            % Write message to log with color, see io.msgStyle()
            io.msgStyle(Level, Style, varargin{:});
        end
    end


    %----------------------------------------------------------------------
    % Unit test
    methods (Static)

        function [FileName, FileSize] = getTestFits(Index)
            % Return FITS file name and size from our test data folder
            % Input:   Index - integer file number in the FITS folder
            % Output:  FileName - char, FileSize - numeric
            % Example: [File,Size] = getTestFits(1)
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
            % Dummy unitTest to avoid warnning about missing unitTest() function            
            Result = true;
        end
        
        
        function Result = test()
            %
            % Output:  true if all tested passed successfully
            % Example: UnitTester.test()            
            Tester = UnitTester;
            Result = Tester.doTest();
        end


        function Result = beforePush()
            % Call to perform tests before git push - PUSH ONLY IF ALL TESTS PASS
            % Before processing it calls 'clear all' and 'clear java'            
            % Output:  true if all tested passed successfully
            % Example: UnitTester.beforePush()
            clear all;
            clear java;
            
            %
            Tester = UnitTester;
            Result = Tester.doBeforePush();
        end


        function Result = perfTest()
            % Run all Performance tests
            % Output:  true if all tested passed successfully
            % Example: UnitTester.perfTest()
            Tester = UnitTester;
            Result = Tester.doPerfTest();
        end


        function Result = stressTest()
            % Run all Stress tests
            % Output:  true if all tested passed successfully
            % Example: UnitTester.stressTest()
            Tester = UnitTester;
            Result = Tester.doStressTest();
        end

    end

end

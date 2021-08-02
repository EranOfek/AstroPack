% Top-Level Unit Tester

%--------------------------------------------------------------------------

classdef UnitTester < handle
    
    % Properties
    properties (SetAccess = public)

        PassCount = 0
        FailCount = 0
        TestList = {}
        TestResult = {} 
        MyFileName = ''
        SourcePath = ''
    end
    
    %-------------------------------------------------------- 
    methods % Constructor            
        function Obj = UnitTester()
            Obj.setup();
        end
    end

    
    methods % Main functions
               
        function Result = setup(Obj)
            
            % Clear all persistent objecst and java spaces
            clear all;
            clear java;
            
            % Get full path and name of the file in which the call occurs, 
            % not including the filename extension
            Obj.MyFileName = mfilename('fullpath');       
            [MyPath, ~, ~] = fileparts(Obj.MyFileName);            
            Obj.SourcePath = fullfile(MyPath, '..');
           
            % @Todo: Fix path
            if tools.os.iswindows()
                Obj.SourcePath = 'D:\Ultrasat\AstroPack.git\matlab\';
            else
                Obj.SourcePath = '/home/eran/matlab/AstroPack/matlab/';
            end
            
            io.msgLog(LogLevel.Test, 'SourcePath: %s', Obj.SourcePath);
            
            Result = true;
        end
        
        
        function Result = doTest(Obj)
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
            
            %Obj.msgLog(LogLevel.Test, 'Test list:\n');            
            
            % Print passed
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
            
            % Print failed
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
            
            Obj.msgLog(LogLevel.Test, '\n');
            Obj.msgLog(LogLevel.Test, 'Passed: %d', Obj.PassCount);
            Obj.msgLog(LogLevel.Test, 'Failed: %d', Obj.FailCount);
            Result = true;
        end
 

        function Result = doBeforePush(Obj)
            % Call before Push
            
            Result = Obj.testCore();
            
            Result = Obj.testImage();
            
            Obj.report();
            
            if Obj.FailCount == 0
                Obj.msgStyle(LogLevel.Test, '@pass', 'BeforePush passed, ready to git push');
            else
                Obj.msgStyle(LogLevel.Test, '@failed', 'BeforePush FAILED - DO NOT PUSH !!!');
            end
        end
        
    end
    
    
    methods % Test functions
        
        function Result = testAll(Obj)
  
            Result = Obj.testCore();
            
            Result = Obj.runFolder(Obj.SourcePath);
            
            Obj.report();
        end
            
        
        function Result = testCore(Obj)
            
            Obj.msgLog(LogLevel.Test, 'UnitTester.testCore started\n');
            Result = false;
            
            % Set maximum log level
            MsgLogger.setLogLevel(LogLevel.All);
            
            % Must be tested in THIS ORDER:
            
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
            Obj.runTest('DbInfo');
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
              
        
        function Result = runTest(Obj, Target)
            % Run single unit test
            
            if any(strcmp(Obj.TestList, Target))
                Obj.msgLog(LogLevel.Test', 'already tested: %s', Target);
                Result = false;
                return
            end
            
            Result = false;

            % Save current dir
            PWD = pwd;            
            
            try
                UnitTestFunc = [Target, '.unitTest()'];
                Obj.msgStyle(LogLevel.Info, 'blue', 'runTest: %s', UnitTestFunc);
                Result = eval(UnitTestFunc);
            catch
                Obj.msgStyle(LogLevel.Error, '@error', 'runTest exception: ');
            end
            
            Obj.TestList{end+1} = Target;
            Obj.TestResult{end+1} = Result;
            
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
        
        
        function Result = runFolder(Obj, Path)
            % Load specify folder to properties
            % Note: Recursive function

            %Obj.msgLog(LogLevel.Test, 'UnitTester.runFolder: %s', Path);
            
            % Scan all .m files in folder
            List = dir(fullfile(Path, '*'));
            for i = 1:length(List)
                fname = List(i).name;
                if List(i).isdir                    
                    if fname ~= "." && fname ~= ".." 
                        FolderName = fullfile(List(i).folder, List(i).name);
                        Obj.runFolder(FolderName);
                    end
                else
                    [path, name, ext] = fileparts(fname);
                    if ext == ".m"
                        FileName = fullfile(List(i).folder, List(i).name);
                        Obj.runFile(FileName);
                    end
                end
            end
            
            Result = true;
        end
       
        
        
        function Result = runFile(Obj, FileName)
            
            %Obj.msgLog(LogLevel.Test, 'UnitTester.runFile: %s', FileName);
            
            % Skip self
            %MyFileName = mfilename('fullpath');

            
            % Read file to Lines{}
            fid = fopen(FileName);
            Line = fgetl(fid);
            Lines = cell(0, 1);
            while ischar(Line)
                Lines{end+1, 1} = Line;
                Line = fgetl(fid);
            end
            fclose(fid);

            % Search class name
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
                    if length(items) > 1
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
                % @Todo
                return;
                
                % Search unitTest() function
                haveUnitTest = false;
                haveFunc = false;
                for i=1:length(Lines)

                    % Check that it is not a comment
                    Line = Lines{i};
                    
                    items = split(Line, '%');
                    items = split(items{1}, 'function ');
                    if length(items) > 1
                        items = split(items{2}, '=');
                        
                        items = split(items{2}, ' ');
                        
                        %if lower(items{1}) == 'unitTest('
                            haveFunc = true;
                            %break;
                        %end
                    end
                end            

                if haveFunc && haveUnitTest
                    % Call unitTest
                    Func = ClassName + ".unitTest();"';
                    Result = eval(Func);
                    %Result = (ClassName).unitTest();
                else
                end
                
            end
        end
        

        function Result = isTested(Obj, Target)
            % Check if already tested
            
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
    methods(Static)
        
        function Result = test()
            Tester = UnitTester;
            Result = Tester.doTest();
        end
        
        
        function Result = beforePush()
            % Call to perform tests before git push - PUSH ONLY IF ALL TESTS PASS
            Tester = UnitTester;
            Result = Tester.doBeforePush();            
        end
    end
        
end


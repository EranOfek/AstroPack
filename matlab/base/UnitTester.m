% Top-Level Unit Tester

%--------------------------------------------------------------------------

classdef UnitTester < handle
    
    % Properties
    properties (SetAccess = public)

        PassCount = 0
        FailCount = 0
        TestList = {}
        TestResult = {} 
    end
    
    %-------------------------------------------------------- 
    methods % Constructor            
        function Obj = UnitTester()
        end
    end

    
    methods % Main functions
               
        
        function Result = doTest(Obj)
            Obj.msgLog(LogLevel.Test, 'Started\n');
            
            try
                Result = Obj.testAll();
            catch
                Result = false;
                Obj.msgStyle(LogLevel.Error, '@error', 'unitTest: Exception');
            end
            
            Obj.msgLog(LogLevel.Test, 'Done\n');
            
            
            Obj.msgLog(LogLevel.Test, 'Test list:\n');            
            for i = 1:numel(Obj.TestList)
                Passed = Obj.TestResult{i};
                if Passed                    
                    Obj.msgStyle(LogLevel.Test, 'black', 'Test: %s, Result: %d', Obj.TestList{i}, Obj.TestResult{i});
                else
                    Obj.msgStyle(LogLevel.Test, 'red', 'Test: %s, Result: %d', Obj.TestList{i}, Obj.TestResult{i});
                end
            end
            
            Obj.msgLog(LogLevel.Test, 'Passed: %d', Obj.PassCount);
            Obj.msgLog(LogLevel.Test, 'Failed: %d', Obj.FailCount);
        end
 

        function Result = testAll(Obj)
            
            % Get full path and name of the file in which the call occurs, 
            % not including the filename extension
            MyFileName = mfilename('fullpath');       
            [MyPath, ~, ~] = fileparts(MyFileName);            
            SourcePath = fullfile(MyPath, '..');
            
            Result = Obj.testCore();
            
            SourcePath = '/home/eran/matlab/AstroPack/matlab/';  %'D:\Ultrasat\AstroPack.git\matlab\';
            
            Result = Obj.runFolder(SourcePath);
        end
            
        
        function Result = testCore(Obj)
            
            Obj.msgLog('UnitTester.testCore started\n');
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
            Obj.runTest('io.db.DbDriver');
            Obj.runTest('io.db.DbConnection');
            Obj.runTest('io.db.DbRecord');
            Obj.runTest('io.db.DbQuery');
            
            % Images            
            Obj.runTest('VirtImage');
            Obj.runTest('ImageComponent');
            
            Obj.msgLog('\nUnitTester.testCore done');
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
            else
                Obj.FailCount = Obj.FailCount + 1;
            end
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
                    MyPath = [MyPath, '/'];
                    
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
    end
        
end


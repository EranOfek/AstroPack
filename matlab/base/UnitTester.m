% Top-Level Unit Tester

%--------------------------------------------------------------------------

classdef UnitTester < handle
    
    % Properties
    properties (SetAccess = public)

        Path            % Path
    end
    
    %-------------------------------------------------------- 
    methods % Constructor            
        function Obj = UnitTester()
            % Replace it with env? move to startup.m?
            % addpath('D:\Ultrasat\AstroPack.git\matlab\external');
        end
    end

    
    methods % Main functions
               
        function Result = runFolder(Obj, Path)
            % Load specify folder to properties

            Obj.Path = Path;
            io.msgLog(LogLevel.Test, 'UnitTester.runFolder: %s', Obj.Path);
            
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
            
            io.msgLog(LogLevel.Test, 'UnitTester.runFile: %s', FileName);
            
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
                
                items = split(Line, "%");
                items = split(items{1}, "classdef ");
                if length(items) > 1
                    io.msgLog(LogLevel.Test, 'Found classdef: %s', Line);
                    items = split(items{2}, ' ');
                    %if items{1} == "classdef "
                        ClassName = items{1};
                        break;
                    %end
                end
            end
            
            % Found classdef 
            if ClassName ~= ""
                
                % Search unitTest() function
                haveUnitTest = false;
                for i=1:length(Lines)

                    % Check that it is not a comment
                    Line = Lines{i};
                    
                    items = split(Line, '%');
                    items = split(items{1}, 'unitTest(');
                    if length(items) > 1
                        %if lower(items{1}) == 'unitTest('
                            haveUnitTest = true;
                            break;
                        %end
                    end
                end            

                if haveUnitTest
                    % Call unitTest
                    Func = ClassName + ".unitTest();"';
                    io.msgLog(LogLevel.Test, 'Calling %s', Line);
                    Result = eval(Func);                    
                else
                end
            
                
                
            % classdef not found
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
    end

    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = testBase()
            
            LogFile.unitTest();
            MsgLogger.unitTest();
            Base.unitTest();
            Component.unitTest();
            
        end
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        
        function Result = unitTest()
            try
                Result = UnitTester.doUnitTest();
            catch
                Result = false;
                io.msgLog(LogLevel.Error, 'unitTest: Exception');
            end
        end
        
            
        function Result = doUnitTest()
            io.msgLog(LogLevel.Test, 'Started\n');
            Path = 'C:\Ultrasat\AstroPack.git\matlab\base';
            Tester = UnitTester;
            Result = Tester.runFolder(Path);
        end
    end
        
end


% Top-Level Unit Tester

%--------------------------------------------------------------------------

classdef RunUnitTests < handle
    
    % Properties
    properties (SetAccess = public)

        Path            % Path
    end
    
    %-------------------------------------------------------- 
    methods % Constructor            
        function Obj = RunUnitTests()
            % Replace it with env? move to startup.m?
            % addpath('D:\Ultrasat\AstroPack.git\matlab\external');
        end
    end

    
    methods % Main functions
               
        function Result = runFolder(Obj, Path)
            % Load specify folder to properties

            Obj.Path = Path;
            msgLog('runFolder: %s', Obj.Path);
            
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
            
            msgLog('runFile: %s', FileName);
            
            fid = fopen(FileName);
            Line = fgetl(fid);
            Lines = cell(0, 1);
            while ischar(Line)
                Lines{end+1, 1} = Line;
                Line = fgetl(fid);
            end
            fclose(fid);

            %Data = fileread(FileName);
            %Lines = strsplit(Data);
            
            % Search class name
            ClassName = '';
            for i=1:length(Lines)
                % Check that it is not a comment
                Line = Lines{i};
                
                items = split(Line, "%");
                items = split(items{1}, "classdef ");
                if length(items) > 1
                    msgLog(Line);
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
                    %Result = eval(Func);
                    
                else
                end
            
                
                
            % classdef not found
            else

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
        
        function Result = unitTest()
            try
                Result = RunUnitTests.doUnitTest();
            catch
                Result = false;
                msgLog('unitTest: Exception');
            end
        end
        
            
        function Result = doUnitTest()
            msgLog('Started\n');
            Path = 'C:\Ultrasat\AstroPack.git\matlab\base';
            Tester = RunUnitTests;
            Result = Tester.runFolder(Path);
        end
    end
        
end



function msgLog(varargin)
    %fprintf('Configuration: ');
    fprintf(varargin{:});
    fprintf('\n');
end


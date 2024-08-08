% Database base class
% Chen Tishler, 04/08/2024

% Must set python to 3.10 when I have both 3.11 and 3.10 installed
%
% setenv PYTHON C:\Python310\python.exe
% setenv PYTHON3 C:\Python310\python.exe
% setenv PYTHONHOME C:\Python310
% pyversion c:\python310\python.exe

classdef DbBase < handle
    properties (Access = private)
        
    end
    
    methods
        function Obj = DbBase()
            % Must set python to 3.10 when I have both 3.11 and 3.10 installed
            setenv PYTHON C:\Python310\python.exe
            setenv PYTHON3 C:\Python310\python.exe
            setenv PYTHONHOME C:\Python310

            % This line can run only once per MATLAB session, need to find
            % solution for it (or have the correct Python version)            
            %pyversion c:\python310\python.exe

        end
        

        function data = select(Obj, query)
            % Implement in derived class
        end


        function data = convertPyToCell(Obj, result)
            % Convert the result to a MATLAB cell array
            numRows = length(result);
            if numRows > 0
                numCols = length(result{1});
                data = cell(numRows, numCols);
                for i = 1:numRows
                    row = result{i};
                    for j = 1:numCols
                        data{i, j} = Obj.convertPyData(row{j});
                    end
                end
            else
                data = {};
            end
        end


        function out = convertPyData(Obj, pydata)
            % Convert Python data to MATLAB data
            % Note: When selecting from Postgres using psycopg2, timestamp
            % is returned with isa(pydata, 'datetime'), and not 'py.datetime.datetime'
            if isa(pydata, 'py.str')
                out = char(pydata);
            elseif isa(pydata, 'py.int') || isa(pydata, 'py.float')
                out = double(pydata);
            elseif isa(pydata, 'py.bool')
                out = logical(pydata);
            elseif isa(pydata, 'py.datetime.datetime')
                out = char(pydata.isoformat());                
            elseif isa(pydata, 'py.list') || isa(pydata, 'py.tuple')
                out = cell(pydata);
                for k = 1:numel(out)
                    out{k} = Obj.convertPyData(out{k});
                end
            else
                out = pydata; % handle other types as needed
            end
        end
        
    end
end


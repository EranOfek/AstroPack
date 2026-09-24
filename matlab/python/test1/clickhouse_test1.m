% https://chatgpt.com/c/66faa918-a998-8012-b210-f42095801857
% 
% set path=C:\Matlab\R2023a\bin
% set PYTHONPATH=C:\Python310

function clickhouse_test()

    % Remove Python from system PATH 
    % fixPath();
    
	% Required on my Windows machine, I have multiple Python installations.
	% setenv('PYTHON', '');
	% setenv('PYTHON3', '');
	% setenv('PYTHONHOME', '');
	% setenv('PYTHONPATH', '');
	% pyenv('Version', 'C:\Python310\python.exe');
    disp(pyenv);

    % Ensure Python environment is correctly set
    if pyenv().Status ~= "Loaded"
        disp('Python environment is not loaded. Please configure pyenv correctly.');
        %return;
    end
  
    % Import the ClickHouse driver
    clear classes;
    clickhouseDriver = py.importlib.import_module('clickhouse_driver');
    py.importlib.reload(clickhouseDriver);

    % Create a connection to the ClickHouse database
    conn = clickhouseDriver.Client(host='socsrv', ...
                                   user='default', ...
                                   password='PassRoot', ...
                                   port=py.int(9000));
    
    % Execute a query to get the ClickHouse version
    query = "SELECT version()";
    result = conn.execute(query);
    
    % Extract and display the database version
    dbVersion = result{1}{1};
    disp(['ClickHouse Database Version: ', char(dbVersion)]);

    query = "SELECT t.* FROM sources_test.sources1hp t LIMIT 2";
    result = conn.execute(query);

    % Use string, double or cell function to convert to a MATLAB array.
    disp(result);

    % The message you're seeing indicates that the result from your ClickHouse 
    % query is returned as a Python list of tuples. In MATLAB, this Python 
    % object can be converted into MATLAB-compatible data types like strings, 
    % doubles, or cells. Each tuple in the Python list represents a row of data 
    % from your ClickHouse query result. 

    % Assume 'result' contains the data fetched from ClickHouse
    matlabResult = cell(result);

    % Display the MATLAB cell array
    disp(matlabResult);

    % Create a table and assign column names
    myTable = cell2table(matlabResult, ...
    'VariableNames', {'Timestamp', 'ID', 'Datetime', 'Latitude', 'Longitude', 'Column6', ... % Add more column names as appropriate
    });

% Display the table
disp(myTable);
end


function fixPath()
    % Get the current PATH environment variable
    currentPath = getenv('PATH');
    
    % Split the PATH into individual folders based on the platform-specific separator
    if ispc  % Windows
        separator = ';';
    else     % Linux/Mac
        separator = ':';
    end
    folders = strsplit(currentPath, separator);
    
    % Filter out folders that contain the word "python" (case-insensitive)
    filteredFolders = folders(~contains(folders, 'python', 'IgnoreCase', true));
    
    % Join the filtered folders back into a single string
    updatedPath = strjoin(filteredFolders, separator);
    
    % Update the PATH environment variable
    setenv('PATH', updatedPath);
    
    % Display the updated PATH for verification
    disp(getenv('PATH'));
end


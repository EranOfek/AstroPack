function clickhouse_test()

	% Required on my Windows machine, I have multiple Python installations.
	setenv('PYTHON', '');
	setenv('PYTHON3', '');
	setenv('PYTHONHOME', '');
	setenv('PYTHONPATH', '');
	pyenv('Version', 'C:\Python38\python.exe');
    disp(pyenv);

    % Ensure Python environment is correctly set
    if pyenv().Status ~= "Loaded"
        disp('Python environment is not loaded. Please configure pyenv correctly.');
        return;
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
end



function bitfields_test()

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
  
    % Import the Python module
    clear classes;
    pyModule = py.importlib.import_module('bitfields');
    py.importlib.reload(pyModule);
    
    % Call with JD
    jd = double(2459945.5);
	for year = 2023:2029
        id = pyModule.make_image_id_jd(double(jd), int32(1), int32(1), int32(1)); 
        fprintf('year (julian): %d = %X\n', year, id);
        jd = jd + 365;
    end

    % Call with datetime
	for year = 2023:2029
        dt = datetime(year, 1, 1);
        id = pyModule.make_image_id(dt, int32(1), int32(1), int32(1));
        fprintf('year: %d = %X\n', year, id);
    end    
	
end

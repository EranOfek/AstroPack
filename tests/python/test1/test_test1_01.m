
function simpletest_test()

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

    % Define the inputs
    inputString = "Hello";
    inputScalar = 42;
    inputList = [1, 2, 3, 4, 5];
    
    % Convert MATLAB array to Python list
    inputListPy = py.list(inputList);
    
    % Import the Python module
    pyModule = py.importlib.import_module('simpletest');
    
    % Call the Python function
    result = pyModule.process_data(inputString, inputScalar, inputListPy);
    
    % Extract and display the results
    processedString = string(result{1});
    processedScalar = double(result{2});
    processedList = double(py.array.array('d', result{3}));
    
    disp(['Processed String: ', processedString]);
    disp(['Processed Scalar: ', num2str(processedScalar)]);
    disp('Processed List:');
    disp(processedList);
end



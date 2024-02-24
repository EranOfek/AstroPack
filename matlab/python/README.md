# MATLAB-Python Integration

MATLAB R2020b supports Python 2.7, 3.6, 3.7, 3.8.

MATLAB R2023a supports Python 3.8, 3.9, 3.10 (3.11 is not supported).

MATLAB R2023b supports 3.9, 3.10, 3.11 (3.8 support was removed).


Therefore we currently use 3.8, which is also the one installed by default on Ubuntu 20.04.1 (LAST0).



## Links

https://www.mathworks.com/help/matlab/ref/pyenv.html

https://www.mathworks.com/help/matlab/matlab_external/install-supported-python-implementation.html

https://www.mathworks.com/support/requirements/python-compatibility.html



## Set python version to use

Required on Windows if multiple version of Python are installed, and the default
is not the one to be used by MATLAB.

	setenv('PYTHON', '');
	setenv('PYTHON3', '');
	setenv('PYTHONHOME', '');
	setenv('PYTHONPATH', '');
	pyenv('Version', 'C:\Python38\python.exe');


Might be required, if the above does not work:

	pyenv('Version', 'C:\Python38\python.exe', 'ExecutionMode', 'OutOfProcess');


Test:

	py.print("Hello from Python", py.int(3.8))


## LAST0

Check version (must use 'python3')

	
	ocs@last0:~$ python3 --version
	
	Python 3.8.10
	
	


Check Linux version

	ocs@last0:~$ uname -a
	
	Linux last0 5.15.0-92-generic #102~20.04.1-Ubuntu SMP Mon Jan 15 13:09:14 UTC 2024 x86_64 x86_64 x86_64 GNU/Linux
	

## Reload Module

https://www.mathworks.com/matlabcentral/answers/576565-matlab-requires-restart-before-changes-in-python-code-changes-are-applied


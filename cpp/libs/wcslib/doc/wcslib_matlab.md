16/02/2021 

# wcslib Matlab Interface

Notes

- Package name must be like **wcslibPkg** (lower-case 'w', not WcslibPkg)
 
  
### MATLAB C++ Interface - Links

[https://www.mathworks.com/help/matlab/matlab_external/publish-interface-to-shared-c-library.html](https://www.mathworks.com/help/matlab/matlab_external/publish-interface-to-shared-c-library.html)
[https://blogs.mathworks.com/developer/2019/07/11/cpp-interface/](https://blogs.mathworks.com/developer/2019/07/11/cpp-interface/)
[https://www.mathworks.com/help/matlab/matlab_external/how-to-define-and-publish-matlab-interface-for-cpp-library-sample.html	](https://www.mathworks.com/help/matlab/matlab_external/how-to-define-and-publish-matlab-interface-for-cpp-library-sample.html )
[https://www.mathworks.com/matlabcentral/fileexchange/38964-example-matlab-class-wrapper-for-a-c-class](https://www.mathworks.com/matlabcentral/fileexchange/38964-example-matlab-class-wrapper-for-a-c-class)
[https://www.mathworks.com/help/coder/ug/call-cc-code-from-matlab-code.html](https://www.mathworks.com/help/coder/ug/call-cc-code-from-matlab-code.html)


## Structure of wcslib C++ Project



	D:\Ultrasat\wcs\wcslib - Base folder
	- cfitsio
	- cfitsio\mf
	- cfitsio\wcslib
	- cfitsio\wcslib\test
	- cfitsio\wcslib\wcslib_class
	- cfitsio\zlib
	- util
	- wcs_matlab
	


The project is based on **cfitsio** repository, using its **CMakeLists.txt**

CMakeLists.txt of cfitsio was modified to compile the modifications:

Subfolders under **cfitsio/**

- **wcslib/** - wcslib files from the original **wcslib** repository
- **wcslib/wcslib_class/** * Class files and auto-generated files 
- **mf/** - Cross platform (Windows/Linux) General C++ library

 
Files in **wcslib/wcslib_class/**
  
- wcslib_class.h
- wcslib_class.cpp
- wcslib_class_matlab.h
- wcslib_class_matlab.cpp
- wcslib_class_test.h
- wcslib_class_test.cpp

Auto generated files by Python script, that are included from wcslib_class_matlab.h and wcslib_class_matlab.cpp 

- wcslib_class_props.h
- wcslib_class_props.cpp


## Generate library definition

### Step 1: Generate Files using Python Script

Run utils/wcsprm_wrapper.py

This script reads **wcsprm_ready.h** which contains **struct wcsprm** of wcslib.

The output of the script is writte to .h and .cpp files, which are included from
wcslib_class_matlab.h and wcslib_class_matlab.cpp. 

- wcslib_class_props.h - Properties getters/setters
- wcslib_class_props.cpp - Properties getters/setters 
	

It also updates the MATLAT class source file **wcslibCl.m**.

by replacing text between tags:
    
    %<props>
	Properties definition will be placed here.    
    %</props> 
    
    %<props_methods>
	Getters/setters will be placed here.
    %</props_methods>

 
### Step 2: Compile C++ Project

#### Windows 

Use Visual Studio 2019.

#### Linux


### Step 3: Clean MATLAB Files 

Run **\_CleanAndCopy.bat** to 

- Clean auto-generated files 
- Copy fresh files


**\_CleanAndCopy.bat**

	set cpp=..\cfitsio\wcslib\wcslib_class
	set bin=..\cfitsio\Build\Debug
	set matlab=.
	set pkg=wcslibPkg
	set definepkg=define%pkg%
	pause
	
	del %matlab%\%pkg%\%pkg%Interface.dll
	del %matlab%\cfitsio.dll
	del %matlab%\cfitsio.lib
	del %matlab%\%definepkg%.m
	del %matlab%\%definepkg%.mlx
	del %matlab%\%pkg%Data.xml
	
	rem copy %cpp%\wcslib_wrapper.h %matlab%\
	copy %cpp%\wcslib_class_matlab.h %matlab%\
	copy %cpp%\wcslib_class_props.h %matlab%\
	copy %bin%\cfitsio.dll %matlab%\
	copy %bin%\cfitsio.lib %matlab%\
	pause


### Step 4: Generate MATLAB interface

Open MATLAB and type the following commands, to generate the interface:

cfitsio source tree in folder
  
Working directory is D:\Ultrasat\wcs\wcslib\wcs_matlab


	matPath = 'D:\Ultrasat\wcs\wcslib\wcs_matlab';
	libFile = 'cfitsio.lib';
	hppFile = 'wcslib_class_matlab.h';
	pkg     = 'wcslibPkg';
	
	if ~isfolder(matPath)
		mkdir(matPath)
	end
	cd(matPath)

	mex -setup:C:\BinMATLAB\R2020b\bin\win64\mexopts\msvcpp2019.xml C++
	mex -setup cpp
	
	clibgen.generateLibraryDefinition(fullfile(matPath,hppFile),...
	'IncludePath', matPath,... 
	'Libraries', fullfile(matPath,libFile),... 
	'PackageName', pkg,...
	'ReturnCArrays',true,... % treat output as MATLAB arrays
	'Verbose',true)


Build

	definewcslibPkg
	summary(definewcslibPkg)
	build(definewcslibPkg)





This is the expected output

	
	Renamed options file 'C:\Users\chen\AppData\Roaming\MathWorks\MATLAB\R2020b\mex_C++_win64.xml' to 'C:\Users\chen\AppData\Roaming\MathWorks\MATLAB\R2020b\mex_C++_win64_backup.xml'.
	MEX configured to use 'Microsoft Visual C++ 2019' for C++ language compilation.
	MEX configured to use 'Microsoft Visual C++ 2019' for C++ language compilation.
	
	To choose a different C++ compiler, select one from the following:
	Microsoft Visual C++ 2015  mex -setup:C:\BinMATLAB\R2020b\bin\win64\mexopts\msvcpp2015.xml C++
	Microsoft Visual C++ 2017  mex -setup:C:\BinMATLAB\R2020b\bin\win64\mexopts\msvcpp2017.xml C++
	Microsoft Visual C++ 2019  mex -setup:C:\Users\chen\AppData\Roaming\MathWorks\MATLAB\R2020b\mex_C++_win64.xml C++
	Warning: Some C++ language constructs in the files for generating interface file are not supported and not
	imported.
	 
	Using Microsoft Visual C++ 2019 compiler.
	Generated definition file definewcslibPkg.mlx and data file 'wcslibPkgData.xml' contain definitions for 11 constructs supported by MATLAB.
	4 construct(s) require(s) additional definition. To include these construct(s) in the interface, edit the definitions in definewcslibPkg.mlx.
	Build using build(definewcslibPkg).
	>> build(definewcslibPkg)
	matPath = 'D:\Ultrasat\wcs\wcslib\wcs_matlab';
	libFile = 'cfitsio.lib';
	hppFile = 'wcslib_class_matlab.h';
	pkg     = 'wcslibPkg';
	if ~isfolder(matPath)
	mkdir(matPath)
	end
	cd(matPath)
	mex -setup:C:\BinMATLAB\R2020b\bin\win64\mexopts\msvcpp2019.xml C++
	mex -setup cpp
	clibgen.generateLibraryDefinition(fullfile(matPath,hppFile),...
	'IncludePath', matPath,...
	'Libraries', fullfile(matPath,libFile),...
	'PackageName', pkg,...
	'ReturnCArrays',true,... % treat output as MATLAB arrays


### Step 5: Fix MATLAB unresolved 

5.1 - Click the **definewcslibPkg.mlx** link in the output line of the previous step.

	Generated definition file definewcslibPkg.mlx and data file 'wcslibPkgData.xml' contain definitions for 11 constructs supported by MATLAB.

The editor displays file **definewcslibPkg.mlx**

From the **Save** menu, select **Save As...**. 

- File name: **definewcslibPkg.m**
- Save as type: **MATLAB Code files (windows-1255) (.m)**


The editor now displays **definewcslibPkg.m** in a new tab.


### Step 6: Run Python script to automaticlly fix definewcslibPkg.m

The script will fix such commented definitions which normally require manual edit. 


	%% C++ function |wcsReadFits| with MATLAB name |clib.wcslibPkg.wcsReadFits|
	% C++ Signature: bool wcsReadFits(char const * fname)
	%wcsReadFitsDefinition = addFunction(libDef, ...
	%    "bool wcsReadFits(char const * fname)", ...
	%    "MATLABName", "clib.wcslibPkg.wcsReadFits", ...
	%    "Description", "clib.wcslibPkg.wcsReadFits    Representation of C++ function wcsReadFits."); % Modify help description values as needed.
	%defineArgument(wcsReadFitsDefinition, "fname", <MLTYPE>, "input", <SHAPE>); % '<MLTYPE>' can be clib.array.wcslibPkg.Char,int8,string, or char
	%defineOutput(wcsReadFitsDefinition, "RetVal", "logical");
	%validate(wcsReadFitsDefinition);


and

	%% C++ function |wcsPixToSky| with MATLAB name |clib.wcslibPkg.wcsPixToSky|
	% C++ Signature: void wcsPixToSky(double * x,double * y,size_t len)
	%wcsPixToSkyDefinition = addFunction(libDef, ...
	%    "void wcsPixToSky(double * x,double * y,size_t len)", ...
	%    "MATLABName", "clib.wcslibPkg.wcsPixToSky", ...
	%    "Description", "clib.wcslibPkg.wcsPixToSky    Representation of C++ function wcsPixToSky."); % Modify help description values as needed.
	%defineArgument(wcsPixToSkyDefinition, "x", "clib.array.wcslibPkg.Double", "input", <SHAPE>); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
	%defineArgument(wcsPixToSkyDefinition, "y", "clib.array.wcslibPkg.Double", "input", <SHAPE>); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
	%defineArgument(wcsPixToSkyDefinition, "len", "uint64");
	%validate(wcsPixToSkyDefinition);

Fixes
	
	wcsReadFitsDefinition = addFunction(libDef, ...
	    "bool wcsReadFits(char const * fname)", ...
	    "MATLABName", "clib.wcslibPkg.wcsReadFits", ...
	    "Description", "clib.wcslibPkg.wcsReadFits    Representation of C++ function wcsReadFits."); % Modify help description values as needed.
	defineArgument(wcsReadFitsDefinition, "fname", "string", "input", "nullTerminated"); % '<MLTYPE>' can be clib.array.wcslibPkg.Char,int8,string, or char
	defineOutput(wcsReadFitsDefinition, "RetVal", "logical");
	validate(wcsReadFitsDefinition);
	
	
	wcsReadHeaderDefinition = addFunction(libDef, ...
	    "bool wcsReadHeader(char const * fname)", ...
	    "MATLABName", "clib.wcslibPkg.wcsReadHeader", ...
	    "Description", "clib.wcslibPkg.wcsReadHeader    Representation of C++ function wcsReadHeader."); % Modify help description values as needed.
	defineArgument(wcsReadHeaderDefinition, "fname", "string", "input", "nullTerminated"); % '<MLTYPE>' can be clib.array.wcslibPkg.Char,int8,string, or char
	defineOutput(wcsReadHeaderDefinition, "RetVal", "logical");
	validate(wcsReadHeaderDefinition);
	
	
	wcsPixToSkyDefinition = addFunction(libDef, ...
	    "void wcsPixToSky(double * x,double * y,size_t len)", ...
	    "MATLABName", "clib.wcslibPkg.wcsPixToSky", ...
	    "Description", "clib.wcslibPkg.wcsPixToSky    Representation of C++ function wcsPixToSky."); % Modify help description values as needed.
	defineArgument(wcsPixToSkyDefinition, "x", "double", "inputoutput", "len"); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
	defineArgument(wcsPixToSkyDefinition, "y", "double", "inputoutput", "len"); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
	defineArgument(wcsPixToSkyDefinition, "len", "uint64");
	validate(wcsPixToSkyDefinition);
	
	
	wcsSkyToPixDefinition = addFunction(libDef, ...
	    "void wcsSkyToPix(double * x,double * y,size_t len)", ...
	    "MATLABName", "clib.wcslibPkg.wcsSkyToPix", ...
	    "Description", "clib.wcslibPkg.wcsSkyToPix    Representation of C++ function wcsSkyToPix."); % Modify help description values as needed.
	defineArgument(wcsSkyToPixDefinition, "x", "double", "inputoutput", "len"); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
	defineArgument(wcsSkyToPixDefinition, "y", "double", "inputoutput", "len"); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
	defineArgument(wcsSkyToPixDefinition, "len", "uint64");
	validate(wcsSkyToPixDefinition);


### Step 7: Build MATLAB interface

	definewcslibPkg
	summary(definewcslibPkg)
	build(definewcslibPkg)



Test basic interface

	clib.wcslibPkg.tstGetBool()
	clib.wcslibPkg.tstSetBool(true)
	clib.wcslibPkg.tstGetBool()
	clib.wcslibPkg.tstSetBool(false)
	clib.wcslibPkg.tstGetBool()

	clib.wcslibPkg.tstGetInt()
	clib.wcslibPkg.tstSetInt(11)
	clib.wcslibPkg.tstGetInt()
	clib.wcslibPkg.tstSetInt(12)
	clib.wcslibPkg.tstGetInt()

	clib.wcslibPkg.tstGetDouble()
	clib.wcslibPkg.tstSetDouble(111)
	clib.wcslibPkg.tstGetDouble()
	clib.wcslibPkg.tstSetDouble(112)
	clib.wcslibPkg.tstGetDouble()


### Step 8: Test the class

	cd('D:\Ultrasat\wcs\wcslib\wcslibCl')

Test

	clib.wcslibPkg.wcsReadFile('D:\Ultrasat\wcs\Images\ztf_20190709485764_000600_zg_c04_o_q4_sciimg.fits')


	clib.wcslibPkg.wcsPix2sky([100,200,300],[400,500,600])


	w = clib.wcslibPkg.wcslibUtil()

	w.readFits('12345678');


After restart


	addpath('D:\Ultrasat\wcs\wcslib\wcs_matlab')

	w = clib.wcslibPkg.wcslibUtil()

	w.readFits('12345678');



### Fix Definitions by clicking defineWcslibCpp.mlx

See: [https://www.mathworks.com/help/matlab/matlab_external/define-missing-information-for-matlab-signatures.html](https://www.mathworks.com/help/matlab/matlab_external/define-missing-information-for-matlab-signatures.html)


Fix **openFitsDefinition**

	<MLTYPE> "string"
	<SHAPE> "nullTerminated"

Definition

	openFitsDefinition = addMethod(WcslibUtilDefinition, ...
	    "bool WcslibUtil::openFits(char const * _filename,int mode)", ...
	    "Description", "clib.WcslibCpp.WcslibUtil.openFits    Method of C++ class WcslibUtil."); % Modify help description values as needed.
	defineArgument(openFitsDefinition, "_filename", "string", "input", "nullTerminated"); % '<MLTYPE>' can be clib.array.WcslibCpp.Char,int8,string, or char
	defineArgument(openFitsDefinition, "mode", "int32");
	defineOutput(openFitsDefinition, "RetVal", "logical");
	validate(openFitsDefinition);


Save the **.mlx** file


### Build

	definewcslibPkg
	summary(definewcslibPkg)
	build(definewcslibPkg)


Copy the DLL file from subfolder

Output

	Building interface file 'wcslibPkgInterface.dll'.
	Interface file 'wcslibPkgInterface.dll' built in folder 'D:\Ultrasat\wcs\wcslib\wcs_matlab\wcslibPkg'.
	To use the library, add the interface file folder to the MATLAB path.

### Test

	matPath = 'D:\Ultrasat\wcs\wcslib\wcs';
	cd(matPath)


	addpath('D:\Ultrasat\wcs\wcslib\wcs_matlab')
	addpath('D:\Ultrasat\wcs\wcslib\wcs_matlab\wcslibPkg')


	addpath('D:\Ultrasat\wcs\wcslib\wcs_matlab')

	w = clib.wcslibPkg.wcslibUtil()

	w.readFits('12345678');


	w.openFits('D:\Ultrasat\wcs\Images\ztf_20190709485764_000600_zg_c04_o_q4_sciimg.fits');
	
	w.readFits('D:\Ultrasat\wcs\Images\ztf_20190709485764_000600_zg_c04_o_q4_sciimg.fits');

	w.pix2sky(1, 1)
	w.pix2sky(1, 2)


### Test the class

	cd('D:\Ultrasat\wcs\wcslib\wcslibCl')
	w = wcslibCl() 

## WCS Properties (from Eran's @wcsCl)

See @wcsCl/wcsCl.m




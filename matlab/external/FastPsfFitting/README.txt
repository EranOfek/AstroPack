
 Matlab files:
psfFit.m       - Fit a (non-isotropic, rotated) gaussian point spread function model to the given image.
psfFit_Image.m - Fit multiple spot candidates in a given image with a (non-isotropic, rotated) gaussian point spread function model.
EXAMPLE.m      - Simple demonstration using psfFit_Image.


The fitting code uses the ceres-solver library for optimization which is developed by Google.
  URL: http://ceres-solver.org


-- Pre-compiled files for Windows 7 64-bit and Linux (compiled with Kubuntu 14.4) --

Compiled mex files for 64-bit Windows built using Visual Studio Professional 2012 can be downloaded from:
	https://drive.google.com/file/d/0BzFq6JJEnf3fTVlDdkhjLWlVY0E/view?usp=sharing
Compiled mex files for Linux (in my case Kubuntu 14.4) using g++ 4.8.4 can be downloaded from:
	https://drive.google.com/file/d/0BzFq6JJEnf3fMUg0LWFtY0FpVGs/view?usp=sharing

% WINDOWS NOTE	 
	For the precompiled mex files to work you may need the "Visual C++ Redistributable for Visual Studio 2012 Update 4" which you can download from www.microsoft.com. 
	Make sure all files are in the directory of the Matlab functions before calling them.

% LINUX NOTE:
	Matlab in Linux comes with its own c++ standard library, which is usually too
	old and not compatible with shared libraries used by ceres.
    As Matlab loads its own STL before the system libraries (by
    setting LD_LIBRARY_PATH to the MATLAB library path) this will result
    in failures when the mex file (shared library) is called.
    %
    If you encounter invalid mex files while executing the program, or
    runtime linking errors try setting the LD_PRELOAD environment variable 
    before starting matlab to your system libraries (where libstdc++ and
    libgfortran are located.
	%
	If you still encounter problems, consider installing the ceres dependencies (which are also needed for compilation below) by executing linux_install_forCeresBuild.sh (works only for Ubuntu/Kubuntu etc.).
	


-- Compiling source code by yourself --

(x) Go to the FastPsfFitting directory
   For compiling ceres we use Tal Ben-Nun's github repository for windows (we use it for linux as well).
   - If you acquired FastPsfFitting as a git repository, simply execute "git submodule update --init --recursive" within a git enabled terminal (git bash) to download it automatically.
   - If you acquired FastPsfFitting as a ZIP file, install git and clone Tal Ben-Nun's repository from "https://github.com/tbennun/ceres-windows"
     into the ceres-windows subdirectory. Then switch into the ceres-windows directory and execute "git submodule update --init --recursive" within a git enabled terminal (git bash).
(x) Download the eigen library from
     URL: http://eigen.tuxfamily.org/ 
   and put it in "ceres-windows/eigen"

Windows (build tested with Windows 7, Visual Studio Professional 2012):
To build ceres with Visual Studio simply open the project files in ceres-windows and 
start the compilation of project "ceres" in "Release" "x64" mode.

Linux (build tested with Kubuntu 14.4):
With Linux ceres can be compiled with CMake as follows:
- Execute linux_install_forCeresBuild.sh to install neccessary libraries
- Switch to the "ceres-windows/ceres-solver/" directory
- Execute "mkdir build && cd build"
- Execute "cmake-gui ..", press "Configure" once, set "BUILD_SHARED_LIBS" to true, press "Generate"
- Close cmake-gui and execute "make -j4"


Given a working MEX setup, after building ceres the FastPsfFitting functions can be compiled from MATLAB using the provided build_xxx.m-scripts.


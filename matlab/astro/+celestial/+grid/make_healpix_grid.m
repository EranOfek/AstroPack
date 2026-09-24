function make_healpix_grid(Npix, Args)
    % make a grid of HEALPix positions covering the whole sky
    % Input: - Npix parameter of the HEALPix algorythm (effective map resolution)
    %      * ...,key,val,...
    %      'Python' - system command for the python3 executable
    % Output: - a text file containing the grid points in Lon, Lat
    % Author: A.M. Krassilchtchikov (Dec 2023)
    % Example: celestial.grid.make_healpix_grid(4)
    arguments
        Npix = 4;
        Args.Python = 'python3';
    end
    
    N = sprintf('%d',Npix);
    
    current_function_path = mfilename('fullpath');
    [current_function_directory, ~, ~] = fileparts(current_function_path);
    Script = sprintf('%s/make_healpix_grid.py',current_function_directory);
    
    try
        system([Args.Python ' ' Script ' ' N]);
    catch
        fprintf('Some of the required python libraries may be missing.\n'); 
        fprintf('You can install them in the following way:\n'); 
        fprintf('> pip install healpy \n'); 
        return
    end

end

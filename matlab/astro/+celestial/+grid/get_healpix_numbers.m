function get_healpix_numbers(NSide, RA, Dec, Args)
    % get the healpix pixel numbers (indices) for given sky positions and map resolution
    % Input: - Npix parameter of the HEALPix algorythm (effective map resolution)
    %        - an array of RA [deg] OR a filename with RA Dec in two columns
    %        - an array of Dec [deg] (may skip it if RA is a filename)
    %      * ...,key,val,...
    %      'Python' - system command for the python3 executable
    %      'Nested' - 0 (ring structure) or 1 (nested structure)
    %      'Lonlat' - 1 (degrees) or 0 (radians)
    % Output: - a text file named "healpix_nside_{nside}_pixelnumbers.txt" containing the HEALPIX pixel numbers in a row
    % Author: A.M. Krassilchtchikov (Mar 2024)
    % Example: celestial.grid.get_healpix_numbers(2048, '~/ULTRASAT/SkyGrid/coords.txt')
    %          celestial.grid.get_healpix_numbers(2048, 10.,1)
    %          celestial.grid.get_healpix_numbers(2048, '~/ULTRASAT/SkyGrid/coords.txt', 'Nested', 'True')
    arguments    
        NSide
        RA
        Dec         =  0;
        Args.Python = 'python3';
        Args.Nested = 0;
        Args.Lonlat = 1; 
    end
    
    current_function_path = mfilename('fullpath');
    [current_function_directory, ~, ~] = fileparts(current_function_path);
    Script = sprintf('%s/get_healpix_numbers.py',current_function_directory);
    
    Nside = sprintf('%d',NSide);
    Nested  = sprintf('%d',Args.Nested);
    Degrees = sprintf('%d',Args.Lonlat);
    
    if isnumeric(RA)
        RA  = sprintf('%f',RA);
        Dec = sprintf('%f',Dec);
    else
        Dec = sprintf('%f',Dec);
    end
    
    try
        system([Args.Python ' ' Script ' ' Nside ' ' RA ' ' Dec ' ' Nested ' ' Degrees]);
    catch
        fprintf('Some of the required python libraries may be missing.\n'); 
        fprintf('You can install them in the following way:\n'); 
        fprintf('> pip install healpy \n'); 
        return
    end

end

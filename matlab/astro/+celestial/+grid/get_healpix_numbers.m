function get_healpix_numbers(NSide, RA, Dec, Args)
    % make a grid of HEALPix positions covering the whole sky
    % Input: - Npix parameter of the HEALPix algorythm (effective map resolution)
    %        - an array of RA [deg] OR a filename with RA Dec in two columns
    %        - an array of Dec [deg] (may skip it if RA is a filename)
    %      * ...,key,val,...
    %      'Python' - system command for the python3 executable
    %      'Nested' - 'False' (ring structure) or 'True' (nested structure)
    %      'Lonlat' - 'True' (degrees) or 'False' (radians)
    % Output: - a text file containing the HEALPIX pixel numbers in a row
    % Author: A.M. Krassilchtchikov (Mar 2024)
    % Example: celestial.grid.make_healpix_grid(4)
    arguments    
        NSide
        RA
        Dec         =  0;
        Args.Python = 'python3';
        Args.Nested = 'False';
        Args.Lonlat = 'True'; 
    end
    
    current_function_path = mfilename('fullpath');
    [current_function_directory, ~, ~] = fileparts(current_function_path);
    Script = sprintf('%s/get_healpix_numbers.py',current_function_directory);
    
    Nside = sprintf('%d',NSide);
    
    if isnumeric(RA)
        RA  = sprintf('%f',RA);
        Dec = sprintf('%f',Dec);
    else
        Dec = sprintf('%f',Dec);
    end
    
    try
        system([Args.Python ' ' Script ' ' Nside ' ' RA ' ' Dec ' ' Args.Nested ' ' Args.Lonlat]);
    catch
        fprintf('Some of the required python libraries may be missing.\n'); 
        fprintf('You can install them in the following way:\n'); 
        fprintf('> pip install healpy \n'); 
        return
    end

end

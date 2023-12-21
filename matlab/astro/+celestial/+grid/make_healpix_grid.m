function make_healpix_grid(Npix, Args)
    %
    
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
        error('Some of the required python libraries may be missing'); 
    end

end

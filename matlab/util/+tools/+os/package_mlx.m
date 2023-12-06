function package_mlx(filename)
    % Open MLX file of class, which is expected in folder '/help/mlx/.../+Package.mlx'
    % Example:
    % filename = 'D:\Ultrasat\AstroPack.git\matlab\astro\+astro\help.m'
    % open 'D:\Ultrasat\AstroPack.git\matlab\help\mlx\astro\+astro\+astro.mlx'
    base = fullfile(getenv('ASTROPACK_PATH'), 'matlab');
    fname = filename(numel(base)+2:end);      
    fname = fname(1:end-5);
    [~, pkg, ~] = fileparts(fname);
    mlx = fullfile(base, 'help', 'mlx', fname, pkg);
    open_mlx = ['open ', mlx];
    eval(open_mlx);
end

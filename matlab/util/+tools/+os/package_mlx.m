function package_mlx(filename)
    % Open MLX file of package, which is expected in folder '/help/mlx/.../+Package.mlx'
    % Can be invoked by using the package heirarchy
    % Example:
    % open: 'D:\Ultrasat\AstroPack.git\matlab\help\mlx\astro\+astro\+astro.mlx'    
    % with helper file: 'D:\Ultrasat\AstroPack.git\matlab\astro\+astro\help.m'
    % can be invoked by calling: astro.help
    base = fullfile(tools.os.getAstroPackPath, 'matlab');
    fname = filename(numel(base)+2:end);
    fname = fname(1:end-5);
    [~, pkg, ~] = fileparts(fname);
    mlx = fullfile(base, 'help', 'mlx', fname, pkg);
    open_mlx = ['open ', mlx];
    eval(open_mlx);
end

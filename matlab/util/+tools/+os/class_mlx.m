function class_mlx(filename)
    % Open MLX file of class, which is expected in folder '/help/mlx/.../ClassName.mlx'   
    % Example:
    % filename = 'D:\Ultrasat\AstroPack.git\matlab\util\+db\@DbAdmin\DbAdmin.m'
    % open 'D:\Ultrasat\AstroPack.git\matlab\help\mlx\util\+db\@DbAdmin\DbAdmin.mlx'
    base = fullfile(getenv('ASTROPACK_PATH'), 'matlab');
    fname = filename(numel(base)+2:end);     
    mlx = fullfile(base, 'help', 'mlx', fname);       
    open_mlx = ['open ', mlx];
    eval(open_mlx);
end

function class_mlx(filename)
    % Open MLX file of class, which is expected in folder '/help/mlx/.../ClassName.mlx'
    % Can be invoked by using the package heirarchy
    % Example:
    % open: 'D:\Ultrasat\AstroPack.git\matlab\help\mlx\util\+db\@DbAdmin\DbAdmin.mlx'
    % for class: 'D:\Ultrasat\AstroPack.git\matlab\util\+db\@DbAdmin\DbAdmin.m'
    % can be invoked by calling: db.DbAdmin.help
    base = fullfile(tools.os.getAstroPackPath, 'matlab');
    fname = filename(numel(base)+2:end);     
    mlx = fullfile(base, 'help', 'mlx', fname);       
    open_mlx = ['open ', mlx];
    eval(open_mlx);
end

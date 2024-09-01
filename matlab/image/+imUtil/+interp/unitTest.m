function [Result] = unitTest()
    % UnitTest for imUtil.interp
    % Example: imUtil.interp.unitTest
    
    Npix = 1700;
    XV = (1:Npix);
    YV = (1:Npix);
    [MatX,MatY] = meshgrid(XV, YV);

    MatXI = MatX + 0.3;
    MatYI = MatY + 0.6;
    XVI   = XV + 0.3;
    YVI   = YV + 0.6;

    V     = sin(2.*pi.*MatX./30) + 2.*cos(2.*pi.*MatY./30);
    PredV = sin(2.*pi.*MatXI./30) + 2.*cos(2.*pi.*MatYI./30);

    Nsim = 1;

    I1 = (5:1700-4)';
    I2 = (5:1700-4)';

    % imUtil.interp.interp2nearest_mex
    ZI  = interp2(MatX, MatY, V, XVI, YVI.','nearest');
    ZIm = imUtil.interp.interp2nearest_mex(MatX, MatY, V, XVI, YVI.');
    if max(abs(ZI-ZIm),[],'all')~=0
        error('imUtil.interp.interp2nearest_mex inconsistent with matlab interp2');
    end
    
    ZI  = interp2(MatX, MatY, single(V), XVI, YVI.','nearest');
    ZIm = imUtil.interp.interp2nearest_mex(MatX, MatY, single(V), XVI, YVI.');
    if max(abs(ZI-ZIm),[],'all')~=0
        error('imUtil.interp.interp2nearest_mex inconsistent with matlab interp2');
    end
    
    ZI  = interp2(MatX, MatY, uint32(V), XVI, YVI.','nearest');
    ZIm = imUtil.interp.interp2nearest_mex(MatX, MatY, uint32(V), XVI, YVI.');
    if max(abs(ZI-ZIm),[],'all')~=0
        error('imUtil.interp.interp2nearest_mex inconsistent with matlab interp2');
    end
    
    ZI  = interp2(MatX, MatY, uint16(V), XVI, YVI.','nearest');
    ZIm = imUtil.interp.interp2nearest_mex(MatX, MatY, uint16(V), XVI, YVI.');
    if max(abs(ZI-ZIm),[],'all')~=0
        error('imUtil.interp.interp2nearest_mex inconsistent with matlab interp2');
    end
    
    % performence
    tic;
    for I=1:1:100
        ZI  = interp2(MatX, MatY, V, XVI, YVI.','nearest');
    end
    toc
    
    tic;
    for I=1:1:100
        ZIm = imUtil.interp.interp2nearest_mex(MatX, MatY, V, XVI, YVI.');
    end
    toc
    Result = true;

end

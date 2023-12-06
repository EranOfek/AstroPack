function MatConv = sparse2fullConv(XG, YG, Mat, XI, YI)
    % very slow
    % Example: XG = (101:100:920); YG=XG;
    %          [XGM, YGM] = meshgrid((1:1:numel(XG)), (1:1:numel(YG)) );
    %          Mat = XGM.*YGM + rand(size(XGM)).*0.01;
    %          XI = (1:1:1000); YI=XI;
    %          imUtil.interp.sparse2fullConv(XG, YG, Mat, XI, YI);
    
    arguments
        XG
        YG
        Mat
        XI
        YI
    end
    
    % create a sparse version of the extended Mat
%     if isvector(XI) && isvector(YI)
%         [MatX, MatY] = meshgrid(XI, YI);
%     else
%         MatX = XI;
%         MatY = YI;
%     end
    
    MatFull = zeros(numel(YI), numel(XI));
    %[XGM, YGM] = meshgrid(XG, YG);
    MatFull(YG, XG) = Mat;
    
    DiffX = XG(2) - XG(1);
    DiffY = YG(2) - YG(1);
    
    Args.Gauss = [2.*DiffX, 2.*DiffY, 0];
    
    K       = imUtil.kernel2.box([2.*DiffX, 2.*DiffY]);
    MatConv = imUtil.filter.conv2_fast(MatFull, K, [], 'symmetric');
    
    
    if ~isempty(Args.Gauss)
        Gauss   = imUtil.kernel2.gauss(Args.Gauss, Args.Gauss(1:2).*3);
        MatConv = imUtil.filter.conv2_fast(MatConv, Gauss, [], 'symmetric');
    end
    
end
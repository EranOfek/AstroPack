function analyticThreshold(P, B)
    %
    % Example : P = imUtil.kernel2.gauss(2);
    %           B = 0.005; F = 5;
    %           Pp = imUtil.poissNoise.poissonMatchedFilter(P, B, F);
    
    Nlambda = 10;
    StepX   = 0.01;
    
    Vec = P(:).*B;
    
    Edges = logspace(log10(min(Vec)), log10(max(Vec)), Nlambda);
    LambdaCen  = (Edges(1:end-1) + Edges(2:end)).*0.5;
    Nlambda    = numel(LambdaCen);
    Nhist      = histcounts(Vec, Edges);
    
    X = (0:StepX:10).';
    Xker = (-3:StepX:3);
    Kernel = normpdf(Xker,0,1).';
    
    Nx = numel(X);
    
    Xmat = repmat(X,[1,Nlambda]);
    Lmat = repmat(LambdaCen,Nx,1);
    PDF  = poisspdf(Xmat, Lmat);
    PDFK = zeros(size(PDF));
    for Ilam=1:1:Nlambda
        PDFK(:,Ilam) = conv(PDF(:,Ilam),Kernel,'same');
    end
    
    ifft(prod(PDFK,2))
    
    
    
end

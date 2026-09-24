function [BinCenter, SFW, ErrSFW, SF] = sphericalSF(RA, Dec, RM, ErrRM, Args)
    % Structure function on the celestial sphere.
    %   Given a list of coordinates and measured values and their errors in
    %   these coordinates, calculate the spherical structure function.
    %   The weighted structure function is defined by:
    %       sum( (D^2 - Var) ./ Var )  /  sum( 1 ./ Var )
    %       where Var = Err_i^2 + Err_j^2.
    %
    % Input  : - A vector of RA [deg].
    %          - A vector of Dec [deg].
    %          - A vector of measured values, at the RA, Dec position.
    %          - A vector of error in the measured values.
    %          * ...,key,val,...
    %            'BinWidth' - Structure function bin width [deg].
    %                   Default is 0.05.
    %            'BinMax' - Structure function max distance [deg].
    %                   Default is 30.
    % Output : - (BinCenter) Angular bin centers [deg]
    %          - (SFW) Weighted unbiased structure function
    %          - (ErrSFW) Error on the weighted unbiased structure function
    %            Note that this is estimate may be underestimate due to
    %            multi counting of pairs. Better to use Bootstrap
    %            estimators.
    %          - (SF) Unweighted structure function
    % Author : Eran Ofek + ChatGPT (Mar 2026)
    % Example: [BinCenter, SFW, SF] = celestial.stat.sphericalSF(RA.*RAD, Dec.*RAD, RM, ErrRM);
    
    arguments
        RA (:,1) double
        Dec (:,1) double
        RM (:,1) double
        ErrRM (:,1) double
        Args.BinWidth (1,1) double = 0.05
        Args.BinMax   (1,1) double = 30
    end
    
    % ---------------------------------------------------------
    % Binning
    % ---------------------------------------------------------
    
    BinWidth  = Args.BinWidth;
    DistEdges = 0:BinWidth:Args.BinMax;
    BinCenter = (DistEdges(1:end-1) + DistEdges(2:end)) * 0.5;
    Nbin      = numel(BinCenter);
    
    % Accumulators
    SumD2  = zeros(Nbin,1);
    SumUnb = zeros(Nbin,1);
    SumW   = zeros(Nbin,1);
    Count  = zeros(Nbin,1);
    % for errors:
    SumW2X  = zeros(Nbin,1);   % sum w^2 * x
    SumW2X2 = zeros(Nbin,1);   % sum w^2 * x^2
    SumW2   = zeros(Nbin,1);   % sum w^2
    
    % ---------------------------------------------------------
    % Convert to radians
    % ---------------------------------------------------------
    
    RA  = deg2rad(RA);
    Dec = deg2rad(Dec);
    
    % Unit vectors
    X = cos(Dec).*cos(RA);
    Y = cos(Dec).*sin(RA);
    Z = sin(Dec);
    XYZ = [X Y Z];
    
    % ---------------------------------------------------------
    % KD-tree neighbor search
    % ---------------------------------------------------------
    
    Mdl = createns(XYZ,'NSMethod','kdtree');
    
    % Convert angular limit to chord distance
    ChordMax = 2 * sind(Args.BinMax/2);
    
    IdxList = rangesearch(Mdl, XYZ, ChordMax);
    
    N = numel(RA);
    
    % ---------------------------------------------------------
    % Main loop
    % ---------------------------------------------------------
    
    for I = 1:N
        
        Neigh = IdxList{I};
        Neigh = Neigh(Neigh > I);  % avoid double counting
        
        if isempty(Neigh)
            continue
        end
        
        % Exact spherical cosine
        CosTheta = X(I)*X(Neigh) + Y(I)*Y(Neigh) + Z(I)*Z(Neigh);
        
        % Numerical clipping (rare but safe)
        CosTheta = max(-1, min(1, CosTheta));
        
        Dist = acosd(CosTheta);
        
        % Restrict to BinMax
        Valid = Dist < Args.BinMax;
        if ~any(Valid)
            continue
        end
        
        Dist  = Dist(Valid);
        Neigh = Neigh(Valid);
        
        % -------- Fast bin index computation --------
        % Equivalent to your open interval logic
        Ind = floor(Dist / BinWidth) + 1;
        
        ValidBin = Ind >= 1 & Ind <= Nbin;
        if ~any(ValidBin)
            continue
        end
        
        Ind   = Ind(ValidBin);
        Neigh = Neigh(ValidBin);
        
        % Differences
        DVal = RM(I) - RM(Neigh);
        D2   = DVal.^2;
        
        Var = ErrRM(I).^2 + ErrRM(Neigh).^2;
        W   = 1 ./ Var;

        % for the errors:
        Xk = D2 - Var;      % x_k
        W2 = W.^2;
        
        % -------- Vectorized accumulation --------
        
        SumD2  = SumD2  + accumarray(Ind, D2, [Nbin 1], @sum, 0);
        SumUnb = SumUnb + accumarray(Ind, (D2 - Var)./Var, [Nbin 1], @sum, 0);
        SumW   = SumW   + accumarray(Ind, W, [Nbin 1], @sum, 0);
        Count  = Count  + accumarray(Ind, 1, [Nbin 1], @sum, 0);
        % for the errors:
        SumW2X  = SumW2X  + accumarray(Ind, W2 .* Xk,     [Nbin 1], @sum, 0);
        SumW2X2 = SumW2X2 + accumarray(Ind, W2 .* Xk.^2,  [Nbin 1], @sum, 0);
        SumW2   = SumW2   + accumarray(Ind, W2,           [Nbin 1], @sum, 0);

    end
    
    % ---------------------------------------------------------
    % Final estimators
    % ---------------------------------------------------------
    
    SF  = SumD2 ./ Count;
    SFW = SumUnb ./ SumW;

    % errors:
    ErrSFW = sqrt( ( SumW2X2 - 2.*SFW .* SumW2X + (SFW.^2) .* SumW2 ) ./ (SumW.^2) );

end
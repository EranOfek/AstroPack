function V = vignettingCircFilter(R, D, Args)
    % Calculate geometrical vignetting due to a circular filter.
    % Description:
    %   Calculate the fractional vignetting of an f/N converging beam by a
    %   circular filter located a distance D from the focal plane.
    %
    %   The calculation assumes:
    %   1. A centered circular filter.
    %   2. A circular geometrical beam footprint.
    %   3. The chief-ray displacement between the detector and filter planes
    %      is neglected.
    %
    % Input  : - R - Radial distance from field center [mm].
    %                  Can be a scalar, vector, or matrix.
    %          - D - Distance of filter from focal plane [mm].
    %                  Can be a scalar, vector, or matrix.
    %                  R and D must have compatible sizes for MATLAB implicit
    %                  expansion.
    %          * ...,key,val,...
    %            'FNumber'      - Telescope focal ratio. Default is 2.2.
    %            'FilterRadius' - Filter radius [mm]. Default is 20.
    %
    % Output : - V - Fractional vignetting, in the range [0,1].
    %                V=0 means no vignetting.
    %                V=1 means complete vignetting.
    %
    % Author : Eran Ofek + ChatGPT (2026 Aug)
    % Example:
    %   R = (0:1:17).';
    %   V = vignettingFilter(R,34);
    %
    %   % Vignetting in percent:
    %   Vperc = 100.*telescope.geometry.vignettingFilter(R,34);
    %
    %   % Two-dimensional grid in R and D:
    %   R = (0:1:20).';
    %   D = 20:5:50;
    %   V = telescope.geometry.vignettingCircFilter(R,D);
    %
    %
    %   [MatX,MatY]=meshgrid((1:1:36)-18,(1:1:24)-12); MatR=sqrt(MatX.^2+MatY.^2);
    %   V=telescope.geometry.vignettingCircFilter(MatR,34);                           
    %   mean(1-V,[1 2])


    arguments
        R
        D
        Args.FNumber      = 2.2
        Args.FilterRadius = 20
    end
    
    N  = Args.FNumber;
    Rf = Args.FilterRadius;
    
    % Beam footprint radius at filter plane:
    A = D./(2.*N);
    
    % Determine common size using MATLAB implicit expansion:
    Tmp = R + A;
    
    % Explicitly expand R and A to common size.
    % This is required because logical indexing does not perform
    % implicit expansion.
    R = R + zeros(size(Tmp),'like',Tmp);
    A = A + zeros(size(Tmp),'like',Tmp);
    
    % Initialize:
    V = zeros(size(Tmp),'like',Tmp);
    
    % Fully vignetted and partially vignetted regions:
    FlagFull = R >= (Rf + A);
    FlagPart = R > (Rf - A) & R < (Rf + A);
    
    % Complete vignetting:
    V(FlagFull) = 1;
    
    % Partial overlap:
    Rp = R(FlagPart);
    Ap = A(FlagPart);
    
    X1 = (Rp.^2 + Ap.^2 - Rf.^2) ./ (2.*Rp.*Ap);
    X2 = (Rp.^2 + Rf.^2 - Ap.^2) ./ (2.*Rp.*Rf);
    
    % Protect acos against numerical round-off:
    X1 = max(-1,min(1,X1));
    X2 = max(-1,min(1,X2));
    
    
    Term = (-Rp + Ap + Rf) .* ...
           ( Rp + Ap - Rf) .* ...
           ( Rp - Ap + Rf) .* ...
           ( Rp + Ap + Rf);
    
    Term = max(Term,0);
    
    Aov = Ap.^2 .* acos(X1) + ...
          Rf.^2 .* acos(X2) - ...
          0.5 .* sqrt(Term);
    
    V(FlagPart) = 1 - Aov./(pi.*Ap.^2);
    
    % Protect against small numerical excursions:
    V = max(0,min(1,V));

end


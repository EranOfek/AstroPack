function Result=moment1d(Array, Pos, Args)
    % Calculate the 1st and 2nd 1-D moments on columns or rows of matrix
    %   Given an array in which each colum (or row) is treated as a 1-D
    %   vector for which the 1st and 2nd central moments are needed,
    %   calculate these moments iteratively
    %   The function calculates both the moments and the weighted moments,
    %   where the weighted moments are weighted by a Gaussian with width
    %   defined by the 'WeightSigma paramaeter.
    %   The weighted 1st moment should be used as it is much more accurate.
    % Input  : - A matrix or vector.
    %          - Initial position of the 1st moment positin.
    %            If Dim=1, this is the Y position (I-coordinate) of the initial
    %            guess fisrt moment in each column of the matrix.
    %            If scalar, then assume all columns have the same initial
    %            position.
    %          * ...,key,val,...
    %            'Dim' - Dimension over to calculate the moments.
    %                   For spectra, this will be the spatial dimension.
    %                   Default is 1.
    %            'WinHalfSize' - Half size of window in which to calculate
    %                   the moments.
    %                   Default is 7.
    %            'MaxIter' - Maximum number of iterations.
    %                   Default is 10.
    %            'WeightSigma' - The Gaussian sigma-width used in the
    %                   calculation of the weighted moments.
    % Output : - A structure with the following fields:
    %            .X1 - First central moment for each colum (row, if Dim=2) in the input
    %                   matrix.
    %            .X2 - Second central moment for each colum (row, if Dim=2) in
    %                   the input matrix.
    %            .X1W - Weighted 1st moment.
    %            .X2W - Weighted 2nd moment.
    %            .Xinit - Initial X position.
    % Author : Eran Ofek (May 2023)
    % Example: % No noise - initail pos is best pos
    %          Yg = normpdf((-100:1:100),0,2)';
    %          Array = Yg.*ones(1,200);
    %          Result=imUtil.spec.trace.moment1d(Array, 101);
    %          % no noise - initial pos is not best pos
    %          Result=imUtil.spec.trace.moment1d(Array, 101+randn(1,200).*1);
    %          max(abs(Result.X1-101))  % max error
    %          % with noise
    %          Result=imUtil.spec.trace.moment1d(Array+randn(size(Array)).*0.001, 101+randn(1,200).*1);
    %          max(abs(Result.X1-101))  % max error
    %          max(abs(Result.X1W-101))  % max error using weighted moments
    
    arguments
        Array
        Pos
        Args.Dim           = 1;
        Args.WinHalfSize   = 7;
        Args.MaxIter       = 10;
        Args.WeightSigma   = 3;
        %Args.WeightFun     = 
        %Args.WeightFunArgs = 
        
    end
    
    if Args.Dim==2
        Array = Array.';
    end
    
    [Nspat, Nwave] = size(Array);
    if numel(Pos)==1
        Pos = Pos.*ones(1, Nwave);
    end
    
    if numel(Pos)~=Nwave
        error('Number of Pos elemenst must be equal to the number of wavelength');
    end
    
    RoundedPos = round(Pos);
    
    Cutout = imUtil.spec.trace.image2cutouts1d(Array, Pos, 'Dim',1, 'WinHalfSize',Args.WinHalfSize);
    
    Center = Args.WinHalfSize + 1;
    Norm   = sum(Cutout, 1);
    
    Iiter      = 0;
    Result.X1  = 0;
    Result.X1W = 0;
    
    Cont       = true;
    while Cont
        Iiter = Iiter + 1;
        % X1 is a row vector and X is a column vector
        Xcoo   = (-Args.WinHalfSize:1:Args.WinHalfSize).' - Result.X1;
    
        
        
        % Calculate the non-weighted 1st and 2nd moments:
        
        XC     = Xcoo.*Cutout;
        % 1st moment
        X1   = sum(XC, 1)./Norm;
        Result.X1   = Result.X1 + X1;
        % 2nd moment
        Result.X2   = sum(Xcoo.*XC)./Norm;
       
        % calculate the weighted moments:
        
        % Weight function
        XcooW   = (-Args.WinHalfSize:1:Args.WinHalfSize).' - Result.X1W;
        W      = normpdf(XcooW, 0, Args.WeightSigma);
        NormW  = sum(Cutout.*W, 1);
        XCW    = XcooW.*Cutout.*W;
        X1W    = sum(XCW, 1)./NormW;
        Result.X1W = Result.X1W + X1W;
        % 2nd moment
        Result.X2W   = sum(Xcoo.*XCW)./NormW;
       
        
        if Iiter>=Args.MaxIter
            Cont = false;
        end
        
        
    end
    Result.X1    = RoundedPos + Result.X1;
    Result.X1W   = RoundedPos + Result.X1W;
    Result.Xinit = Pos;
    
    
end
    
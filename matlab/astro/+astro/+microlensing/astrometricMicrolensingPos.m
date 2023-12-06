function Result = astrometricMicrolensingPos(Args)
    % Calculate the unresolved images position in astrometric microlenisng
    %   The output is calculated in the coordinate system in which the X
    %   axis is the direction of the proper motion.
    %   The Y axis is in the direction of the source from the lens).
    % Input  : * ...,key,val,...
    %            'T' - Vector of times [days].
    %                   Default is (-1000:10:1000).'
    %            'BetaMin' - Min. impact parameter [ThetaE units]
    %            'PM' - Proper motions [ThetaE/day]
    %            'T0' - t0. Default is 0.
    % Output : - A structure with:
    %            .T
    %            .T0
    %            .Delta - the source light deflection of the images, as
    %                     measured relative to the source in the direction
    %                     of the lens.
    %            .X - The X position of the image in respect to the source.
    %               Where X dir is in the direction of the motion.
    %               [ThetaE units]
    %            .Y 
    %            .Xl - in respect to the stationary lens
    %            .Yl
    % Author : Eran Ofek (Jun 2022)
    % Example: Result = astro.microlensing.astrometricMicrolensingPos
   
    arguments
        Args.T         = (-1000:1:1000).';
        Args.BetaMin   = 1;         % ThetaE
        Args.PM        = 1./30;     % ThetaE/day
        Args.T0        = 0;         % day
    end
    
    Mu = sqrt((Args.PM.*(Args.T - Args.T0)).^2 + Args.BetaMin.^2).*sign(Args.T - Args.T0);
    PA   = atan(Args.PM.*(Args.T - Args.T0)./Args.BetaMin);  % Position Angle [rad]
    
    % In the thin screen, point mass, small angle approximation, the
    % center-of-light of the source light deflection of the images, as
    % measured relative to the source in the direction of the lens, is:
    % (two images combined)
    % L -> S -> I
    
    %ThetaE = 1;
    
    %Mu = Beta./ThetaE;
    
    Delta = Mu./(Mu.^2 + 2);  % [ThetaE units]
    
    X = Delta.*sin(PA);
    Y = Delta.*cos(PA);
    
    Result.T     = Args.T;
    Result.T0    = Args.T0;
    Result.Delta = Delta;
    Result.X     = X;
    Result.Y     = Y;
    
    Result.Xl    = X + Mu;
    Result.Yl    = Y + Args.BetaMin;
end
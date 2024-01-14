function [Imu,Mu,R]=limbDarkening(Coef, Mu, Args)
    % Return the limb darkening value as a function of mu and coef.
    %   The function supports various types of limb darkening models (e.g.,
    %   'linear',...'4par').
    % Input : - A vector of coef.
    %         - Mu = cos(theta), where θ is the angle
    %           between the line of sight and the outward surface normal
    %           Alternatively, this can be specified in units of theta
    %           or the star radius (normlaized to 1).
    %           (see MuUnits argument).
    %           Default is (0:0.01:1)';
    %         * ...,key,val,...
    %           'Fun' - Model name:
    %                   'linear'
    %                   'quadratic'
    %                   'squareroot'
    %                   'log'
    %                   'pow2'
    %                   '4par'
    %           'MuUnits' - The units of mu: 'mu'|'theta'|'r'
    % Output : - Limb darkening
    %          - Requested Mu
    %          - Corresponding r (=sqrt(1 - Mu^2))
    % Author : Eran Ofek (Oct 2023)
    % Example: [Coef, F] = astro.stars.getClaret2020_LimbDarkeningWD(10000,[7]);
    %          [Imu,Mu,R] = astro.stars.limbDarkening(Coef);
    
    arguments
        Coef
        Mu       = (0:0.01:1)';        
        Args.Fun      = '4par';   % linear|quadratic
        Args.MuUnits  = 'mu';   % 'mu'|'theta'|'r' - mu=cos(theta), r=sin(theta)
    end
    
    % note that mu=1 corresponds to the center of the star
    switch lower(Args.MuUnits)
        case 'mu'
            % already in mu
            Mu = Mu;
        case 'theta'
            % mu = cos(theta)
            Mu = cos(Mu);
        case 'r'
            % r = sin(theta) = sqrt(1-cos^2(theta)) = sqrt(1-Mu^2)
            % r^2 = 1-Mu^2 -> Mu^2 = 1 - r^2
            Mu = sqrt(1-Mu.^2);
        otherwise
            error('Unknown MuUnits option');
    end
    
    if nargout>2
        R = sqrt(1 - Mu.^2);
    end
    
    %Note that: Imu = I(\mu)/I(1)
    switch lower(Args.Fun)
        case 'linear'
            % The linear law (Schwarzschild 1906; Russell 1912; Milne 1921)
            Imu = 1 - Coef(1).*(1 - Mu);
        case 'quadratic'
            % The quadratic law: Kopal 1950
            Imu = 1 - Coef(1).*(1 - Mu) - Coef(2).*(1 - Mu).^2;
        case 'squareroot'
             % The square-root law (Díaz-Cordovés & Giménez 1992)
             Imu = 1 - Coef(1).*(1 - Mu) - Coef(2).*(1 - sqrt(Mu));
        case 'log'
            % The logarithmic law (Klinglesmith & Sobieski 1970)
            Imu = 1 - Coef(1).*(1 - Mu) - Coef(2).*Mu.*log(Mu);
        case 'pow2'
            % The power-2 law (Hestroffer 1997)
            Imu = 1 - Coef(1).*(1 - Mu.^Coef(2));
        case '4par'
            % The four-term law (Claret 2000a)
            K   = (1:1:numel(Coef));
            Imu =  1 - Coef(1).*(1 - Mu).^0.5 - ...
                       Coef(2).*(1 - Mu).^1 - ...
                       Coef(3).*(1 - Mu).^1.5 - ...
                       Coef(4).*(1 - Mu).^2;   
        case 'constant'
            Imu = 1.*ones(size(Mu));
        otherwise
            error('Unknown Fun option');
    end
end

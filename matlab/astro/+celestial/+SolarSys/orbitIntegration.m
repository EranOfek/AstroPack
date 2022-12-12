function [X,V] = orbitIntegration(JD,X0,V0,Args)
    % Calculate the position and velocity evolution for a list of asteroids 
    % Input  :  - Vector of initial and final JD.
    %           - Target asteroids position vector in au.
    %            This is a 3 X Nobj matrix, where 3 is the number of
    %            coordinates, and Nobj the number of objects.
    %          - Target asteroids velocity vector in au / day, same size as X0.
    %          * ...,key,val,...
    %            'RelTol' - Relative tolerance of the ODE
    %            'AbsTol' = Absolute tolerance of the ODE
    % Output : - A 3 X Nobj matrix containing the position at the requested
    %            time.
    %          - A 3 X Nobj matrix containing the velocity at the requested
    %            time.
    % Author : Amir Sharon (April 2022)
    % Example: 

    arguments
        JD
        X0
        V0
        Args.RelTol     = 1e-10; 
        Args.AbsTol     = 1e-10;
    end

    Nobj = size(X0,2);
    opts = odeset('RelTol',Args.RelTol,'AbsTol',Args.AbsTol );

    if JD(1)~=JD(2)
        InitialValues = [X0;V0];
    
        [Times,Result] = ode45(@(T,XVmat) odeDirectVectorized(T,XVmat,Nobj), JD, InitialValues,opts);
    
        FinalValues = reshape(Result(end,:),[],Nobj);
        
        X = FinalValues(1:3,:);
        V = FinalValues(4:6,:);
    else
        X = X0;
        V = V0;
    end

end



function DXVDt = odeDirectVectorized(T,XVmat,Nobj)

  XVmat = reshape(XVmat,[],Nobj);
  DXVDt = zeros(6,Nobj);
  DXVDt(1:3,:) = XVmat(4:6,:);

  DXVDt(4:6,:) = celestial.SolarSys.ple_force(XVmat(1:3,:), T,'EqJ2000',true);

  DXVDt(4:6,:) = DXVDt(4:6,:)  -  celestial.SolarSys.ple_force([0 0 0]', T,'EqJ2000',false);

  DXVDt = DXVDt(:);
end
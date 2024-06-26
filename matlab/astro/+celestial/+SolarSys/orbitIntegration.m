function [X,V] = orbitIntegration(JD, X0, V0, Args)
    % Calculate the position and velocity evolution for a list of bodies
    %   in the Solar System includeing perturbations from all planets.
    %   This include only the integration between two points in time (i.e.,
    %   no observer and light time correction).
    %   The integration is done in the equatorial J2000 reference frame.
    % Input  : - Vector of initial and final JD.
    %          - Target asteroids position vector in au.
    %            This is a 3 X Nobj matrix, where 3 is the number of
    %            coordinates, and Nobj the number of objects.
    %            Barycentric J2000 equatorial ref. frame.
    %          - Target asteroids velocity vector in au / day, same size as X0.
    %            Barycentric J2000 equatorial ref. frame.
    %          * ...,key,val,...
    %            'RelTol' - Relative tolerance of the ODE.
    %                   Default is 1e-10.
    %            'AbsTol' = Absolute tolerance of the ODE
    %                   Default is 1e-10.
    %            'INPOP' - A populated celestial.INPOP object.
    %                   If provided then will use the forceAll method to
    %                   calculate the force by all planets and the Sun.
    %                   If empty, then will use celestial.SolarSys.ple_force
    %                   Default is [].
    %            'TimeScale' - INPOP integration time scale.
    %                   Default is 'TDB'.
    % Output : - A 3 X Nobj matrix containing the position at the requested
    %            time.
    %          - A 3 X Nobj matrix containing the velocity at the requested
    %            time.
    % Author : Amir Sharon (April 2022)
    % Example: [X,V] = celestial.SolarSys.orbitIntegration([2451545 2451546],[1 1 1]',[0.001 0.001 0.001]')
    %          [X,V] = celestial.SolarSys.orbitIntegration([2451545 2451546],[1 2; 1 1; 1 3],[0.001 0.001; 0.001 0.001; -0.001 0.001])
    %
    %          Ts = 2460000;
    %          Te = 2463000;
    %          IN = celestial.INPOP.init;
    %          [T,~,U] = celestial.SolarSys.getJPL_ephem('299;','EPHEM_TYPE','VECTORS','TimeScale','TT','StartTime',Ts,'StopTime',Te,'StepSize',100);
    %          XJ  = celestial.INPOP.ecliptic_2eqJ2000([T.X.'; T.Y.'; T.Z.']);
    %          VJ  = celestial.INPOP.ecliptic_2eqJ2000([T.VX.'; T.VY.'; T.VZ.']);
    %          [X,V] = celestial.SolarSys.orbitIntegration([Ts Te],XJ(:,1), VJ(:,1), 'INPOP',IN);

    arguments
        JD
        X0
        V0
        Args.RelTol     = 1e-10; 
        Args.AbsTol     = 1e-10;
        Args.INPOP      = [];   % if empty use celestial.SolarSys.ple_force
        Args.TimeScale  = 'TDB';
        %Args.RefFrame   = 'eq';
    end

    
    if JD(1)~=JD(2)
        Nobj = size(X0,2);
        
        Opts = odeset('RelTol',Args.RelTol, 'AbsTol',Args.AbsTol);

        if size(X0,2)==1
            Method = 'rkn1210';
        else
            Method = 'ode45';
        end
        %Method = 'ode45';
        %Method = 'rknmex';
        %Method = 'rkn1210'
        switch Method
            case 'ode45'
                InitialValues = [X0;V0];
                [Times,Result] = ode45(@(T,XVmat) odeDirectVectorized(T,XVmat,Nobj,Args.INPOP, Args.TimeScale), JD, InitialValues, Opts);
                FinalValues = reshape(Result(end,:),[],Nobj);

                X = FinalValues(1:3,:);
                V = FinalValues(4:6,:);
            case 'rkn86'
                [Times, X, V] = tools.math.ode.rkn86(@(T,XVmat) odeSecondOrder(T,XVmat,Nobj,Args.INPOP, Args.TimeScale),...
                                                       JD(1), JD(2), X0, V0, Args.RelTol);
                X = X(end,:).';
                V = V(end,:).';
            case 'rkn1210'

                [Times, X, V] = tools.math.ode.rkn1210(@(T,XVmat) odeSecondOrder(T,XVmat,Nobj,Args.INPOP, Args.TimeScale),...
                                                        [JD(1), JD(2)], X0, V0, Opts);

                 X = X(end,:).';
                 V = V(end,:).';
            case 'rknmex'
                [Times, X, V] = RKN_multithreaded(@(T,XVmat) odeSecondOrder(T,XVmat,Nobj,Args.INPOP, Args.TimeScale),...
                                                        [JD(1), JD(2)], X0, V0, Opts);

                 X = X(end,:).';
                 V = V(end,:).';
            case 'ode15s'
                InitialValues = [X0;V0];
                
                % try stiff equation solver / no change
                [Times,Result] = ode15s(@(T,XVmat) odeDirectVectorized(T,XVmat,Nobj,Args.INPOP, Args.TimeScale), JD, InitialValues, Opts);
                FinalValues = reshape(Result(end,:),[],Nobj);

                X = FinalValues(1:3,:);
                V = FinalValues(4:6,:);
            otherwise
                error('Unknown Method option');
        end
        
    else
        X = X0;
        V = V0;
    end

end



function DXVDt = odeDirectVectorized(T,XVmat,Nobj, ObjINPOP, TimeScale)
    % DXVDt - elements 1:3 contains:
    %           \dot{X} = V
    %         elements 4:6 contains:
    %           \dot{V} = sum force
    
    XVmat = reshape(XVmat,[],Nobj);
    DXVDt = zeros(6,Nobj);
    DXVDt(1:3,:) = XVmat(4:6,:);

    if isempty(ObjINPOP)
        DXVDt(4:6,:) = celestial.SolarSys.ple_force(XVmat(1:3,:), T,'EqJ2000',true);
    else
        DXVDt(4:6,:) = ObjINPOP.forceAll(T, XVmat(1:3,:), 'IsEclipticOut',false, 'OutUnits','au', 'TimeScale',TimeScale);
    end

    % For Heliocentric frame you need the following line:
    % DXVDt(4:6,:) = DXVDt(4:6,:)  -  celestial.SolarSys.ple_force([0 0 0]', T,'EqJ2000',false);

    DXVDt = DXVDt(:);
end


function DXDt = odeSecondOrder(T,XVmat,Nobj, ObjINPOP, TimeScale)
    %
    
    if isempty(ObjINPOP)
        DXDt(1:3,:) = celestial.SolarSys.ple_force(XVmat(1:3,:), T,'EqJ2000',true);
    else
        DXDt(1:3,:) = ObjINPOP.forceAll(T, XVmat(1:3,:), 'IsEclipticOut',false, 'OutUnits','au', 'TimeScale',TimeScale);
    end
    

end
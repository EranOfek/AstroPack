function MinDist = minDist_PointArc(P,R,Q, Units)
    % Calculate the min distance between a point and a great circle
    %   The great circle is defined by two points (Q and R) on the sphere.
    % Input  : - [Long, Lat] of a point P.
    %          - [Long, Lat] of q apoint R
    %          - [Long, Lat] of q apoint Q.
    %          - Input/output units: 'deg'|'rad'. Default is 'rad'.
    % Output : - Min. angular dustance (in Units) between the point P
    %            and great circle defined by Q and R.
    % Author : Eran Ofek (Mar 2023)
    % Example: MinDist = celestial.coo.minDist_PointArc([1 1;2 1],[0 1],[1,1.1])
    
    arguments
        P
        R
        Q
        Units   = 'rad';
    end
    
    Conv = convert.angular(Units, 'rad',1);
    P    = P.*Conv;
    Q    = Q.*Conv;
    R    = R.*Conv;
    
    
    [P1, P2, P3] = celestial.coo.coo2cosined(P(:,1),P(:,2));
    [Q1, Q2, Q3] = celestial.coo.coo2cosined(Q(:,1),Q(:,2));
    [R1, R2, R3] = celestial.coo.coo2cosined(R(:,1),R(:,2));
   
    Pc = [P1, P2, P3];
    Qc = [Q1, Q2, Q3];
    Rc = [R1, R2, R3];
    
    Np = size(Pc,1);
    Qc = repmat(Qc,Np,1);
    Rc = repmat(Rc,Np,1);
    
    
    QcR  = cross(Qc,Rc,2);  % Q x R
    Norm = sqrt(sum(QcR.^2, 2));
    MinDist = abs(asin(dot(Pc, QcR, 2)./Norm));
    
    MinDist = MinDist./Conv;
    
end
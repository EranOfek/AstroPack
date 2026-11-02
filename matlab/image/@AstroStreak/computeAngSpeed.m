function [speed,JDExtremes]=computeAngSpeed(A,direction)
% Compute values for the property AngSpeed, and JDExtremes if all necessary information
%  is given; that is, neither extreme is close to the edgge, and a
%  direction is assigned (direction>0 --> track goes from X1 to X2 with
%  time; direction <0, viceversa)
% The values are not immediately assigned in place to the corresponding
%  properties of A, because we may want to run this function for tests,
%  without overwriting if the test fails 

    RAD=180/pi;

    if ~any(A.IsEdge)
        % rolling shutter correction depending on direction
        Y=A.Y(A.ExtentIndices);
        if direction>0
            JDExtremes=[...
                imUtil.image.rollingShutterTime(A.JD(1),Y(1)+A.Yoffset),...
                imUtil.image.rollingShutterTime(A.JD(2),Y(2)+A.Yoffset) ];
        else
            JDExtremes=[...
                imUtil.image.rollingShutterTime(A.JD(2),Y(1)+A.Yoffset),...
                imUtil.image.rollingShutterTime(A.JD(1),Y(2)+A.Yoffset) ];
        end
        dt=diff(JDExtremes)*86400;
        speed=celestial.coo.sphere_dist_fast( ...
                A.RAExtremes(1)/RAD, A.DecExtremes(1)/RAD, ...
                A.RAExtremes(2)/RAD, A.DecExtremes(2)/RAD)*...
                RAD*3600/dt;
    else
        speed=NaN;
    end

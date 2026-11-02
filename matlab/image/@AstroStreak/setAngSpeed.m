function setAngSpeed(A,direction)
% assign a value to the property AngSpeed, and JDExtremes if all necessary information
%  is given; that is, neither extreme is close to the edgge, and a
%  direction is assigned (direction>0 --> track goes from X1 to X2 with
%  time; direction <0, viceversa)
    [A.AngSpeed,A.JDExtremes]=computeAngSpeed(A,direction);

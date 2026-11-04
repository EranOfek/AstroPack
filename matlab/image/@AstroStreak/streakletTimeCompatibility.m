function compatible = streakletTimeCompatibility(A1,A2,Args)
% check if a couple of streaklets are time compatible, in the sense that
%  their time stamps and extents are such that they are sampled from
%  the same trajectory. For that, it has to be possible to define a signed 
%  speed for at least one of the two streaklets; which is possible if they
%  don't have edge extremes.
% Near collinearity of A1 and A2 is assumed, and is not checked by this
%  function.
% If a speed cannot be defined for either streaklet, declare them
%  compatible in the following cases:
%  - their along track spans overlap and their time spans overlap
%  - ...
% Corrects in place the streaklets A1 and A2, if they were found
%  compatible, that is objects in which the speed is updated, the time extent
%  JD is corrected for rolling shutter effects, if 'Update' is true
arguments
    A1 AstroStreak
    A2 AstroStreak
    Args.SpeedTolerance = 0.2; % fraction
    Args.Update = false; % update the streaks with JDextremes and AngSpeed if possible
    Args.TimeJitter = 50; % in ms; tolerance, because our timestamping is not exact
end

RAD=180/pi;

[angdist,along1,along2,ca]=imUtil.streaks.crossTrackAngularDistance(...
                                A1.RAExtremes/RAD,A1.DecExtremes/RAD, ...
                                A2.RAExtremes/RAD,A2.DecExtremes/RAD);

% enumeration of many cases - whch grouping is more readable and clever?
compatible=false;
if (all(along1>0) && all(along2>0))
    % A1. A1. A2. A2.
    if all(A2.JD>A1.JD(2)-Args.TimeJitter/86400000)
        % disjoint exposure intervals, 2 after 1
        dir1=sign(along2(1)-along1(1));
        dir2=sign(along1(2)-along1(1));
        % now we should check if speeds are consistent with displacements
        [speed1,JDExtremes1]=A1.computeAngSpeed(dir1);
        compatible=true;
    elseif all(A2.JD==A1.JD)
        % likely different crops of the same image
        % One could either accept all (rely separately on collinearity to
        %  filter out), or accept only if both streaklets share an edge
        %  point (but then we may discard incompletely detected streaks)
        compatible=true;
    end
    % A1. A2. A1. A2.
    % A1. A2. A2. A1.
    % A2. A1. A2. A1.
    % A2. A2. A1. A1.
    % A2. A1. A1. A2.
end
    
if compatible && Args.Update
    % update in place
    A1.setAngSpeed(dir1)
    A2.setAngSpeed(dir2)
end
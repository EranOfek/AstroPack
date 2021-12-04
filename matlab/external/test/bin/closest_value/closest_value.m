function [v, inf] = closest_value(arr, val)
% Returns value and index of arr that is closest to val. If several entries
% are equally close, return the first. Works fine up to machine error (e.g.
% [v, i] = closest_value([4.8, 5], 4.9) will return [5, 2], since in float
% representation 4.9 is strictly closer to 5 than 4.8).
% ===============
% Parameter list:
% ===============
% arr : increasingly ordered array
% val : scalar in R


len = length(arr);
inf = 1;
sup = len;

% Binary search for index
while sup - inf > 1
    med = floor((sup + inf)/2);
    
    % Replace >= here with > to obtain the last index instead of the first.
    if arr(med) >= val 
        sup = med;
    else
        inf = med;
    end
end

% Replace < here with <= to obtain the last index instead of the first.
if sup - inf == 1 && abs(arr(sup) - val) < abs(arr(inf) - val)
    inf = sup;
end  

v = arr(inf);
end

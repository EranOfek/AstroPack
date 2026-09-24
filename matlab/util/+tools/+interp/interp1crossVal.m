function [Result] = interp1crossVal(Pos, Vec, CrossVal, IsAscending, FirstLast, Algo)
    % Return the interpolated position where a monotonic sampled vector crosses a specified value.
    %       Interpolate the X position at which a monotonic vector crosses a given Y value.
    %       The function searches for the first or last crossing of CrossVal in Vec and
    %       returns the corresponding interpolated position in Pos using linear interpolation
    %       between the two neighboring samples. The direction of monotonicity can be
    %       provided explicitly or detected automatically from Vec
    % Input  : - A vector of positions - X coordinates.
    %          - A vector of values - Y coordinates.
    %            Note that this values must be monotonic, and if they are
    %            not (e.g., due to noise), the function will force
    %            montonicity on the data.
    %          - Y Crossing value. 
    %          - True for asending Y values.
    %            False for descening Y values.
    %            [] for automatic detection.
    %            Defaut is [].
    %          - Find 'first' | 'last' point. Default is 'first'.
    %          - Alogorithm: 'interp'|'find'. Default is 'interp'.
    % Output : - The interpolated position of the Y crossing value.
    % Author : Eran Ofek (2026 Apr) 
    % Example: R=tools.interp.interp1crossVal([1 2 3 4 5],[0.3 0.4 0.5 0.6 0.9], 0.5)

    arguments
        Pos
        Vec
        CrossVal
        IsAscending         = []
        FirstLast           = 'first';
        Algo                = 'find';% 'interp'; % 'interp' | 'find'
    end

    if isempty(IsAscending)
        if (Vec(end)-Vec(1))>0
            IsAscending = true;
        else
            IsAscending = false;
        end
    end

    % Force monotonicity on the data
    % See issue #966
    Vec = Vec(:);
    Pos = Pos(:);
    Diff = [0;diff(Vec)];
    if IsAscending
        Sign = 1;
        Diff(Diff>0) = 0;
    else
        Sign = -1;
        Diff(Diff<0) = 0;
    end
    Vec = Vec - Sign.*cumsum(Diff);

    switch Algo
        case 'interp'
            N = numel(Vec);
            EpsVec = (1:1:N).*1e-7;
            Result = interp1(Vec(:)+Sign.*EpsVec(:), Pos(:), CrossVal);
            
        case 'find'
            if IsAscending
                I = find(Vec>CrossVal, 1, FirstLast);
                if isempty(I)
                    Result = Pos(end);
                else
                    if I==1
                        Result = CrossVal./Vec(I);
                    else
                        DY = Vec(I) - Vec(I-1);
                        DX = Pos(I) - Pos(I-1);
                        Result = Pos(I-1) + (CrossVal - Vec(I-1)).*DX./DY;
                    end
                end
            else
               Vec = rot90(Vec,2);
               Pos = rot90(Pos,2);
               I = find(Vec>CrossVal, 1, FirstLast);
                if isempty(I)
                    Result = Pos(end);
                else
                    if I==1
                        Result = CrossVal./Vec(I);
                    else
                        DY = Vec(I) - Vec(I-1);
                        DX = Pos(I) - Pos(I-1);
                        Result = Pos(I-1) + (CrossVal - Vec(I-1)).*DX./DY;
                    end
                end
        
            end
        otherwise
            error('Unknown Algo option');
    end

end

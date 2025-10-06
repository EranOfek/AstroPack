function [C] = rectangularMaskCorners(B, Args)
    % Return the four corners of a single-filled rectangular region og true in a logical matrix.
    %   Given a logical matrix of true/false, in which there is a
    %   rectangular (maybe rotated) shape containing true. The function
    %   return the four corners of the rectangular shape
    % Input  : - A matrix of logicals.
    %          * ...,key,val,...
    %            'UseMex' - A logical indicating if to use the fast mex
    %                   version imUtil.mask.mex.rectangularMaskCorners_mex
    %                   Default is false.
    % Output : - A two column matrix [Y, X] containing the four corners.
    % Author : ChatGPT + Eran Ofek (2025 Oct) 
    % Example: Corners=imUtil.mask.rectangularMaskCorners(Flag)

    arguments
        B
        Args.UseMex = false;
    end

    if Args.UseMex
        C = imUtil.mask.mex.rectangularMaskCorners_mex(B);
    else
    
        %RECTCORNERSFROMMASK  Four corners of a single filled rectangular region (may be rotated).
        % Input :
        %   B  - Logical matrix with exactly one contiguous, filled rectangle of true; rest false.
        % Output:
        %   C  - 4x2 matrix of [Row Col] corners, clockwise order (rounded to nearest pixel).
        %   IsAxisAligned - Logical flag indicating an axis-aligned rectangle fast path was used.
        
        % Defaults
        C = zeros(0, 2);
        IsAxisAligned = false;
        
        % Guard: Empty or no true pixels
        HasAny = ~isempty(B) && any(B(:));
        if ~HasAny
            return
        end
        
        % ---------- Fast Axis-Aligned Path (O(N)) ----------
        RowHas = any(B, 2);
        ColHas = any(B, 1);
        
        HasRows = any(RowHas);
        if HasRows
            Row1 = find(RowHas, 1, 'first');
            Row2 = find(RowHas, 1, 'last');
            Col1 = find(ColHas, 1, 'first');
            Col2 = find(ColHas, 1, 'last');
        
            % Verify all rows in [Row1:Row2] have a single contiguous run equal to [Col1:Col2]
            IsAligned = true;
            for R = Row1:Row2
                RowV = B(R, :);
                if ~any(RowV)
                    IsAligned = false;
                    break
                end
                Cl = find(RowV, 1, 'first');
                Cr = find(RowV, 1, 'last');
                if any(~RowV(Cl:Cr)) || Cl ~= Col1 || Cr ~= Col2
                    IsAligned = false;
                    break
                end
            end
        
            if IsAligned
                % Corners [Row Col], clockwise
                C = [Row1, Col1;
                     Row1, Col2;
                     Row2, Col2;
                     Row2, Col1];
                IsAxisAligned = true;
            end
        end
        
        % If not axis-aligned, continue to oriented rectangle
        if ~IsAxisAligned
            % ---------- Boundary Extraction (perimeter-sized set) ----------
            % 8-neighborhood sum; boundary pixels have sum < 9
            K = conv2(double(B), ones(3), 'same');
            Boundary = B & (K < 9);
            [Ry, Cx] = find(Boundary);
            if numel(Ry) < 4
                [Ry, Cx] = find(B);  % Fallback if boundary too small (e.g., tiny blob)
            end
        
            % ---------- Convex Hull ----------
            % Note: convhull expects x=columns, y=rows
            Idx = convhull(Cx, Ry);
            Xh = Cx(Idx);
            Yh = Ry(Idx);
            Nh = numel(Idx);
        
            % ---------- Rotating Calipers: Minimum-Area Bounding Rectangle ----------
            BestArea = inf;
            BestCorners = [];
        
            for I = 1:Nh-1
                Dx = double(Xh(I+1) - Xh(I));
                Dy = double(Yh(I+1) - Yh(I));
                Len = hypot(Dx, Dy);
                if Len == 0
                    continue
                end
                Ux = Dx / Len;  Uy = Dy / Len;     % Edge unit vector
                Vx = -Uy;       Vy = Ux;           % Perpendicular unit
        
                % Project hull onto (U,V) axes using origin at current hull vertex
                X0 = double(Xh(I));
                Y0 = double(Yh(I));
                T = (double(Xh) - X0) .* Ux + (double(Yh) - Y0) .* Uy;
                S = (double(Xh) - X0) .* Vx + (double(Yh) - Y0) .* Vy;
        
                Tmin = min(T);  Tmax = max(T);
                Smin = min(S);  Smax = max(S);
        
                Area = (Tmax - Tmin) * (Smax - Smin);
                if Area < BestArea
                    BestArea = Area;
        
                    % Rectangle corners in (U,V) frame, then back to image coords
                    Xc = [X0 + Tmin*Ux + Smin*Vx;
                          X0 + Tmax*Ux + Smin*Vx;
                          X0 + Tmax*Ux + Smax*Vx;
                          X0 + Tmin*Ux + Smax*Vx];
        
                    Yc = [Y0 + Tmin*Uy + Smin*Vy;
                          Y0 + Tmax*Uy + Smin*Vy;
                          Y0 + Tmax*Uy + Smax*Vy;
                          Y0 + Tmin*Uy + Smax*Vy];
        
                    BestCorners = [Yc, Xc];  % [Row Col]
                end
            end
        
            % Round to nearest pixel
            C = round(BestCorners);
        end
    end
end

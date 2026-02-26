function [Xnew, Ynew] = applyFaceTransform(NSide, X, Y, Transform, Args)
    % Apply HEALPix face coordinate transformation.
    %   Applies rotation / reflection / axis swap to local HEALPix
    %   face coordinates (X,Y) when crossing a face boundary.
    %
    %   Coordinates are assumed to be integer pixel indices:
    %       0 <= X,Y <= NSide-1
    %
    %   Transform is a struct with fields:
    %       .Rotate   : 0, 90, 180, or 270  (degrees CCW)
    %       .FlipX    : logical
    %       .FlipY    : logical
    %       .SwapXY   : logical
    %
    %   Transform operations are applied in this order:
    %       1) SwapXY
    %       2) Rotate
    %       3) FlipX / FlipY
    %
    % Input  : - NSide.
    %          - X
    %          - Y
    %          - Ttransform struct with the following optional fields:
    %            'Rotate', 'FlipX', 'FlipY'
    %          * ...,key,val,...
    %            'CheckRange' - Default is true.
    % Output : - X new transformed coordinates.
    %          - Y new transformed coordinates.
    % Notes  : This function is required for correct HEALPix neighbor finding.
    % Author : Eran Ofek (Feb 2026)
    %
    % Example: [Xnew, Ynew] = applyFaceTransform(NSide, X, Y, Transform)
    
    arguments
        NSide (1,1) {mustBeInteger, mustBePositive}
        X {mustBeInteger}
        Y {mustBeInteger}
        Transform struct
        Args.CheckRange (1,1) logical = true
    end
    
    % Convert to double for arithmetic safety
    NS = double(NSide);
    X = double(X);
    Y = double(Y);
    
    if Args.CheckRange
        if any(X(:) < 0 | X(:) >= NS | Y(:) < 0 | Y(:) >= NS)
            error('X and Y must satisfy 0 <= X,Y < NSide.');
        end
    end
    
    % --- 1) Optional axis swap ---
    if isfield(Transform,'SwapXY') && Transform.SwapXY
        Tmp = X;
        X = Y;
        Y = Tmp;
    end
    
    % --- 2) Rotation (CCW) ---
    if isfield(Transform,'Rotate')
        switch Transform.Rotate
            case 0
                % nothing
            case 90
                % (x,y) -> (y, NS-1-x)
                Xtmp = Y;
                Ytmp = NS - 1 - X;
                X = Xtmp;
                Y = Ytmp;
            case 180
                % (x,y) -> (NS-1-x, NS-1-y)
                X = NS - 1 - X;
                Y = NS - 1 - Y;
            case 270
                % (x,y) -> (NS-1-y, x)
                Xtmp = NS - 1 - Y;
                Ytmp = X;
                X = Xtmp;
                Y = Ytmp;
            otherwise
                error('Transform.Rotate must be 0,90,180,270.');
        end
    end
    
    % --- 3) Optional flips ---
    if isfield(Transform,'FlipX') && Transform.FlipX
        X = NS - 1 - X;
    end
    
    if isfield(Transform,'FlipY') && Transform.FlipY
        Y = NS - 1 - Y;
    end
    
    % Convert back to integer type
    Xnew = uint32(X);
    Ynew = uint32(Y);

end
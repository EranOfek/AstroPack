function Result = tranAffine(Obj, AffineMatrix, IsForward, Args)
    % Apply affine transformation to an AstroCatalog object
    % Input  : - An AstroCatalog or AstroImage (with a catalog) object or
    %            a matrix. 
    %            If a matrix, then assumes that it contains two columns of
    %            [x, y] coordinates.
    %          - An affine transformation in one of the following formats:
    %            1. A 3x3 affine transformation matrix.
    %            2. An affine2d class.
    %            3. [ShiftX, ShiftY]
    %            4. [ShiftX, ShiftY, Rotation]
    %            5. [ShiftX, ShiftY, Rotation, Scale]
    %            6. [ShiftX, ShiftY, Rotation, Scale, FlipX, FlipY]
    %          - A logical indicating if to perform a forward
    %            transformation (true) or backward (false).
    %            Default is true (forward transformation).
    %          * ...,key,val,...
    %            'RotUnits' - Units for the rotation angle (if provided).
    %                   Default is 'rad'.
    %            'ColX' - A cell array of column nnames for the X
    %                   coordinates. Will choose the first to appear.
    %                   Default is AstroCatalog.DefNamesX.
    %            'ColY' - A cell array of column nnames for the Y
    %                   coordinates. Will choose the first to appear.
    %                   Default is AstroCatalog.DefNamesY.
    %            'CreateNewObj' - - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    % Output : - An AstroCatalog object in which the coordinates are
    %            transformed.
    % Author : Eran Ofek (Jun 2021)
    % Example: Result = imProc.trans.tranAffine(rand(100,2), [1 1], true)
    

    arguments
        Obj
        AffineMatrix
        IsForward(1,1) logical  = true;
        
        Args.RotUnits           = 'rad';
        Args.ColX               = AstroCatalog.DefNamesX;
        Args.ColY               = AstroCatalog.DefNamesY;
        
        Args.CreateNewObj       = [];
    end
    
    if isnumeric(Obj)
        Tmp = Obj;
        Obj = AstroCatalog;
        Obj.Catalog  = Tmp;
        Obj.ColNames = {'X','Y'};
        %Obj.ColUnits = Args.ColUnits;
    end
    
    if isempty(Args.CreateNewObj)
        if nargout==0
            Args.CreateNewObj = false;
        else
            Args.CreateNewObj = true;
        end
    end
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each element of AstroCatalog/AstroImage
        
        % get X/Y from catalog
        if isa(Obj, 'AstroCatalog')
            Cat = Result(Iobj);
        elseif isa(Obj, 'AstroImage')
            Cat = Result(Iobj).CatData;
        else
            error('Unknown catalog format');
        end
            
        [ColX]   = colnameDict2ind(Cat, Args.ColX);
        [ColY]   = colnameDict2ind(Cat, Args.ColY);
        [X] = getCol(Cat, ColX);
        [Y] = getCol(Cat, ColY);  
        
        % Perform Affine transformation
        if isa(AffineMatrix, 'affine2d')
            % do nothing - already a MATLAB affine2d object
            AffineMatrix = AffineMatrix.T;
        else
            if all(size(AffineMatrix)==[3 3])
                % Assume AffineTrans is a 3x3 matrix of forward affine
                % transformation
                
            else
                % Assume input is vector of parameters
                switch numel(AffineMatrix)
                    case 2
                        % [ShiftX, ShiftY]
                        ShiftX = AffineMatrix(1);
                        ShiftY = AffineMatrix(2);
                        Theta  = 0;
                        Scale  = 1;
                        FlipX  = 1;
                        FlipY  = 1;
                    case 3
                        % [ShiftX, ShiftY, Rotation]
                        ShiftX = AffineMatrix(1);
                        ShiftY = AffineMatrix(2);
                        Theta  = AffineMatrix(3);
                        Scale  = 1;
                        FlipX  = 1;
                        FlipY  = 1;
                    case 4
                        % [ShiftX, ShiftY, Rotation, Scale]
                        ShiftX = AffineMatrix(1);
                        ShiftY = AffineMatrix(2);
                        Theta  = AffineMatrix(3);
                        Scale  = AffineMatrix(4);
                        FlipX  = 1;
                        FlipY  = 1;
                    case 6
                        % [ShiftX, ShiftY, Rotation, Scale, FlipX, FlipY]
                        ShiftX = AffineMatrix(1);
                        ShiftY = AffineMatrix(2);
                        Theta  = AffineMatrix(3);
                        Scale  = AffineMatrix(4);
                        FlipX  = AffineMatrix(5);
                        FlipY  = AffineMatrix(6);
                    otherwise
                        error('Unknown Addine Transformation option');
                end
                Theta        = convert.angular(Args.RotUnits, 'rad', Theta);
                AffineMatrix = [FlipX.*Scale.*cos(Theta), -FlipY.*Scale.*sin(Theta), ShiftX; FlipX.*Scale.*sin(Theta),  FlipY.*Scale.*cos(Theta), ShiftY; 0  0  1];
            end
            
        end
        
        % backward/forward transformation
        if ~IsForward
            AffineMatrix = AffineMatrix.';        
        end
        
        N = size(X,1);
        if N>0
            Z = ones(N,1);

            Coo    = [X,Y,Z];
            NewCoo = [AffineMatrix * Coo.'].';
            
            Cat = replaceCol(Cat, NewCoo(:,1:2), [ColX, ColY]);
           
            if isa(Obj, 'AstroCatalog')
                Result(Iobj) = Cat;
            elseif isa(Obj, 'AstroImage')
                Result(Iobj).CatData = Cat;
            else
                error('Unknown catalog format');
            end
            
        end
    end
    
end


classdef Rect
    % Rectangular
    % CCDSEC: [Xmin Xmax Ymin Ymax]

    properties
        X = 0;
        Y = 0;
        Width = 0;
        Height = 0;
    end
    
    properties (Dependent)
        Top
        Left
        Right
        Bottom
        CCDSEC
    end
       
   
    methods % constructor
        function Obj = Rect(varargin)
            if numel(varargin) == 4
                Obj.setRect(varargin{1}, varargin{2}, varargin{3}, varargin{4});
            end
        end        
    end
    
    
    methods        
        function Obj = setRect(Obj, X, Y, W, H)
            Obj.X = X;
            Obj.Y = Y;
            Obj.Width = W;
            Obj.Height = H;
        end

    end
    
        
    methods % setters/getters        
        function Result = get.Left(Obj)
            Result = Obj.X;
        end
        
        function Result = get.Bottom(Obj)
            Result = Obj.Y + Obj.Height - 1;
        end        
        
        function Result = get.Right(Obj)
            Result = Obj.X + Obj.Width - 1;
        end        
        
        function Result = get.CCDSEC(Obj)
            Result = [Obj.X, Obj.X + Obj.Width - 1, Obj.Y, Obj.Y + Obj.Height - 1];
        end                
        
        function Obj = set.CCDSEC(Obj, Mat)
            Obj.X = Mat(1);
            Obj.Y = Mat(3);
            Obj.Width = Mat(3) - Mat(1);
            Obj.Height = Mat(4) - Mat(2);
        end                        
        
    end
    
    
    methods (Static)  % unitTest
        function Result = unitTest()

            A = Rect();
            B = Rect();
            
            Result = true;
            
        end
    end      
  
end

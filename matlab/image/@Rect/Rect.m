% Rect
%

% #functions (autogen)
% Rect -
% get.Bottom -
% get.CCDSEC -
% get.Left -
% get.Right -
% get.Top -
% set.Bottom -
% set.CCDSEC -
% set.Left -
% set.Right -
% set.Top -
% #/functions (autogen)
%

classdef Rect
    % Rectangular - Note: non-handle class
    % CCDSEC: [Xmin Xmax Ymin Ymax]

    properties
        X = 0;
        Y = 0;
        Width = 0;
        Height = 0;
    end
    
    properties
        % Do NOT use 'dependent' with non-handle class, just use getters/setters
        Top = 0;
        Left = 0;
        Right = 0;
        Bottom = 0;
        CCDSEC = [0, 0, 0, 0];
    end
       
   
    methods % constructor
        function Obj = Rect(varargin)

            if numel(varargin) == 0
                disp('0');
                            
            % X, Y, Width, Height
            % Note: Non-handle object must be set in constructor and not in
            % function
            elseif numel(varargin) == 4
                Obj.X = varargin{1};
                Obj.Y = varargin{2};
                Obj.Width = varargin{3};
                Obj.Height = varargin{4};
                
            % CCDESC
            elseif numel(varargin) == 1
                Sec = varargin{1};
                Obj.CCDSEC = Sec;
            end
            
        end
    end
    
    
    methods
        
    end
    
        
    methods % setters/getters
        
        % Getters
        function Result = get.Left(Obj)
            Result = Obj.X;
        end
        
        function Result = get.Top(Obj)
            Result = Obj.Y;
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
        
        % Setters
        function Obj = set.Left(Obj, Value)
            Obj.X = Value;
        end
        
        function Obj = set.Top(Obj, Value)
            Obj.Y = Value;
        end
        
        function Obj = set.Bottom(Obj, Value)
            Obj.Height = Value - Obj.Y  + 1;
        end
        
        function Obj = set.Right(Obj, Value)
            Obj.Width = Value - Obj.X + 1;
        end
             
        function Obj = set.CCDSEC(Obj, Mat)
            Obj.X = Mat(1);
            Obj.Y = Mat(3);
            Obj.Width = Mat(2) - Mat(1) + 1;
            Obj.Height = Mat(4) - Mat(3) + 1;
        end
        
    end
    
    
    methods (Static)  % unitTest
        Result = unitTest()
    end
  
end

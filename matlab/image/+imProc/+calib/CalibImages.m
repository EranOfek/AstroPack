% CalibImages class - A class for storing calibration image, and performs
%       the basic calibration steps on images.
% Description: Each elementof this class may contain ...

classdef CalibImages < Component
    properties
        Dark AstroImage     % may hold multiple images
        Flat AstroImage     % may hold multiple images
        Fringe AstroImage   % may hold multiple images
        
        FlatFilter         = NaN;
        DarkExpTime        = NaN;
        DarkTemp           = NaN;
        FringeFilter       = NaN;
        FringeExpTime      = NaN;
    end
   
    methods  % constructor
        function Obj = CalibImages(Args)
            %
            
            arguments
                Args.Dark
                Args.Flat
                Args.FlatFilter     = [];
                Args.DarkExpTime    = [];
                Args.DarkTemp       = [];
            end
            
            Obj.Dark = Args.Dark;
            Obj.Flat = Args.Flat;
            
            
            
            if isempty(Args.FlatFilter)
                % attempt to read FlatFilter from Flat header
            end
            
            
        end
    end
    
    methods
        function Result = dedark(Obj, Image, Args)
            %
            
            arguments
                Obj
                Image AstroImage
                Args.CreateNewObj     = [];
            end
            
            % create new copy of Image object
            [Result, CreateNewObj] = createNewObj(Image, Args.CreateNewObj, nargout);
           
            % select
            
            
        end
    end
    
    
    
end
% @Chen
% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

% FITS 
% https://www.mathworks.com/help/matlab/ref/fitsread.html
classdef FitsDb < ImageDb
    % Properties
    properties (SetAccess = public)
        %config          % Configuration 
        %log             % Log file
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = FitsDb(FileName)
            Obj.FileName = FileName;
        end
        
        
        %
        function Result = Open(Obj, FileName)
            Obj.FileName = FileName;
            Result = true;
        end
        
        
        % Read         
        function Data = read(Obj)
            Data = fitsread(Obj.FileName);       
        end
        
        
        function Header = readHeader(Obj, Path)
        end
        
            
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            fprintf("Started\n");
            
            addpath("D:\Ultrasat\AstroPack.git\matlab\external");

            FileName = "D:\\Ultrasat\\AstroPack.git\\data\\test_images\\local\\image1.fits";
            db = FitsDb(FileName);
            data = db.read(FileName);
            
            %
            disp(size(data));
 
            Result = true;
        end
    end    
        
    
end


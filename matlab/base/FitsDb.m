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
        function Obj = FitsDb()
        end
        
        % 
        
        function data = read(Obj, FileName)
            Obj.FileName = FileName;
            data = fitsread(Obj.FileName);
        end
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            fprintf("Started\n");
            
            addpath("D:\Ultrasat\AstroPack.git\matlab\external");

            FileName = "D:\\Ultrasat\\AstroPack.git\\data\\test_images\\local\\image1.fits";
            db = FitsDb;
            data = db.read(FileName);
            
            %
            disp(size(data));
 
            Result = true;
        end
    end    
        
    
end


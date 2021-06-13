% BaseImage handle class 
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

% "Component" is in folder ../base/

classdef BaseImage < ImageComponent
    
    properties (Hidden, SetAccess = public)
        % Dict BitDictionary                      % The dictionary of a bit mask image
        
        IsBackSub(1,1) logical = false;         % Is the image back subtracted. Set is done by external functions
        %IsVar(1,1) logical = false;            % not really useful here
        
        %DataProp cell = {'Data'};              % a cell of properties on which the fun_* methods will be applied
        Virt VirtImage                          % Actual image data
        %DB
    end
    
    
    methods % Constructor       
        function Obj = BaseImage
            % Base class constructor
            % Package: @Base
            
            
        end
    end 
    
%     methods % DB and read/write
%         function Data = getData(Obj)
%             Data = Obj.Virt.getData();
%         end
%         
%         function setData(Obj, NewData)
%             Obj.Virt.setData(NewData);
%         end
%         
%         % Read from file
%         function read(Obj, FileName)
%             Db = ImageDbManager.getDbByFileExt(FileName);
%             if notempty(Db)
%                 %Db.
%             end
%         end
%         
%         function readDb(Obj, Db, Path)
%         end
%         
%         
%         % Write to file
%         function write(Obj, FileName)
%         end
%         
%         function writeDb(Obj, Db, Path)
%         end
%         
%     end
        
    methods % Setters/getters
        function set.Data(Obj, NewData)
            Obj.setData(NewData);
        end
        

        function Result = get.Data(Obj)
            % getter for image data including scaling
            if isempty(Obj.Scale)
                % return image as is - no scaling is required
                Result = Obj.Data;
            else
                % scaling is required
                Result = imresize(Obj.Data,Obj.Scale,'Method',Obj.ScaleMethod);
            end                
        end
    end
  
    
    methods (Static) % static methods
       
    end
   
    
    methods (Static) % Unit-Test
        function Result = unitTest()
            
            Result = true;
        end
    end
    
end




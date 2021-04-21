% 
%--------------------------------------------------------------------------

classdef SqlDbManager < handle % Component
    
    % Properties
    properties (SetAccess = public)
        
        % Map object is a data structure that allows you to retrieve values 
        % using a corresponding key. Keys can be real numbers or character 
        % vectors. As a result, they provide more flexibility for data 
        % access than array indices, which must be positive integers. 
        % Values can be scalar or nonscalar arrays.
        Map = []            % containers.Map - List of VirtImage objects
    end
    
    %-------------------------------------------------------- 
    methods % Constructor 
        
        function Obj = SqlDbManager()
            Obj.msgLog(LogLevel.Debug, 'SqlDbManager created');
            Obj.Map = containers.Map();
            
        end
        
        
        function delete(Obj)
            % Destructor
            Obj.msgLog(LogLevel.Debug, 'SqlDbManager deleted');
            release();
        end
    end
    
    
    methods    
        function registerDb(Obj, Db)
            Obj.msgLog(LogLevel.Debug, 'registerDb: %s', Image.Uuid);
            
            Key = Db.Key;
            if ~Obj.Map.isKey(Key)
                Obj.Map(Key) = Image;
            else
                Obj.msgLog(ObjLevel.Warning, 'registerDb: Database already exists in map: %s', Key);
            end
        end
        
        
        function unregisterDb(Obj, Db)
            Obj.msgLog(LogLevel.Debug, 'registerDb: %s', Image.Uuid);
            
            Key = Db.Key;
            if Obj.Map.isKey(Key)
                Obj.Map.remove(Key)
            else
                Obj.msgLog(ObjLevel.Warning, 'unregisterDb: Database does not exist in map: %s', Key);
            end            
            
        end
        
 
    end

    
    methods(Static)
        
        function Result = getSingle()
            persistent Manager
            if isempty(Manager)
                Manager = SqlDbManager;
            end
            Result = Manager;
        end
    end
     
    
    methods(Static) % Unit test
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'SqlDbManager test started');

            Manager = SqlDbManager.getSingle();
            
            % See tests in VirtImage.m
            
            io.msgLog(LogLevel.Test, 'SqlDbManager test passed');
            Result = true;
        end
    end    
        
end



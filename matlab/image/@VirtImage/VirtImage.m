% Virtual Image Class
%

% #functions (autogen)
% VirtImage - @Todo: should we take care of VirtImage array?
% delete -
% getData -
% setData -
% #/functions (autogen)
%

% See Datastore (v2021a and up)
% https://www.mathworks.com/help/matlab/datastore.html
%--------------------------------------------------------------------------

classdef VirtImage < Component
    
    % Properties
    properties (SetAccess = public)
        Data                        %
        FileName = ''               %
        Manager VirtImageManager    %
    end
    
    %--------------------------------------------------------
    methods
        % Constructor
        function Obj = VirtImage()
			% @Todo: should we take care of VirtImage array?
            Obj.needUuid();
            Obj.Manager = VirtImageManager.getSingleton();
            Obj.Manager.add(Obj);
        end
        
        % Destructor
        function delete(Obj)
            Obj.Manager.remove(Obj);
        end
                
        % Get data as matrix
        function Result = getData(Obj)
            Result = Obj.Data;
        end
        
        % Set data from matrix
        function setData(Obj, NewData)
            Obj.Data = NewData;
        end
        
    end
    
    
    % Unit test
    methods(Static)
        Result = unitTest()
    end
        
end

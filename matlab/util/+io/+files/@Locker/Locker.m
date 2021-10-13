% Mutex locker
% See https://www.python.org/dev/peps/pep-0343/
% See https://stackoverflow.com/questions/3774328/implementing-use-of-with-object-as-f-in-custom-class-in-python

% #functions (autogen)
% Locker -
% delete -
% lock -
% unlock -
% #/functions (autogen)
%

classdef Locker < handle
    
    properties (Hidden, SetAccess = public)
  
    end
    

    methods % Constructor
        
        function Obj = Locker
            
        end
        
       
        function delete(Obj)
            Obj.unlock()
        end
        
        
        %
        function Result = lock(Obj)
            Result = true;
        end
        
        
        function Result = unlock(Obj)
            Result = true;
        end
        
    end
    
    
        % Unit test
    methods(Static)
        Result = unitTest()

    end
    
end


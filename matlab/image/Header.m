% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------


classdef Header < Component
    % Properties
    properties (SetAccess = public)
        
        filename        
        configPath = "";
        data
        lines
        userData
        
        inputImagePath
        inputImageExt
    end
    

    methods
        % Constructor    
        function obj = Header()
        end
    end
    
    
    % Unit test
    methods(Static)
        function result = uTest()
            fprintf("Started\n");
            conf = Config("c:/temp/conf.txt")
            val = conf.getValue("key1");
            disp(val);           
            num = conf.getNum("key3");
            disp(num);
            result = true;
        end
    end    
        
end



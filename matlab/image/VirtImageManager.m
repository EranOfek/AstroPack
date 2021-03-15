% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------


classdef VirtImageManager < PipelineComponent
    % Properties
    properties (SetAccess = public)
        
        filename        
        configPath = "";
        data
        lines
        userData
        imageList
        virtPath = "";
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function obj = VirtImageManager()
            obj.imageList = containers.Map();
        end
        
        % Read file to lines
        function result = pollInputImage(obj)
            
            obj.filename = fullfile(obj.configPath, fname);
            obj.data = fileread(obj.filename);
            obj.lines = strsplit(obj.data);
            result = true;
        end
        
        
        % Get string
        function value = getStr(obj, key)
            value = getValue(obj, key, "");
        end
        
        
        % Get number
        function value = getNum(obj, key)
            str = getStr(obj, key);
            num = str2double(str);
            if ~isempty(num)
                value = num;
            else
                value = 0;
            end
        end        
             
        
        % Get value from config
        function value = getValue(obj, key, default)
            key = lower(key);
            for i=1:length(obj.lines)
                items = split(obj.lines{i}, "=");
                if length(items) > 1
                    if lower(items{1}) == key
                        value = items{2};
                        return
                    end
                end
            end
            value = default;
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



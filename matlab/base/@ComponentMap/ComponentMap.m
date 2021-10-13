% ComponentMap - Wrapper for MATLAB's containers.Map dictionary object
% Note that this class is derived from Base and not from Component

classdef ComponentMap < handle

    % Properties
    properties (SetAccess = public)

        % Map object is a data structure that allows you to retrieve values
        % using a corresponding key. Keys can be real numbers or character
        % vectors. As a result, they provide more flexibility for data
        % access than array indices, which must be positive integers.
        % Values can be scalar or nonscalar arrays.
        Map = []        % containers.Map(Key) -> Component
        Name = []       % Map name
    end

    %--------------------------------------------------------
    methods % Constructor

        function Obj = ComponentMap(varargin)
            % Constructor, optional parameter is used as map name,
            % otherwise the default '(unnamed)' is used
            if numel(varargin) > 0
                Obj.Name = varargin{1};
            else
                Obj.Name = '(unnamed)';
            end
            Obj.Map = containers.Map();
            Obj.msgLog(LogLevel.Debug, 'ComponentMap created: %s', Obj.Name);
        end


        function delete(Obj)
            % Destructor
            Obj.msgLog(LogLevel.Debug, 'ComponentMap deleted: %s', Obj.Name);
            Obj.release();
        end
    end


    methods % Map functions
        function add(Obj, Comp)
            % Add Component to the map using its MapKey property

            Key = Obj.getKey(Comp);
            Obj.msgLog(LogLevel.Info, 'ComponentMap.add: %s', Key);
            if ~Obj.Map.isKey(Key)
                Obj.Map(Key) = Comp;
            else
                Obj.msgLog(ObjLevel.Warning, 'ComponentMap.add: Component already exists in map: %s', Key);
            end
        end


        function remove(Obj, Comp)
            % Remove the specified component from map

            Key = Obj.getKey(Comp);
            Obj.msgLog(LogLevel.Debug, 'ComponentMap.remove: %s', Key);
            if Obj.Map.isKey(Key)
                Obj.Map.remove(Key);
            else
                Obj.msgLog(ObjLevel.Warning, 'ComponentMap.remove: Component does not exist in map: %s', Key);
            end
        end


        function Result = find(Obj, CompKey)
            % Find component in map by the specified key, returns [] if not
            % found
            if Obj.Map.isKey(CompKey)
                Result = Obj.Map(CompKey);
            else
                Result = [];
            end
        end


        function Result = getKey(Obj, Comp)
            % Get component map key, generate it if required
            Result = Comp.needMapKey();
        end


        function Result = getCount(Obj)
            % Return number of items in map
            Result = Obj.Map.Count;
        end


        function release(Obj)
            % Release all components from map

            for Key = Obj.Map.keys
                % Comp = Obj.Map(Key);

                % @TODO: Need release or delete??
                %Comp.release();

                % Remove from map (this DOES NOT delete the component itself)
                Obj.Map.remove(Key);
            end

            % Make sure that everything was removed
            assert(Obj.Map.Count == 0);
        end


        function msgLog(Obj, Level, varargin)
            % Write message to log

            % Since ComponentMap is derived from Base and not from Component
            % we use the global io.msgLog() function
            io.msgLog(Level, varargin{:});
        end

    end


    methods(Static) % Unit test

        function Result = getSingleton()
            % Return singleton object
            persistent PersObj
            if isempty(PersObj)
                PersObj = ComponentMap;
            end
            Result = PersObj;
        end
    end


    methods(Static) % Unit test

        Result = unitTest()
    end

end

% Component class - this class hinerits from Base class, and most
% astro-related class are hinerits from this class.
%
% The component class starts loads the Configuration object into it.
% Functionality:
%       makeUuid - (re)generate a UUID to each element in an object
%       needUuid - generate a UUID to each element, only if empty
%       needMapKey -
%       msgLog   - write msg to log
%       msgStyle - set msg style.
%       selectDefaultArgsFromProp(Obj, Args) -
%               Given an Args structure, go over fields - if empty, take
%               value from object property. Otherwise, use value.
%       convert2class(Obj, DataPropIn, DataPropOut, ClassOut, Args) -
%               Convert a class that henhirts from Component to another class
%               Uses eval, so in some cases maybe slow. Creates a new copy.
%       data2array(Obj, DataProp) -
%               Convert scalar data property in an object into an array
%--------------------------------------------------------------------------

% To check:
% Can we inherit from HandleBase or we need to inhertic from handle?

classdef HandleComponent < HandleBase & Component
    % Parent class for all components

    % Properties
    properties (SetAccess = public)
        Name   = []                % Name string
        Owner  = []                % Indicates the component that is responsible for streaming and freeing this component
        Uuid   = []                % Global unique ID, generated with java.util.UUID.randomUUID()
        Tag    = []                % Optional tag (i.e. for events handling)
        MapKey = []                % Used with ComponentMap class
        Config Configuration       % Configuration, deafult is system configuration
        Log MsgLogger              % Logger, default is system logger
        DebugMode = true           % DebugMode
    end

    %--------------------------------------------------------
    methods % Constructor

        function Obj = HandleComponent(varargin)
            % Constructor
            % By default use system log and configuration
            %   NewComp = Component()
            %   NewComp = Component(Owner)

            % Set owner component
            if numel(varargin) > 0
                Obj.Owner = varargin{1};
            end

            % Use default log and configuration
            Obj.Log = MsgLogger.getSingleton();
            Obj.Config = Configuration.getSingleton();
        end
    end


    methods

        function setName(Obj, Name)
            % Set component name
            Obj.Name = Name;
        end


        function Result = makeUuid(Obj)
            % Generate or re-generate unique ID for each element in object,
            % Return Uuid or [] for array
            % MyUuid = Obj.makeUuid()

            for i = 1:numel(Obj)
                Obj(i).Uuid = Component.newUuid();
            end

            if numel(Obj) == 1
                Result = Obj.Uuid;
            else
                Result = [];
            end
        end


        function Result = needUuid(Obj)
            % Generate unique ID only if empty
            % Return Uuid or [] for array
            for i = 1:numel(Obj)
                if isempty(Obj(i).Uuid)
                    Obj(i).makeUuid();
                end
            end

            if numel(Obj) == 1
                Result = Obj.Uuid;
            else
                Result = [];
            end
        end


        function Result = needMapKey(Obj)
            % Generate or get current map key as uuid
            % Map key is used with ComponentMap class as key to the object
            for i = 1:numel(Obj)
                if isempty(Obj(i).MapKey)
                    Obj(i).MapKey = Obj(i).needUuid();
                end
            end

            if numel(Obj) == 1
                Result = Obj.MapKey;
            else
                Result = [];
            end
        end


        function msgLog(Obj, Level, varargin)
            % Write message to log according to current log-level settings
            % Example: Obj.msgLog(LogLevel.Debug, 'Value: %d', i)

            % Do nothing if both display and file logs are disabled
            if ~Obj(1).Log.shouldLog(Level, Obj(1).Log.CurDispLevel) && ...
                ~Obj(1).Log.shouldLog(Level, Obj(1).Log.CurFileLevel)
                return
            end

            % Log all items in array
            for i = 1:numel(Obj)

                % Add array index to log message
                Index = '';
                if numel(Obj) > 1
                    Index = ['(', char(string(i)), ')'];
                end

                % Add Name to log
                vararg = varargin;
                if ~isempty(Obj(i).Name) || numel(Obj) > 1
                    vararg{1} = [Obj.Name, Index, ': ' , varargin{1}];
                end

                Obj(i).Log.msgLog(Level, vararg{:});
            end
        end


        function msgStyle(Obj, Level, Style, varargin)
            % Log with style (color, etc.)
            % Example: Obj.msgLog(LogLevel.Debug, 'Value: %d', i)

            % Do nothing if both display and file logs are disabled
            if ~Obj(1).Log.shouldLog(Level, Obj(1).Log.CurDispLevel) && ...
                ~Obj(1).Log.shouldLog(Level, Obj(1).Log.CurFileLevel)
                return
            end

            % Log all items in array
            for i = 1:numel(Obj)

                % Add array index to log message
                Index = '';
                if numel(Obj) > 1
                    Index = ['(', char(string(i)), ')'];
                end

                % Add Name to log
                vararg = varargin;
                if ~isempty(Obj(i).Name) || numel(Obj) > 1
                    vararg{1} = [Obj.Name, Index, ': ' , varargin{1}];
                end

                Obj(i).Log.msgStyle(Level, Style, vararg{:});
            end
        end
    end


    methods % Auxiliary functions

        function Args = selectDefaultArgsFromProp(Obj, Args)
            % Given an Args structure, go over fields - if empty, take
            % value from object property. Otherwise, use value.

            ArgNames = fieldnames(Args);
            for Ian = 1:1:numel(ArgNames)
                if isempty(Args.(ArgNames{Ian}))
                    Args.(ArgNames{Ian}) = Obj.(ArgNames{Ian});
                end
            end

        end
    end


    methods % some useful functionality
        function Result = convert2class(Obj, DataPropIn, DataPropOut, ClassOut, Args)
            % Convert a class that henhirts from Component to another class
            %       Uses eval, so in some cases maybe slow.
            %       Creates a new copy.
            % Input  : - An object which class hinherits from Component.
            %          - A cell array of data properties in the input class.
            %          - A cell array of data properties, corresponding to
            %            the previous argument, in the target class.
            %          - A function handle for the target class.
            %          * ...,key,val,...
            %            'UseEval' - A logical indicating if to use the
            %                   eval function. This is needed when the data
            %                   properties have levels. Default is false.
            % Output : - The converted class
            % Author : Eran Ofek (Apr 2021)
            % Example: IC=ImageComponent; IC.Image = 1;
            %          AI=convert2class(IC,{'Image'},{'Image'},@AstroImage)
            %          AI=convert2class(IC,{'Data'},{'ImageData.Data'},@AstroImage,'UseEval',true)

            arguments
                Obj
                DataPropIn
                DataPropOut
                ClassOut function_handle
                Args.UseEval(1,1) logical     = false;
            end

            Nprop = numel(DataPropIn);
            if Nprop~=numel(DataPropOut)
                error('Number of Data properties in and out should be the same');
            end

            Nobj   = numel(Obj);
            Result = ClassOut(size(Obj));
            for Iobj=1:1:Nobj
                for Iprop=1:1:Nprop
                    if Args.UseEval
                        Str = sprintf('Result(Iobj).%s = Obj(Iobj).%s;',DataPropOut{Iprop},DataPropIn{Iprop});
                        eval(Str);
                    else
                        Result(Iobj).(DataPropOut{Iprop}) = Obj(Iobj).(DataPropIn{Iprop});
                    end
                end
            end
        end


        function varargout = data2array(Obj, DataProp)
            % Convert scalar data property in an object into an array
            % Input  : - An object that hinherits from Component.
            %            That have data properties that contains scalar or
            %            empty.
            %          - A cell array of data properties.
            %            The scalar content of each such data property will
            %            be inserted into an array of numbers which size is
            %            equal to the size of the input object.
            % Output : * An array per each data property.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC= ImageComponent({1, 2});
            %          [A] = data2array(IC,'Image')
            %          [A,B] = data2array(IC,{'Image','Data'})

            arguments
                Obj
                DataProp
            end

            if ischar(DataProp)
                DataProp = {DataProp};
            end

            Nobj  = numel(Obj);
            Nprop = numel(DataProp);
            if nargout > Nprop
                error('Numbre of input data properties must be equal or larger than the number of output arguments');
            end
            DataProp = DataProp(1:nargout);

            for Iprop=1:1:Nprop
                varargout{Iprop} = nan(size(Obj));
                for Iobj=1:1:Nobj
                    Nd = numel(Obj(Iobj).(DataProp{Iprop}));
                    if Nd==1
                        varargout{Iprop}(Iobj) = Obj(Iobj).(DataProp{Iprop});
                    elseif Nd==0
                        % do nothing - filled with NaNs
                    else
                        error('data2array works only on data properties that contains scalars or empty');
                    end
                end
            end
        end
    end


    methods(Static)
        function Result = newUuid()
            % Generate Uuid using java package
            Temp = java.util.UUID.randomUUID;

            % Convert java string to char
            Result = string(Temp.toString()).char;
        end


        function Result = newSerial()
            % Generate simple serial number, used as alternative to Uuid
            persistent Counter
            if isempty(Counter)
                Counter = 0;
            end
            Counter = Counter + 1;
            Result = Counter;
        end


        function Result = newSerialStr(varargin)
            % Generate simple serial number, used as alternative to Uuid,
            % shorter string and fast performance.
            % If parameter is specified, use it as prefix to the counter
            % Example:
            %   Serial = Obj.newSerialStr('MyIndex') -> 'MyIndex1'
            if numel(varargin) == 1
                Result = string(varargin(1) + string(Component.newSerial())).char;
            else
                Result = string(Component.newSerial()).char;
            end
        end
    end


    methods(Static) % Unit test
        Result = unitTest()
            % unitTest for Component class
    end
end

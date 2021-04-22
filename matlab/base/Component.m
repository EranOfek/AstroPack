% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef Component < Base
    % Parent class for all components
    
    % Properties
    properties (SetAccess = public)
        Name                    % Name string
        Owner                   % Indicates the component that is responsible for streaming and freeing this component
        Uuid                    % Global unique ID
        Tag                     % Optional tag (i.e. for events handling)
        RegKey                  % Used with CompRegManager class
        Config Configuration    % Configuration, deafult is system configuration
        Log MsgLogger           % Logger, default is system logger
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = Component()
            % By default use system log and configuration
            Obj.Log = MsgLogger.getSingle();
            Obj.Config = Configuration.getSingle();
        end
    end
    
    methods
        
        function Result = makeUuid(Obj)
            % Generate unique ID
            Temp = java.util.UUID.randomUUID;
            Obj.Uuid = string(Temp.toString()).char;
            Result = Obj.Uuid;
        end
        
        
        function Result = needUuid(Obj)
            % Generate unique ID
            if isempty(Obj.Uuid)
                Obj.makeUuid();
            end
            Result = Obj.Uuid;
        end
        
        
        function Result = needRegKey(Obj)
            % Generate unique ID
            if isempty(Obj.RegKey)
                Obj.RegKey = Obj.needUuid();
            end
            Result = Obj.RegKey;
        end        

        
        function msgLog(Obj, Level, varargin)  
            % Write message to log
            Obj.Log.msgLog(Level, varargin{:});
        end
    end
    
    methods % some useful functionality
        function Result = convert2class(Obj, DataPropIn, DataPropOut, ClassOut, Args)
            % Convert a class that henhirts from Component to another class
            %       Uses eval, so in some cases maybe slow.
            %       Creates a new copy.
            % Input  : - An object which class hinherits from Component.
            %          - A cell array of data properties in the input
            %            class.
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
            if nargout>Nprop
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
    
    
      
    methods(Static) % Unit test
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'Component test started');
            
            a = Component;
            a.msgLog(LogLevel.Test, 'a created');
            
            b = Component;
            b.msgLog(LogLevel.Test, 'b created');            
            
            c = Component;
            c.msgLog(LogLevel.Test, 'c created');
            
            io.msgLog(LogLevel.Test, 'Testing Uuid');            
            a.needUuid();            
            b.needUuid();
            assert(~all(a.Uuid ~= b.Uuid));
           
            io.msgLog(LogLevel.Test, 'Component test passed');   
                       
            
            Result = true;
        end
    end    
end


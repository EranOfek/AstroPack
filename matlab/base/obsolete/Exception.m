% https://www.mathworks.com/matlabcentral/answers/370457-error-management-in-oop-framework

classdef ObjException < MException
    methods
        function obj=ObjException(src,varargin)
            obj=obj@MException(varargin{:});
            obj.sourceObj=src;
        end
        function rep=getReport(obj,varargin)
            rep=sprintf('Object Error on object %s\n%s', obj.sourceObj.name, getReport@MException(obj,varargin{:}));
        end
    end
    properties
        sourceObj
    end
end


classdef testobj
    methods
        function meth(obj,arg)
            assert(obj,arg);
        end
        function assert(obj,cond,varargin)
            if ~cond
                if nargin>2
                    ex=ObjException(obj,varargin{:});
                else
                    ex=ObjException(obj,'testobj:AssertFailed','assertion failed');
                end
                throwAsCaller(ex);
            end
        end
        function error(obj,varargin)
            ex=ObjException(obj,varargin{:});
            throwAsCaller(ex);
        end
    end
    properties
        name
    end
end

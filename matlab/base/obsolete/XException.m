% https://www.mathworks.com/matlabcentral/answers/370457-error-management-in-oop-framework

classdef XException < MException

    properties
        SourceObj
    end

    methods
        function Obj = XException(Src, varargin)
            Obj = Obj@MException(varargin{:});
            Obj.SourceObj = Src;
        end

        function rep = getReport(Obj, varargin)
            rep = sprintf('Object Error on object %s\n%s', Obj.SourceObj.name, getReport@MException(Obj, varargin{:}));
        end
    end


    methods(Static)
        function Result = unitTest()
        end
    end

end


classdef testobj
    properties
        name
    end

    methods
        function meth(Obj, arg)
            assert(Obj, arg);
        end

        function assert(Obj, Cond, varargin)
            if ~Cond
                if nargin > 2
                    ex = XException(Obj, varargin{:});
                else
                    ex = XException(Obj, 'testobj:AssertFailed', 'assertion failed');
                end
                throwAsCaller(ex);
            end
        end


        function error(Obj, varargin)
            ex = ObjException(Obj, varargin{:});
            throwAsCaller(ex);
        end
    end

end


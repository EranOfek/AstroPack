%==========================================================================
% ULTRASAT
%
% File:   debug_ModelBase.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 11/02/2025
%==========================================================================
%
% Debug function for ultrasat.api.BaseModel class
% Run by: ultrasat.api.debug_ModelBase()
%


classdef debug_ModelBase_MyClass < handle

    properties
        id = 123
        name = 'TestObject'
        values = [1, 2, 3]
        st1
        st2
    end


   methods(Access = public)

       function obj = debug_ModelBase_MyClass()
           obj.st1 = struct('A', 'aaa', 'B', 'bbb');
           obj.st2 = struct('C', 'ccc', 'D', struct('E', 'eee', 'dt', now, 'dt_str', datestr(now)));
       end

   end
end


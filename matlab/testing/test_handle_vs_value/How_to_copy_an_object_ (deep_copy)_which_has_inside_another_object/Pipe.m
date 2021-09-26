
classdef Pipe < matlab.mixin.Copyable
   properties
      PipeName;
      PipeVault;
      UnLock;
      Units;
   end
   methods (Access = protected)
      function thiscopy = copyElement(this)
         thiscopy = copyElement@matlab.mixin.Copyable(this); %shallow copy of all elements
         thiscopy.PipeVault = copy(this.PipeVault); %Deep copy of pipevault
      end
   end
end



classdef Pipe < matlab.mixin.Copyable
    
   properties
      PipeName;
      PipeVault;
      UnLock;
      Units;
   end
   
   methods (Access = protected)
       
      function thiscopy = copyElement(this)
          
         % Shallow copy of all elements
         thiscopy = copyElement@matlab.mixin.Copyable(this); 
         
         % Deep copy of pipevault
         thiscopy.PipeVault = copy(this.PipeVault); 
      end
   end
   
end


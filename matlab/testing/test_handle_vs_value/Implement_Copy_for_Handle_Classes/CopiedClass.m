classdef CopiedClass < matlab.mixin.Copyable

   properties (NonCopyable)
      Prop1
      Prop2 = datestr(now) % Assign current time
   end
   
end

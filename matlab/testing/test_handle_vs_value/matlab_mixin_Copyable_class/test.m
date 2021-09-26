% https://www.mathworks.com/help/matlab/ref/matlab.mixin.copyable-class.html

% 
sc = ShallowCp(7);
dc = DeepCp(7);
a = ContainsHandles(4, 5, dc, sc);
a.DeepObj

% 
b = a;
b.Prop1
b.Prop2
b.DeepObj
b.ShallowObj
      

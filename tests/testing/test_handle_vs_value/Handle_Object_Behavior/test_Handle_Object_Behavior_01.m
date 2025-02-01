
% Handle Objects Modified in Functions

% When you pass an argument to a function, the function copies the variable 
% from the workspace in which you call the function into the parameter variable 
% in the function’s workspace.

% Passing a nonhandle variable to a function DOES NOT affect the original variable that 
% is in the caller’s workspace. For example, myFunc modifies a local variable called var, 
% but when the function ends, the local variable var no longer exists:


function myFunc(var)
   var = var + 1;
end


% The myFunc function can return the modified value, which you could assign to the 
% same variable name (x) or another variable.
function out = myFunc2(var)
   out = var + 1;
end


% When the argument is a handle variable, the function copies only the handle, 
% not the object identified by that handle. Both handles (original and local copy) 
% refer to the same object.

% When the function modifies the data referred to by the object handle, 
% those changes are accessible from the handle variable in the calling workspace 
% without the need to return the modified object.

% For example, the modifySampleRate function changes the audioplayer sample rate:


function modifySampleRate(audioObj,sr)
   audioObj.SampleRate = sr;
end

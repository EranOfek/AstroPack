% https://www.mathworks.com/matlabcentral/answers/268195-ho-to-copy-an-object-deep-copy-which-has-inside-another-object

% As per its documentation matlab.mixin.Copyable does not make a deep copy of the object 
% properties even if they themselves derived from copyable: 
% "In making a shallow copy, MATLAB® does not call copy recursively on any handles 
% contained in property values." 

% You actually have to override the copyElement to make the copy yourself

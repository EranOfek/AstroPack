function trace_spec(Image,Args)
% UNDER CONSTRUCTION

arguments
    Image           {mustBeNumeric(Image)}
    Args.DispAxis                           = 2;  % 0-auto; 1,2
end

if Args.DispAxis==1
    % transpose the image so DispAxis is 2
    Image = Image.';
elseif Args.DispAxis==0
    % attempt to find dispersion axis
    
elseif Args.DispAxis==2
    % do nothing
else
    error('Unknown DispAxis option');
end

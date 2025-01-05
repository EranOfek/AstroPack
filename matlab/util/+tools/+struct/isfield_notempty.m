function Flag=isfield_notempty(Struct,Field)
% Check if field exist and not empty
% Package: Util.struct
% Description: Check if a field exist in a structure and if it is not
%              empty.
% Input  : - Structure array, or an object with properties.
%          - String containing field name.
% Output : - A flag indicating if the field exist and not empty (true),
%            or otherwise (false).
% Tested : Matlab R2014a
%     By : Eran O. Ofek                    Jan 2015
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Flag=Util.struct.isfield_notempty(Sim,'Mask');
% Reliable: 2
%--------------------------------------------------------------------------

Flag = false(size(Struct));
if isstruct(Struct)
    if all(isfield(Struct,Field))
        Nst = numel(Struct);
        for Ist=1:1:Nst
            if (~isempty(Struct(Ist).(Field)))
                Flag(Ist) = true;
            end
        end
    end
else
    
    if all(isprop(Struct,Field))
        Nst = numel(Struct);
        for Ist=1:1:Nst
            if (~isempty(Struct(Ist).(Field)))
                Flag(Ist) = true;
            end
        end
    end
end
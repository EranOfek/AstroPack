function Result = struct2text(Struct)
    % Structure field name and content to text, with recursion
    % Description: Given a structure, prepare a char of all the
    %              fieldname: fieldvalue, ...
    % Input  : - A structure.
    % Output : - char of fieldname:fieldvalue,...
    % Example: text = = tools.struct.struct2text(s);
    %--------------------------------------------------------------------------

    % Recursive dump struct and replace data in struct:
    fields = fieldnames(Struct);
    Result = '';
    for i=1:numel(fields)
        FieldName = fields{i};
        Value = Struct.(FieldName);
        if ~isempty(Result)
            Result = strcat(Result, ', ');
        end
        Result = strcat(Result, FieldName, ': ');
        if isstruct(Value)
            Result = strcat(Result, '{ ', tools.struct.struct2text(Value), '}');
        else
            if isnumeric(Value) || islogical(Value)
                Result = strcat(Result, sprintf('%d', Value));
            elseif ischar(Value)
                Result = strcat(Result, sprintf('%s', Value));
            else
                Result = strcat(Result, '?');
            end
        end
    end
end

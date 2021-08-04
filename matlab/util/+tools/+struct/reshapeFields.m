function St = reshapeFields(St, NewSize)
    % Reshape all fields which have a consistent size
    % Input  : - A structure array
    %          * A vector of new size. (vector, or multiple arguments).
    % Output : - If a field contains a vector which number of elements is
    %            consistent with the new size, then the fields will be
    %            reshaped with this new size.
    % Author : Eran Ofek (Aug 2021)
    % Example: St.A = ones(20,1); St(1).B = 1; St(2).A=ones(20,1); St(2).B=ones(20,1);
    %          St = tools.struct.reshapeFields(St, [5, 4]);
   
    FieldNames = fieldnames(St);
    Nfn        = numel(FieldNames);
    N = numel(St);
    for I=1:1:N
        for Ifn=1:1:Nfn
            if numel(St(I).(FieldNames{Ifn}))==prod(NewSize)
                % field is reshapeable
                % reshape
                St(I).(FieldNames{Ifn}) = reshape(St(I).(FieldNames{Ifn}), NewSize);
            end
        end
    end
        
end


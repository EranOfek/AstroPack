function St2 = copyProp(St1, St2, PropList, Check)
    % Copy properties between struct, with optional checking they exist.
    %       Copy selected fields in in the first input, to the second
    %       input, possibly doing the copy only if the field exist in both
    %       structures.
    % Input  : - First structure.
    %          - Second structure.
    %          - A cell array of field names to copy.
    %            If empty, use all fields in St1.
    %          - A logical indicating if to perform the copy only if the
    %            specific field exist in both structures.
    %            Default is true.
    % Output : - The second structure with the copied fields from the first
    %            struct.
    % Author : Eran Ofek (Sep 2021)
    % Example: St1.A = 1; St1.B = 2; St2.A=[]; St2.B = [];
    %          St2 = tools.struct.copyProp(St1, St2, {'B'}, true)

    arguments
        St1
        St2
        PropList        = [];
        Check logical   = true;
    end

    Prop1 = fieldnames(St1);
    Prop2 = fieldnames(St2);

    if isempty(PropList)
        PropList = Prop1;
    end

    if ischar(PropList)
        PropList = {PropList};
    end
    

    Nf = numel(PropList);
    for Ifield=1:1:Nf
        if Check
            if any(strcmp(PropList{Ifield}, Prop1)) && any(strcmp(PropList{Ifield}, Prop2))
                % field found
                St2.(PropList{Ifield}) = St1.(PropList{Ifield});
            end
        else
            % copy without check
            St2.(PropList{Ifield}) = St1.(PropList{Ifield});
        end
    end
end
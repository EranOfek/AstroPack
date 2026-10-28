function MatVersion = chooseMatVersion(S, MatVersion, MaxBytes)
    % MAT-file version for saving a struct: '-v7.3' if a variable is too large for '-v7'
    %   '-v7' and '-v6' cannot hold a variable larger than 2 GB (issue #1447).
    %   whos counts a handle object as 8 bytes, so the size of a variable is
    %   estimated from the data held in its handle objects (their stored
    %   public properties, without the handle objects in them, e.g. Config
    %   or Dictionary). Used by io.files.saveMat and io.files.saveAsync.
    % Input  : - Scalar struct. Each field is a variable to save.
    %          - Requested MAT-file version flag for save(), e.g. '-v7',
    %            {'-v7','-nocompression'}. Default is '-v7'.
    %          - Size [bytes] of a variable above which '-v7.3' is used.
    %            Default is 1.8e9 (2 GB limit minus a margin).
    % Output : - Cell array of the save() flags: the requested ones, or
    %            {'-v7.3'}.
    % Author : A.M. Krassilchtchikov (Oct 2026)
    % Example: S.A = rand(10); V = io.files.chooseMatVersion(S, '-v7')

    arguments
        S(1,1) struct
        MatVersion = '-v7';
        MaxBytes   = 1.8e9;
    end

    MatVersion = cellstr(MatVersion);
    if ~any(strcmp(MatVersion, '-v7.3')) && isfinite(MaxBytes)
        Bytes = cellfun(@varBytes, struct2cell(S));
        if any(Bytes > MaxBytes)
            MatVersion = {'-v7.3'};
        end
    end
end


function Bytes = varBytes(V)
    % Estimated size [bytes] of a variable as save() writes it.
    %   whos counts a handle object as 8 bytes, so the data held in the
    %   handle objects is added (see handleBytes).
    W     = whos('V');
    Bytes = W.bytes + handleBytes(V);
end


function Bytes = handleBytes(V)
    % Size [bytes] of the data in handle objects that whos does not count.
    %   For a handle object, the stored public properties are summed;
    %   handle objects in them (e.g. Config, Dictionary) are not counted.
    %   Cells and structs are searched for handle objects.
    Bytes = 0;
    if isa(V, 'handle')
        MC    = metaclass(V);
        Prop  = MC.PropertyList;
        Prop  = Prop(~[Prop.Dependent] & ~[Prop.Constant] & ~[Prop.Transient] & strcmp({Prop.GetAccess}, 'public'));
        Names = {Prop.Name};
        for I=1:1:numel(V)
            for J=1:1:numel(Names)
                Val = V(I).(Names{J});
                if ~isa(Val, 'handle')
                    Bytes = Bytes + varBytes(Val);
                end
            end
        end
    elseif iscell(V) || isstruct(V)
        if isstruct(V)
            V = struct2cell(V);
        end
        IsH = cellfun(@(X) isa(X, 'handle') || iscell(X) || isstruct(X), V);
        if any(IsH(:))
            Bytes = sum(cellfun(@handleBytes, V(IsH)));
        end
    end
end

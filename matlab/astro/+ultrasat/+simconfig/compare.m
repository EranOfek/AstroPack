function Diff = compare(A, B, Args)
    % The fields in which two simulation configurations differ
    %   Walks both structures recursively and lists every field path whose
    %   value differs (isequaln), or that exists in only one of them. The
    %   top-level fields in 'Ignore' (default: Record, the run record that
    %   ultrasat.simconfig.write adds) are skipped.
    % Input  : - A, B: configurations (structures).
    %          * ...,key,val,...
    %            'Ignore' - top-level fields not compared. Default is "Record".
    % Output : - Diff: a string column of field paths, e.g. "Usim.JitterPar";
    %               empty when the configurations agree.
    % Author : D. Kovaleva (Oct 2026)
    % Example: Diff = ultrasat.simconfig.compare(ultrasat.dut.config, ultrasat.dut.config('NewRoll', 6))
    %          % "New.Roll"

    arguments
        A (1,1) struct
        B (1,1) struct
        Args.Ignore string = "Record"
    end

    A = rmfield(A, intersect(fieldnames(A), cellstr(Args.Ignore)));
    B = rmfield(B, intersect(fieldnames(B), cellstr(Args.Ignore)));
    Diff = walk(A, B, "");
end

function Diff = walk(A, B, Prefix)
    % the differing field paths below Prefix
    Diff  = strings(0, 1);
    Names = union(fieldnames(A), fieldnames(B), 'stable');
    for I = 1:numel(Names)
        Path = Names{I};
        if Prefix ~= ""
            Path = Prefix + "." + Path;
        end
        InA = isfield(A, Names{I});
        InB = isfield(B, Names{I});
        if InA && InB
            Va = A.(Names{I});
            Vb = B.(Names{I});
            if isstruct(Va) && isstruct(Vb) && isscalar(Va) && isscalar(Vb)
                Diff = [Diff; walk(Va, Vb, Path)]; %#ok<AGROW>
            elseif ~isequaln(Va, Vb)
                Diff = [Diff; string(Path)]; %#ok<AGROW>
            end
        else
            Diff = [Diff; string(Path)]; %#ok<AGROW>
        end
    end
end

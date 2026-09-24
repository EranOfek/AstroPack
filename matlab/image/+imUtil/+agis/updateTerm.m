function State = updateTerm(State, Term, Epsilon)
    % Add a solved parameter increment to a term's current parameters,
    % then apply an optional gauge-fixing hook (Term.PinFun), if present.
    % Input  : - State   : struct with field .Params.
    %          - Term    : term struct (needs .Name, .NParams; optionally
    %                      .PinFun, a function handle State->State applied
    %                      after the update to enforce constraints such as
    %                      pinning a reference epoch -- see imUtil.agis.affine).
    %          - Epsilon : [NBlocks*NParams x 1] increment from bicg.
    % Output : - State : updated struct.
    % Author : N. Segev / imUtil.agis rewrite
    % Example: State = imUtil.agis.updateTerm(State,Term,Epsilon);

    arguments
        State (1,1) struct
        Term (1,1) struct
        Epsilon (:,1) double
    end

    if ~isfield(State.Params, Term.Name)
        error('imUtil:agis:updateTerm:missingTerm', ...
            'State.Params has no entry for term "%s".', Term.Name);
    end

    CurSize = size(State.Params.(Term.Name));
    DeltaP = reshape(Epsilon, Term.NParams, []);

    if ~isequal(size(DeltaP), CurSize)
        error('imUtil:agis:updateTerm:sizeMismatch', ...
            'Increment size [%d x %d] does not match parameter size [%d x %d] for term "%s".', ...
            size(DeltaP,1), size(DeltaP,2), CurSize(1), CurSize(2), Term.Name);
    end

    State.Params.(Term.Name) = State.Params.(Term.Name) + DeltaP;

    if isfield(Term, 'PinFun') && ~isempty(Term.PinFun)
        State = Term.PinFun(State);
    end
end

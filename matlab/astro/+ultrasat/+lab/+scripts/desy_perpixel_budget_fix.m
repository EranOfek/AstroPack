% Recompute the stored noise budgets of desy_perpixel_run.m in place.
%   One-off, needed only for output produced before budgetCurve corrected the
%   sign convention of the threshold: a NEGATIVE threshold means charge is
%   present at zero intensity (a constant offset removed by the bias and dark
%   subtraction), not extra collected signal, so Qc = Q - max(T,0) and never
%   exceeds Q. The old max(Q-T,0) handed the TX >= 3.7 V setups, whose light-
%   method threshold is negative, a limiting signal they do not have (run 38-2
%   came out at 5.5 e- instead of ~40 e-).
%   The budget is a pure function of scalars that are already in the file, so
%   nothing has to be read again and nothing else in the file changes.
Dir   = '/home/sasha/claude/desy_perpixel';
Names = [{'perpixel.json'}, {dir(fullfile(Dir, 'perpixel_patch*.json')).name}];
for If = 1:1:numel(Names)
    Path = fullfile(Dir, Names{If});
    if ~isfile(Path)
        continue
    end
    Txt = fileread(Path);
    D   = jsondecode(Txt);
    if ~isfield(D, 'Full') || isempty(D.Full)
        continue
    end
    Full = D.Full;
    if ~iscell(Full)
        Full = num2cell(Full);
    end
    Nfix = 0;
    for Ie = 1:1:numel(Full)
        E = Full{Ie};
        if ~isfield(E, 'Budget')
            continue
        end
        for Pn = reshape(fieldnames(E.Budget), 1, [])
            for Mt = reshape(fieldnames(E.Budget.(Pn{1})), 1, [])
                B = E.Budget.(Pn{1}).(Mt{1});
                N = ultrasat.lab.PTCAnalysis.budgetCurve(reshape(B.Q, 1, []), B);
                N.Parity = Pn{1};  N.Method = Mt{1};
                if isfield(B, 'Gain'), N.Gain = B.Gain; end
                E.Budget.(Pn{1}).(Mt{1}) = N;
                Nfix = Nfix + 1;
            end
        end
        Full{Ie} = E;
    end
    D.Full = Full;
    Fid = fopen(Path, 'w');  fwrite(Fid, jsonencode(D));  fclose(Fid);
    fprintf('%s: %d budgets recomputed for %d die-runs\n', Names{If}, Nfix, numel(Full));
end
fprintf('BUDGETFIX DONE\n');

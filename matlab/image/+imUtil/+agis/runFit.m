function [State, History] = runFit(Data, Terms, Args)
    % Top-level driver: run the full AGIS-style iterative fit.
    % Input  : - Data  : struct from imUtil.agis.buildFitData.
    %          - Terms : [1 x Nterm] struct array of terms, applied in order
    %                    on every sweep.
    %          * ...,key,val,...
    %            'NIter'            - number of full sweeps over Terms. Default 10.
    %            'WeightUpdateEvery'- recompute weights every N sweeps. Default 1.
    %            'UseWeights'       - if false, uniform weights throughout. Default true.
    %            'Tol'              - bicg tolerance. Default 1e-9.
    %            'MaxIt'            - bicg max iterations. Default 100.
    %            'Verbose'          - print per-term RMS each sweep. Default false.
    %            'InitialState'     - struct with field .Params to warm-start
    %                                 from (e.g. the output of a previous
    %                                 runFit call). Default: [] -> fresh init.
    % Output : - State   : final struct with field .Params.
    %          - History : struct array, one entry per (sweep,term).
    % Author : N. Segev / imUtil.agis rewrite
    % Example:
    %   [S1,H1] = imUtil.agis.runFit(Data,Terms,'UseWeights',false);
    %   [S2,H2] = imUtil.agis.runFit(Data,Terms,'UseWeights',true,'InitialState',S1);

    arguments
        Data (1,1) struct
        Terms (1,:) struct
        Args.NIter (1,1) double {mustBePositive, mustBeInteger} = 10
        Args.WeightUpdateEvery (1,1) double {mustBePositive, mustBeInteger} = 1
        Args.UseWeights (1,1) logical = true
        Args.Tol (1,1) double = 1e-9
        Args.MaxIt (1,1) double = 100
        Args.Verbose (1,1) logical = false
        Args.InitialState = []
    end

    if isempty(Args.InitialState)
        State = imUtil.agis.initState(Data, Terms);
    else
        State = Args.InitialState;
    end

    W = ones(Data.Nepoch, Data.Nsrc);

    History = struct('Iter', {}, 'Term', {}, 'RMS2D', {}, 'BicgFlag', {});

    for It = 1:Args.NIter
        if Args.UseWeights && mod(It-1, Args.WeightUpdateEvery) == 0
            [Rx, Ry] = imUtil.agis.computeResiduals(Data, Terms, State);
            W = imUtil.agis.computeWeights(Data, Rx, Ry);
        end

        for Ik = 1:numel(Terms)
            if ~Terms(Ik).Active
                continue
            end

            [State, Diag] = imUtil.agis.stepTerm(Terms, Ik, Data, State, W, ...
                'Tol', Args.Tol, 'MaxIt', Args.MaxIt);

            History(end+1) = struct('Iter', It, 'Term', Diag.Name, ...
                'RMS2D', Diag.RMS2D, 'BicgFlag', Diag.BicgFlag); %#ok<AGROW>

            if Args.Verbose
                fprintf('Iter %2d | %-14s | RMS2D = %8.4f | bicg flag = %d\n', ...
                    It, Diag.Name, Diag.RMS2D, Diag.BicgFlag);
            end
        end
    end
end

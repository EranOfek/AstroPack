function [Result] = unitTest()
   % unitTest for timeSeries.fit package

   %% timeSeries.fit.fitLinearSegmentsDP

   %% Test fitLinearSegmentsDP
    % This script tests fitLinearSegmentsDP using simulated piecewise-linear data.
    % 
    % clear;
    % close all;
    % clc;
    
    %% Parameters
    
    Ntime = 300;
    Nsrc  = 3;
    Nseg  = 3;
    
    Sigma = 0.15;
    
    rng(1);
    
    T = sort(100 .* rand(Ntime,1));
    
    M = NaN(Ntime, Nsrc);
    TrueSeg = cell(1, Nsrc);
    TrueBreak = cell(1, Nsrc);
    
    %% Generate synthetic piecewise-linear light curves
    
    for Isrc = 1:Nsrc
    
        % Different break points for each source
        BreakT = sort(100 .* rand(Nseg - 1,1));
        EdgeT = [-Inf; BreakT(:); Inf];
    
        TrueBreak{Isrc} = BreakT;
    
        % True slopes and intercepts
        Slope = [-0.03; 0.04; -0.01] + 0.01 .* randn(Nseg,1);
        Intercept = [3; -2; 5] + randn(Nseg,1);
    
        TrueSeg{Isrc} = [Slope.'; Intercept.'];
    
        Y = NaN(Ntime,1);
    
        for Iseg = 1:Nseg
            Flag = T > EdgeT(Iseg) & T <= EdgeT(Iseg + 1);
            Y(Flag) = Slope(Iseg).*T(Flag) + Intercept(Iseg);
        end
    
        % Add Gaussian noise
        M(:,Isrc) = Y + Sigma .* randn(Ntime,1);
    
    end
    
    %% Define errors
    
    % Test scalar error
    ErrorM = Sigma;
    
    % You may also test these:
    % ErrorM = Sigma .* ones(Ntime,1);
    % ErrorM = Sigma .* ones(Ntime,Nsrc);
    
    %% Run dynamic-programming segmented fit
    
    Result = timeSeries.fit.fitLinearSegmentsDP(T, M, ErrorM, ...
        'Nseg', Nseg, ...
        'MinNpt', 5, ...
        'SortT', true);
    
    %% Display numerical results
    
    for Isrc = 1:Nsrc
    
        fprintf('\nSource %d\n', Isrc);
        fprintf('Fit success: %d\n', Result(Isrc).Flag);
    
        fprintf('\nFitted segments:\n');
        fprintf(' Iseg      T1        T2        Npt       Slope        Intercept       Chi2      Dof\n');
    
        for Iseg = 1:Nseg
            fprintf('%5d  %8.3f  %8.3f  %5d  %11.5f  %13.5f  %9.3f  %5d\n', ...
                Iseg, ...
                Result(Isrc).Tlim(1,Iseg), ...
                Result(Isrc).Tlim(2,Iseg), ...
                Result(Isrc).Npt(Iseg), ...
                Result(Isrc).Seg(1,Iseg), ...
                Result(Isrc).Seg(2,Iseg), ...
                Result(Isrc).Chi2(Iseg), ...
                Result(Isrc).Dof(Iseg));
        end
    
        fprintf('\nTrue segments used to generate data:\n');
        disp(array2table(TrueSeg{Isrc}.', ...
            'VariableNames', {'Slope','Intercept'}));
    
    end
    
    %% Plot results
    
    for Isrc = 1:Nsrc
    
        figure;
        hold on;
        box on;
        grid on;
    
        plot(T, M(:,Isrc), 'k.', 'DisplayName', 'Data');
    
        for Iseg = 1:Nseg
    
            Ind1 = Result(Isrc).Ind(1,Iseg);
            Ind2 = Result(Isrc).Ind(2,Iseg);
    
            TsegMin = Result(Isrc).Tlim(1,Iseg);
            TsegMax = Result(Isrc).Tlim(2,Iseg);
    
            Tplot = linspace(TsegMin, TsegMax, 100).';
    
            Slope = Result(Isrc).Seg(1,Iseg);
            Intercept = Result(Isrc).Seg(2,Iseg);
    
            Yplot = Slope .* Tplot + Intercept;
    
            plot(Tplot, Yplot, 'LineWidth', 2, ...
                'DisplayName', sprintf('Fit seg %d', Iseg));
    
            xline(TsegMin, '--', 'HandleVisibility', 'off');
    
            if Iseg == Nseg
                xline(TsegMax, '--', 'HandleVisibility', 'off');
            end
        end
    
        xlabel('T');
        ylabel('M');
        title(sprintf('Source %d: dynamic-programming segmented linear fit', Isrc));
        legend('Location', 'best');
    
    end
    
    %% Sanity checks
    
    for Isrc = 1:Nsrc
    
        assert(Result(Isrc).Flag, 'Fit failed for source %d', Isrc);
    
        assert(all(isfinite(Result(Isrc).Seg(:))), ...
            'Non-finite fitted parameters for source %d', Isrc);
    
        assert(all(Result(Isrc).Npt >= 5), ...
            'A segment has fewer than MinNpt points for source %d', Isrc);
    
        assert(numel(Result(Isrc).Chi2) == Nseg, ...
            'Wrong number of Chi2 values for source %d', Isrc);
    
    end
    
    fprintf('\nAll tests completed successfully.\n');


    %%
    T = (1:30).';
    Err = 0.1;
    Nsrc = 100;
    Par1 = rand(2,Nsrc);
    Par2 = rand(2,Nsrc);
    Par3 = rand(2,Nsrc);
    M    = zeros(30,Nsrc);
    for Isrc=1:1:Nsrc
        M(1:11,Isrc) = polyval(Par1(:,Isrc), T(1:11)) + randn(11,1).*Err;
        M(12:20,Isrc) = polyval(Par2(:,Isrc), T(12:20)) + randn(9,1).*Err;
        M(21:30,Isrc) = polyval(Par3(:,Isrc), T(21:30)) + randn(10,1).*Err;
    end

    tic;
    Result = timeSeries.fit.fitLinearSegmentsDP(T, M, Err, ...
        'Nseg', 3, ...
        'MinNpt', 3, ...
        'SortT', true);
    toc
    tic;
    Result1 = fitPiecewiseLinear(T, M, Err, ...
        'Nseg', 3, ...
        'MinPts', 3);
    toc

    %%

   Result = true;
end

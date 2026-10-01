% Region PTC of both ladders with robust per-step variance statistics (mean, clipped mean, median/ln2)
Root = '/bigdata3/projects/ultrasat/DESY';
Runs = {'31','LOT_TH02954_31_FT_PTCint_-50_2026-08-25','W04_D07';
        '31','LOT_TH02954_31_FT_PTCint_-50_2026-08-25','W08_D02';
        '32','LOT_TH02954_32_FT_PTCint_-50_2026-08-27','W04_D07'};
Out = {};
for I = 1:size(Runs,1)
    Dev = fullfile(Root, Runs{I,2}, ['LOT_TH02954_', Runs{I,3}]);
    fprintf('%s %s\n', Runs{I,1}, Runs{I,3});
    P = ultrasat.lab.PTCAnalysis(Dev, 'FitSteps',struct('D','auto','B','auto'));
    P.run;
    S = struct('Run',Runs{I,1}, 'Die',Runs{I,3}, 'ExpSen',P.ExpSen, ...
               'Gain',P.PTC.Fit.temporal.Gain, 'Offset',P.PTC.Fit.temporal.Offset, ...
               'RNrms',P.ZeroStats.ReadNoiseTemporalRMS, 'RNmed',P.ZeroStats.ReadNoiseTemporal, ...
               'RNdiff',P.ZeroStats.ReadNoiseDiff, 'NZero',P.ZeroStats.Nframes, ...
               'DarkSlope',P.DarkFit.MedianSlope, 'DarkIntercept',P.DarkFit.MedianIntercept, ...
               'BrightSlope',P.BrightFit.MedianSlope, 'BrightIntercept',P.BrightFit.MedianIntercept);
    % robust read noise from the ZE cube on the same footing as the ladders
    Z2 = reshape(double(P.ZeroNoise).^2, [], 1);
    S.RN2mean = mean(Z2, 'omitnan');
    S.RN2clip = mean(Z2(Z2 <= quantile(Z2, 0.99)), 'omitnan');
    S.RN2med  = median(Z2, 'omitnan')./log(2);
    for T = {'Dark','Bright'}
        L  = P.(T{1});  Ns = numel(L.X);
        V  = double(reshape(L.VarTemporal, [], Ns));       % per-pixel temporal variance
        M  = double(reshape(L.Mean, [], Ns));
        Q  = struct('X',L.X, 'Nframes',L.Nframes, 'Mean',mean(M,1,'omitnan'), 'MeanMed',median(M,1,'omitnan'), ...
                    'VarMean',mean(V,1,'omitnan'), 'VarMed',median(V,1,'omitnan')./log(2), ...
                    'VarClip',nan(1,Ns), 'FracHit',nan(1,Ns));
        for Is = 1:Ns
            Vi = V(:,Is);  Cut = quantile(Vi, 0.99);
            Q.VarClip(Is) = mean(Vi(Vi<=Cut), 'omitnan');
            Q.FracHit(Is) = mean(Vi > 10.*median(Vi,'omitnan'), 'omitnan');
        end
        S.(T{1}) = Q;
    end
    Out{end+1} = S; %#ok<SAGROW>
end
fid = fopen('/home/sasha/claude/ptc_gain_check/dark_light2.json','w'); fwrite(fid, jsonencode(Out)); fclose(fid);
fprintf('DARKLIGHT2 DONE\n');

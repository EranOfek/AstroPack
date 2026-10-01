% Per-pixel mean and temporal variance of both ladders + per-pixel ZE noise, DESY region
Root = '/bigdata3/projects/ultrasat/DESY';
Out  = '/home/sasha/claude/ptc_gain_check/perpixel';
if ~isfolder(Out), mkdir(Out); end
Runs = {'31','LOT_TH02954_31_FT_PTCint_-50_2026-08-25','W04_D07';
        '31','LOT_TH02954_31_FT_PTCint_-50_2026-08-25','W08_D02';
        '32','LOT_TH02954_32_FT_PTCint_-50_2026-08-27','W04_D07'};
Meta = {};
for I = 1:size(Runs,1)
    Tag = sprintf('run%s_%s', Runs{I,1}, Runs{I,3});
    Dev = fullfile(Root, Runs{I,2}, ['LOT_TH02954_', Runs{I,3}]);
    fprintf('%s\n', Tag);
    P = ultrasat.lab.PTCAnalysis(Dev, 'FitSteps',struct('D','auto','B','auto'));
    P.run;
    for T = {'Dark','Bright'}
        L = P.(T{1});
        for F = {'Mean','VarTemporal'}
            fid = fopen(fullfile(Out, sprintf('%s_%s_%s.bin', Tag, lower(T{1}), lower(F{1}))), 'w');
            fwrite(fid, single(L.(F{1})), 'single');  fclose(fid);
        end
    end
    fid = fopen(fullfile(Out, [Tag '_zeronoise.bin']), 'w'); fwrite(fid, single(P.ZeroNoise), 'single'); fclose(fid);
    M = struct('Tag',Tag, 'Run',Runs{I,1}, 'Die',Runs{I,3}, 'Size',size(P.Zero), ...
               'DarkX',P.Dark.X, 'BrightX',P.Bright.X, 'NframesD',P.Dark.Nframes, 'NframesB',P.Bright.Nframes, ...
               'NZero',P.ZeroStats.Nframes, 'Gain',P.PTC.Fit.temporal.Gain, 'Offset',P.PTC.Fit.temporal.Offset, ...
               'RegionDarkMean',P.Dark.RegionMean, 'RegionDarkVar',P.Dark.RegionVarTemporal, ...
               'RegionBrightMean',P.Bright.RegionMean, 'RegionBrightVar',P.Bright.RegionVarTemporal, ...
               'ExpSen',P.ExpSen, 'DarkSlope',P.DarkFit.MedianSlope, 'BrightIntercept',P.BrightFit.MedianIntercept);
    Meta{end+1} = M; %#ok<SAGROW>
end
fid = fopen(fullfile(Out, 'meta.json'), 'w'); fwrite(fid, jsonencode(Meta)); fclose(fid);
fprintf('PERPIXEL DONE\n');

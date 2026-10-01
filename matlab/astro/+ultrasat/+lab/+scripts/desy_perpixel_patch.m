% Re-run selected die-runs of desy_perpixel_run.m against the NFS share.
%   The local mirror /Data1/DESY has one corrupt sidecar (verified 2026-09-30
%   by comparing the sizes of all 9783 files of runs 31-40: exactly one
%   mismatch, LOT_TH02954_36 .../W08_D04/PTC_int_hr/PTC_Config.xlsx, 10293
%   bytes locally against 10117 on the share, and the local copy is not a
%   valid zip). Those dies are reduced from /bigdata3 instead, with the same
%   settings as the main driver, and written to perpixel_patch.json for the
%   report builder to merge. The mirror itself is left untouched.
Root   = '/bigdata3/projects/ultrasat/DESY';
OutDir = '/home/sasha/claude/desy_perpixel';
LinLimit = 2900;
Qgrid    = logspace(0, 3, 61);
%        id      folder                                        settings TX   RSTH  die
Cases = {'36',  'LOT_TH02954_36_FT_PTCint_-50_2026-09-02', 'aSpect', 3.3, 3.0, 'W08_D04'};
Flav  = containers.Map({'W04','W08'}, {6, 2});
Res   = struct('Full', {{}}, 'LinLimit',LinLimit, 'Qgrid',Qgrid);
T0    = tic;
for I = 1:1:size(Cases,1)
    Die = Cases{I,6};
    Tag = sprintf('run%s_%s', Cases{I,1}, Die);
    fprintf('[%6.0f s] %s (from the share)\n', toc(T0), Tag);
    try
        P = ultrasat.lab.PTCAnalysis(fullfile(Root, Cases{I,2}, ['LOT_TH02954_', Die]), ...
                                     'FitSteps',struct('D','auto','B','auto'), 'Parity','rawcol');
        P.run;
        S = struct('Tag',Tag, 'Run',Cases{I,1}, 'Settings',Cases{I,3}, 'TX',Cases{I,4}, ...
                   'RSTH',Cases{I,5}, 'Die',Die, 'Flavour',Flav(Die(1:3)), ...
                   'Pass',P.Sidecar.Result.Trailer.Pass, 'GainUsed',P.PTC.GainUsed, ...
                   'GainTemporal',P.PTC.Fit.temporal.Gain, 'ExpSen',P.ExpSen);
        B = P.badColumns;
        S.Bad = struct('Nbad',B.Nbad, 'Nrawcol',B.Nrawcol, 'BadRawCol',B.BadRawCol, ...
                       'NoiseProfile',B.NoiseProfile, 'RespProfile',B.RespProfile);
        Zn = P.zeroNoiseStats('Mask',B.GoodMask);
        S.Zero = Zn;
        FD = P.perPixelFits('D', 'LinLimit',LinLimit, 'Mask',B.GoodMask);
        FB = P.perPixelFits('B', 'LinLimit',LinLimit, 'Mask',B.GoodMask);
        S.DarkFit   = stripMaps(FD);
        S.BrightFit = stripMaps(FB);
        S.PatternB  = P.stepFixedPattern('B', 'Mask',B.GoodMask);
        S.PatternD  = P.stepFixedPattern('D', 'Mask',B.GoodMask);
        Th = P.perPixelThreshold('DarkFit',FD, 'BrightFit',FB, 'Pattern',S.PatternB, 'Mask',B.GoodMask);
        S.Threshold = stripMaps(Th);
        for Pn = {'All','Even','Odd'}
            for Mt = {'light','dark','none'}
                S.Budget.(Pn{1}).(Mt{1}) = P.noiseBudget('Threshold',Th, 'Zero',Zn, ...
                                                         'Parity',Pn{1}, 'Method',Mt{1}, 'Q',Qgrid);
            end
        end
        Res.Full{end+1} = S; %#ok<SAGROW>
    catch ME
        fprintf('  FAILED: %s\n', ME.message);
    end
end
Fid = fopen(fullfile(OutDir, 'perpixel_patch.json'), 'w');
fwrite(Fid, jsonencode(Res));
fclose(Fid);
fprintf('[%6.0f s] PATCH DONE\n', toc(T0));

function S = stripMaps(S)
    for F = {'Slope','Intercept','VarSlope','VarIntercept','CovSlopeIntercept','ResidRMS', ...
             'Chi2Dof','Nused','DarkADU','DarkE','LightADU','LightE','VarDarkADU','VarLightADU', ...
             'DCADU','DCE','VarDCADU','RespADU','VarRespADU'}
        if isfield(S, F{1})
            S = rmfield(S, F{1});
        end
    end
end

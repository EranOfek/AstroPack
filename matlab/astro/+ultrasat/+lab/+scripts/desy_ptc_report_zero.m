% Bias level and read noise from the ZE frames of all TH02954 dies (runs 31, 32),
% DESY region and 400x400 region, with the raw-column parity split; appended to results.json.
Root = '/bigdata3/projects/ultrasat/DESY';  OutDir = '/home/sasha/claude/desy_ptc_report';
Runs = {'31','LOT_TH02954_31_FT_PTCint_-50_2026-08-25'; '32','LOT_TH02954_32_FT_PTCint_-50_2026-08-27'};
Dies = {'W04_D03','W04_D05','W04_D06','W04_D07','W08_D02','W08_D03','W08_D04'};
Secs = {'desy', [1361 1460 1861 1960]; 'big', [1161 1560 1661 2060]};
Out = {};
for Ir = 1:2
    for Id = 1:numel(Dies)
        Dev = fullfile(Root, Runs{Ir,2}, ['LOT_TH02954_', Dies{Id}]);
        for Is = 1:size(Secs,1)
            AI = ultrasat.lab.readPTC(Dev, 'FrameType','ZE', 'CCDSEC',Secs{Is,2});
            Cube = single(cat(3, AI.Image));
            Zero = mean(Cube, 3);  Noise = std(Cube, 0, 3);
            H = AI(1).HeaderData;
            RawSec = sscanf(H.getVal('RAWSEC'), '[%d:%d,%d:%d]').';
            RawCol = RawSec(2) - H.getVal('RAWXOFF') - (0:size(Zero,1)-1).';       % DESY orientation: per row
            Odd    = repmat(mod(RawCol,2)==1, 1, size(Zero,2));
            Z = ultrasat.lab.PTCAnalysis.zeroStats(Cube, Zero, Noise, true(size(Zero)));
            Z.Even = ultrasat.lab.PTCAnalysis.zeroStats(Cube, Zero, Noise, ~Odd);
            Z.Odd  = ultrasat.lab.PTCAnalysis.zeroStats(Cube, Zero, Noise,  Odd);
            Z.Run = Runs{Ir,1};  Z.Die = Dies{Id};  Z.Region = Secs{Is,1};  Z.CCDSEC = Secs{Is,2};
            Out{end+1} = Z; %#ok<SAGROW>
            fprintf('%s %s %s: bias %.2f (std %.2f)  RN temporal %.3f diff %.3f spatial %.3f | even/odd bias %.2f/%.2f RN %.3f/%.3f\n', ...
                Z.Run, Z.Die, Z.Region, Z.BiasLevel, Z.BiasStd, Z.ReadNoiseTemporal, Z.ReadNoiseDiff, Z.ReadNoiseSpatial, ...
                Z.Even.BiasLevel, Z.Odd.BiasLevel, Z.Even.ReadNoiseTemporal, Z.Odd.ReadNoiseTemporal);
        end
    end
end
R = jsondecode(fileread(fullfile(OutDir, 'results.json')));
R.Zero = Out;
fid = fopen(fullfile(OutDir, 'results.json'), 'w'); fwrite(fid, jsonencode(R)); fclose(fid);
disp done

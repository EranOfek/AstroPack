% Export the TH02954 flavour-test frames (runs 31, 32; 7 dies; both gains) as FITS
Root = '/bigdata3/projects/ultrasat/DESY';  Out = '/Data1/DESY_FITS';
Runs = {'LOT_TH02954_31_FT_PTCint_-50_2026-08-25', 'LOT_TH02954_32_FT_PTCint_-50_2026-08-27'};
Dies = {'W04_D03','W04_D05','W04_D06','W04_D07','W08_D02','W08_D03','W08_D04'};
T0 = tic;  N = 0;
for Ir = 1:numel(Runs)
    for Id = 1:numel(Dies)
        Dev = fullfile(Root, Runs{Ir}, ['LOT_TH02954_', Dies{Id}]);
        OutDir = fullfile(Out, Runs{Ir}, ['LOT_TH02954_', Dies{Id}]);
        Files = ultrasat.lab.writeFITS(Dev, OutDir, 'Gain','both');
        N = N + numel(Files);
        fprintf('[%6.0f s] %s %s: %d files, total %d\n', toc(T0), Runs{Ir}(13:14), Dies{Id}, numel(Files), N);
    end
end
fprintf('[%6.0f s] done, %d files\n', toc(T0), N);

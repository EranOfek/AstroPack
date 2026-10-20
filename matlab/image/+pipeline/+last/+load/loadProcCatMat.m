function [Cat, Head, Info] = loadProcCatMat(VisitDir, Args)
% loadProcCatMat Load proc Cat/Head MAT from a LAST visit directory.
% Forwards to pipeline.last.load.loadEpochCatMat (issue #1417 proc MAT in visit dir).
% Input/Output : same as loadEpochCatMat.
% Example: [Cat, Head, Info] = pipeline.last.load.loadProcCatMat(DataDir);

    arguments
        VisitDir             = pwd;
        Args.Read            = "both";
        Args.Counter         = [];
        Args.CropID          = [];
        Args.CatTemplate     = '*_sci_proc_Cat_1.mat';
        Args.HeadTemplate    = '*_sci_proc_Head_1.mat';
    end
    [Cat, Head, Info] = pipeline.last.load.loadEpochCatMat(VisitDir, Args);
end

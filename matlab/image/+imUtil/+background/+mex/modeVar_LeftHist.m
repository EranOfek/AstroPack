% Mex for Global image background mode and variance via a left-flank histogram fit.
%   Mex version of: imUtil.background.modeVar_LeftHist
%   Estimate the background level B of an image whose pixel histogram is a
%   Gaussian sky contaminated on its positive (right) side by sources. The
%   peak is located and the (uncontaminated) left flank is fit, with the
%   width NOT a free parameter: under background-limited noise it is fixed
%   by Sigma0^2 = B/VarianceRatio. The known curvature is removed from the
%   log-counts and the location is a weighted LINEAR regression on the left
%   flank, so the estimated peak is orthogonal to the width by construction
%   (no peak/sigma covariance). The variance is then measured in a decoupled
%   second stage with the peak FIXED, so it cannot feed back into the level.
%   The estimator targets the true B (not the histogram mode); see
%   imUtil.background.histogramModel for the mode/B offset.
%
%   This is a thin interface to the compiled MEX
%   imUtil.background.mex.modeVar_LeftHist (default), with an exact
%   pure-MATLAB fallback selectable via 'UseMex' (or used automatically if
%   the MEX is not on the path). Build the MEX with, e.g.:
%     mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -fopenmp' ...
%         LDFLAGS='$LDFLAGS -fopenmp' modeVar_LeftHist.cpp
%
% Input  : - Image : A 2-D image (or any numeric array). Non-finite pixels
%                     are ignored. MEX path supports double/single/uint16/
%                     int16/uint32/int32; pure path accepts any numeric.
%          * ...,key,val,...
%            'VarianceRatio' - B/Var(noise), i.e. the gain, so the noise
%                     variance is Sigma0^2 = B/VarianceRatio. Pure Poisson
%                     photons -> 1. Default is 1.
%            'BinFactor' - Histogram bin width in units of Sigma0.
%                     Default is 0.2.
%            'RangeLo' - Lower histogram extent (Sigma0) below the center.
%                     Default is 5.
%            'RangeHi' - Upper histogram extent (Sigma0) above the center.
%                     Default is 5.
%            'WinLo' - Fit-window extent below the peak (Sigma0); wide, to
%                     anchor the clean left flank. Default is 3.
%            'WinHi' - Fit-window extent above the peak (Sigma0); keep small
%                     so source fill-in does not bias the level up.
%                     Default is 0.5.
%            'SmoothBins' - Moving-average width (bins) for peak finding.
%                     Default is 3.
%            'Niter' - Number of refinement passes. Default is 1.
%            'MinBins' - Minimum populated bins in the window to fit, else
%                     fall back to the SExtractor mode. Default is 5.
%            'UseMex' - Use the compiled MEX (true) or the pure-MATLAB path
%                     (false). Default is true.
%            'FastMedian' - (MEX only) Use the fast single-pass core (true)
%                     or the exact nth_element core (false, bit-faithful to
%                     the pure path) at ~5x the cost. Ignored by the pure
%                     path. Default is true.
%            'OS' - (MEX only) Fine-histogram oversampling vs the working
%                     bin in the fast core; larger -> closer to exact
%                     binning. Ignored by the pure path. Default is 16.
% Output : - Back : Estimated background level B (targets B).
%          - Var  : Background noise variance, measured with the peak fixed.
%                   Compare to Info.VarPred; Var >> VarPred flags residual
%                   contamination / a non-background-limited field.
%          - Info : Structure with: .Method ('fit'|'fallback'), .Mode (raw
%                   histogram peak), .Sigma0, .VarPred (=Back/VarianceRatio),
%                   .Npix, .Nbins, .Niter, .Median, .Mean.
% Author : Claude + Eran Ofek (Jun 2026)
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' modeVar_LeftHist.cpp
% Example: Image = 1000 + sqrt(1000)*randn(1024,1024);
%          [Back,Var,Info] = imUtil.background.mex.modeVar_LeftHist(Image);
%          % exact pure-MATLAB path:
%          [Back,Var] = imUtil.background.mex.modeVar_LeftHist(Image,'UseMex',false);
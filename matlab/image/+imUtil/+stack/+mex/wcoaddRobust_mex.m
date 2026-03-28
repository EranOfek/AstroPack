% Fast mex for weighted and robust coaddition of images
%   The function performs weighted coaddition where the weights are
%   (F/Var), and F is the flux zeri point and Var is the background
%   variance.
%   In addition it performs robust outliers removal in two steps.
%   First, an optional single min/max rejection, followed by an
%   optional sigma clipping.
%   For the Sigma clipping Std calculation, 3 methods are available.
% Input  : - Image cube, where the image index is in the 3rd dim.
%          - Background. If empty, assumes that the images are
%            background subtracted. Otherwise, can be a scalar, vector
%            (with length like the number of images), or cube.
%          - (Var) Scalar, vector, or cube of variance.
%            Default is 1.
%          - (F) Flux matching factor, per image. Each image is
%            multiplied by F in order to bring it to a common
%            zero point.
%            Default is [].
%          - (ZP) Zero point for calculating F.
%            Default is 25.
%          - (ZP0) Arbitrary common ZP offset. Default is 25.
%          - (RemoveMinMax) Remove min and max values in each pixel
%             prior to coaddition.
%             Default is true.
%          - (Niter) Number of sigma clipping iterations.
%            For Niter=0 no sigma clipping is done.
%            Default is 1.
%          - (SigmaClip) [Low High] sigma clipping thresholds.
%            Units are standard deviations.
%            Default is [3 3].
%          - (StdMethod) Method to estimate the scatter for sigma clipping:
%                    1 = std around weighted mean.
%                    2 = scaled mean absolute deviation around weighted
%                        mean.
%                    3 = scaled weighted median absolute deviation
%                        around weighted median.
%                    Default is 2.
% Output  : - Weighted coadd image on the common photometric scale.
%             The flux scale of the coadd image is always 1 (in units of
%             'F'). If ZP is used then the ZP is ZP0.
%           - The coadd variance image (or scalar).
% Author : Eran Ofek (Mar 2026)
% Example: [C, Cvar] = imUtil.stack.wcoaddRobust(Im, B, 'Var',V, F_k, ZP,ZP0,true,1,[3 3],3);
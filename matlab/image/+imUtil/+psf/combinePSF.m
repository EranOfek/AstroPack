function [CombPSF, CombVar] = combinePSF(PSF, Args)
    % Combine a set of PSF stamps into a single weighted-mean PSF stamp
    %     Each input stamp is (optionally) normalized before the combination,
    %     so that stamps carrying different total fluxes contribute according
    %     to the requested weights only. The weights are normalized to a unit
    %     sum, hence the variance of the result is propagated as the variance
    %     of a weighted mean of independent estimates: Var = sum(W_i^2*Var_i).
    % Input  : - A set of PSF stamps, given either as
    %            a cell array, one stamp per cell, where a stamp may have any
    %            number of dimensions beyond the two pixel dimensions
    %            (e.g. a 'Purpose'-dimensioned photometry/detection cube), or
    %            a numeric array [Ny, Nx, Npsf], in which the PSF index is the
    %            3rd dimension (extra stamp dimensions are not supported in
    %            this form - use the cell array instead).
    %            All the stamps must be of the same size.
    %          * ...,key,val,...
    %            'Weights' - A vector of Npsf weights. Only their ratios
    %                   matter, as they are normalized to a unit sum here.
    %                   If empty, equal weights are used. Default is [].
    %            'Var' - The variances of the input stamps, in the same form
    %                   and of the same size as the first input argument.
    %                   If empty, the second output is empty. Default is [].
    %            'Norm' - A logical indicating whether to normalize each of
    %                   the input stamps (scaling its variance accordingly)
    %                   before the combination, and to renormalize the
    %                   result afterwards. Default is true.
    %            'ReNormMethod' - The normalization method, as in
    %                   imUtil.psf.normPSF: 'int' | 'rms'. Default is 'int'.
    % Output : - The combined PSF stamp, of the size of a single input stamp.
    %          - The combined variance stamp ([] if 'Var' was not given).
    % Author : A.M. Krassilchtchikov (Aug 2026)
    % Example: P1 = imUtil.kernel2.gauss([2 2 0],[15 15]);
    %          P2 = imUtil.kernel2.gauss([3 3 0],[15 15]);
    %          P  = imUtil.psf.combinePSF({P1, P2}, 'Weights',[1 3]);
    %          [P, V] = imUtil.psf.combinePSF(cat(3,P1,P2), 'Var',cat(3,0.1.*P1,0.1.*P2));
    arguments
        PSF
        Args.Weights            = [];
        Args.Var                = [];
        Args.Norm logical       = true;
        Args.ReNormMethod       = 'int';   % 'int' | 'rms'
    end

    % bring both the stamps and the variances to a cube whose last dimension
    % is the PSF index
    [Cube, Npsf, DimPsf] = stamps2cube(PSF, 'PSF');
    if isempty(Args.Var)
        VarCube = [];
    else
        [VarCube, NpsfVar, DimVar] = stamps2cube(Args.Var, 'Var');
        if NpsfVar~=Npsf || DimVar~=DimPsf || ~isequal(size(VarCube), size(Cube))
            error('The variance stamps do not match the PSF stamps in number or size');
        end
    end

    % normalize the weights to a unit sum
    if isempty(Args.Weights)
        W = ones(Npsf,1);
    else
        W = Args.Weights(:);
        if numel(W)~=Npsf
            error('The number of weights (%d) does not match the number of PSF stamps (%d)', numel(W), Npsf);
        end
    end
    if ~all(isfinite(W)) || any(W<0) || sum(W)<=0
        error('The weights must be finite, non-negative and not all zero');
    end
    W = W./sum(W);
    W = reshape(W, [ones(1,DimPsf-1), Npsf]);

    % normalize the individual stamps
    if Args.Norm
        NormIn = normFactor(Cube, Args.ReNormMethod);
        Cube   = Cube./NormIn;
        if ~isempty(VarCube)
            VarCube = VarCube./NormIn.^2;
        end
    end

    % the weighted mean and the variance of the weighted mean
    CombPSF = sum(Cube.*W, DimPsf);
    if isempty(VarCube)
        CombVar = [];
    else
        CombVar = sum(VarCube.*W.^2, DimPsf);
    end

    % renormalize the result (a no-op for 'int', where the input stamps are
    % already normalized and the weights sum up to 1)
    if Args.Norm
        NormOut = normFactor(CombPSF, Args.ReNormMethod);
        CombPSF = CombPSF./NormOut;
        if ~isempty(CombVar)
            CombVar = CombVar./NormOut.^2;
        end
    end
end

function [Cube, Npsf, DimPsf] = stamps2cube(Stamps, Name)
    % Build a cube whose last dimension is the stamp index
    % Input  : - A cell array of stamps or a numeric [Ny, Nx, Npsf] array.
    %          - The name of the input, used in the error messages.
    % Output : - The cube of stamps.
    %          - The number of stamps.
    %          - The index of the cube dimension holding the stamp index.
    if iscell(Stamps)
        Npsf = numel(Stamps);
        if Npsf==0
            error('The %s input is empty', Name);
        end
        SizeStamp = size(Stamps{1});
        if ~all(cellfun(@(X) isequal(size(X), SizeStamp), Stamps(:)))
            error('All the %s stamps must be of the same size', Name);
        end
        DimPsf = numel(SizeStamp)+1;
        Cube   = cat(DimPsf, Stamps{:});
    else
        if ndims(Stamps)>3
            error('A numeric %s input must be a [Ny, Nx, Npsf] array - use a cell array for stamps with extra dimensions', Name);
        end
        Cube   = Stamps;
        DimPsf = 3;
        Npsf   = size(Cube,3);
    end
end

function Factor = normFactor(PSF, Method)
    % The normalization factor of a PSF stamp, as used by imUtil.psf.normPSF
    % Input  : - A PSF stamp or a cube of stamps.
    %          - 'int' (sum of the pixel values) or 'rms'.
    % Output : - The factor, with the two pixel dimensions reduced to 1.
    switch lower(Method)
        case 'int'
            Factor = sum(PSF,[1 2]);
        case 'rms'
            Factor = rms(PSF,[1 2]);
        otherwise
            error('Requested renormalization method is not known');
    end
end

function [D1, D2] = stampCenter(Stamps, Args)
    % The light center of PSF stamps, as an offset from the stamp's geometric center
    %   For each stamp, the offset of its light center from the point (N+1)/2 along each array
    %   dimension: the point that the injection functions (imUtil.art.addSources,
    %   imUtil.art.createSourceCube) place at the source position. A source injected at X - D1,
    %   Y - D2 (X along the first dimension) has its light center at X, Y.
    %   'centroid' is the first moment of the whole stamp. 'core' is the first moment within a
    %   Gaussian window of sigma Window, moved to the result and repeated until it converges,
    %   starting at the stamp center: the center of the core, little affected by the wings.
    % Input  : - A 2D stamp, a [N1 N2 Nstamp] cube of stamps, or a cell array of 2D stamps
    %            (of any sizes).
    %          * ...,key,val,...
    %            'Method' - 'centroid' (first moment) or 'core' (windowed first moment).
    %                   Default is 'centroid'.
    %            'Window' - Sigma of the 'core' window [detector pixels]. Default is 1.5.
    %            'Oversample' - Stamp samples per detector pixel. Default is 1.
    %            'MaxIter' - Maximum number of 'core' iterations. Default is 50.
    %            'Tol' - 'core' convergence: the largest change of an offset in the last
    %                   iteration [detector pixels]. Default is 1e-6.
    % Output : - [Nstamp 1] offsets along the first array dimension [detector pixels].
    %            NaN for a stamp with no flux.
    %          - [Nstamp 1] offsets along the second array dimension [detector pixels].
    % Author : Dana Kovaleva (Nov 2026)
    % Example: [D1, D2] = imUtil.psf.stampCenter(rand(15,15,3));
    %          [D1, D2] = imUtil.psf.stampCenter(PSFCube, 'Method', 'core', 'Oversample', 5);

    arguments
        Stamps
        Args.Method {mustBeMember(Args.Method, {'centroid', 'core'})} = 'centroid';
        Args.Window (1,1) double {mustBePositive} = 1.5;
        Args.Oversample (1,1) double {mustBePositive} = 1;
        Args.MaxIter (1,1) double {mustBePositive} = 50;
        Args.Tol (1,1) double {mustBePositive} = 1e-6;
    end

    if iscell(Stamps)
        D1 = NaN(numel(Stamps), 1);
        D2 = NaN(numel(Stamps), 1);
        for Is = 1:numel(Stamps)
            [D1(Is), D2(Is)] = cubeCenter(double(Stamps{Is}), Args);
        end
    else
        [D1, D2] = cubeCenter(double(Stamps), Args);
    end
end

function [D1, D2] = cubeCenter(Cube, Args)
    % the offsets [detector pixels] of the stamps of one cube
    [N1, N2, ~] = size(Cube);
    I1 = (1:N1).' - (N1 + 1) / 2;   % sample offsets from the geometric center
    I2 = (1:N2)   - (N2 + 1) / 2;
    if strcmp(Args.Method, 'centroid')
        [C1, C2] = moment1(Cube, 1, I1, I2);
    else
        Sigma = Args.Window * Args.Oversample;   % [samples]
        C1 = zeros(1, 1, size(Cube, 3));
        C2 = zeros(1, 1, size(Cube, 3));
        Change = Inf;
        Iter   = 0;
        while Iter < Args.MaxIter && Change > Args.Tol * Args.Oversample
            W = exp(-((I1 - C1).^2 + (I2 - C2).^2) / (2 * Sigma^2));
            [New1, New2] = moment1(Cube, W, I1, I2);
            Change = max(abs([New1(:) - C1(:); New2(:) - C2(:)]), [], 'omitnan');
            C1 = New1;
            C2 = New2;
            Iter = Iter + 1;
        end
    end
    D1 = reshape(C1, [], 1) / Args.Oversample;
    D2 = reshape(C2, [], 1) / Args.Oversample;
end

function [C1, C2] = moment1(Cube, W, I1, I2)
    % the weighted first moments [samples] of each stamp, [1 1 Nstamp]; NaN with no flux
    S  = sum(W .* Cube, [1 2]);
    S(S == 0) = NaN;
    C1 = sum(W .* Cube .* I1, [1 2]) ./ S;
    C2 = sum(W .* Cube .* I2, [1 2]) ./ S;
end

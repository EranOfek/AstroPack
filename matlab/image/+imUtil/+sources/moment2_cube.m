function [X2, Y2, XY] = moment2_cube(Cube, Back, X1, Y1, Args)
    % 2nd moment on a cube of images using various algorithms
    %     Calculate the 2nd central moment for soiurces located in a cube
    %     of images.
    % Input  : - A cube of images, where the image index is in the 3rd dim.
    %          - A vector of background per image slice.
    %            If empty, then estimate backgrond using
    %            imUtil.sources.mex.annulus_median
    %            Default is 0.
    %          * ...,key,val,... 
    %            'Method'  - Method function to use:
    %                   'mex' - use imUtil.sources.mex.moment2_cube
    %                   'legacy' - use imUtul.image.moment2
    %                   Default is 'mex'.
    %            'MaxRad' - Use pixels within this max radius.
    %                   If [], then use all pixels.
    % Output : - Vector of 1st X central moment per slice.
    %          - Vector of 1st Y central moment per slice.
    %          - Iteration at which the estimation converged.
    % Author : Eran Ofek (2026 Feb) 
    % Example: [X2,Y2,XY]=imUtil.sources.moment1_cube(Cube,[]);

    arguments
        Cube
        Back                   = 0;
        Args.Method            = 'mex'; %'mex'|'moment1_cibe_mle'|'legacy'
        Args.SN                = 10;
        Args.MaxIter           = 10;
        Args.SigmaWidth        = [4 2];
        Args.TruncateSigma     = 3;
        Args.RelToCenter       = true;
        Args.MaxStepSize       = [0.2 0.1];
        Args.Annulus           = [10 12];
    end

    if isempty(Args.Back)
        [~,Back] = imUtil.sources.mex.annulus_median(Cube, Args.Annulus);
    end

    switch Args.Method
        case 'mex'
            if isscalar(Back)
                [Ni, Nj, Nk] = size(Cube);
                Back         = repmat(Back, Nk, 1);
            end
            if isempty(Args.MaxRad)
                Args.Maxrad = NaN;
            end
            [X1,Y2,XY] = imUtil.sources.mex.moment1_cube(Cube, Back, X1, Y1, Args.MaxRad);

        case 'legeacy'
            % legacy is not correct for moment2
            [Ni, Nj, Nk] = size(Cube);
            Cube = Cube - repmat(Back, [1 1 Nk]);
            [~,M2] = imUtil.image.moment2(Cube, [], [], 'SubBack',false);

            X2  = M2.X2;
            Y2  = M2.Y2;
            XY  = M2.XY;
        otherwise
            error('Unknown Method option');
    end
end

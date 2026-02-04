function [X1, Y1, Con] = moment1_cube(Cube, Back, Args)
    % 1st moment on a cube of images using various algorithms
    %     Calculate the 1st central moment for soiurces located in a cube
    %     of images.
    % Input  : - A cube of images, where the image index is in the 3rd dim.
    %          - A vector of background per image slice.
    %            If empty, then estimate backgrond using
    %            imUtil.sources.mex.annulus_median
    %            Default is 0.
    %          * ...,key,val,... 
    %            'Method'  - Method function to use:
    %                   'mex' - use imUtil.sources.mex.moment1_cube
    %                   'moment1_cibe_mle' - use imUtil.sources.moment1_cibe_mle
    %                   'legacy' - use imUtul.image.moment2
    %                   Default is 'mex'.
    %            'SN' - A vector of S/N per image slice.
    %                   Used by: 'mex' option.
    %                   This will be used for the convergence. The calculation is
    %                   converged when the shift between two iterations is smaller than
    %                   SigmaWidth/SN.
    %            'MaxIter' - Max. number of iterations. Default is 10.
    %            'SigmaWidth' - sigma-width of the Gaussian weight function.
    %                   If two element vector, then the first is used only in the 1st
    %                   iteration, and the 2nd for all the other iterations.
    %                   Default is [4 2].
    %            'TruncateSigma' - When calculate weights, truncate pixels
    %                   outside (+/- K*SigmaWidth). This may speed up the code.
    %                   Used by: 'mex' option.
    %                   Default is 3.
    %            'RelToCenter' - If true, then the output X and Y are relative to
    %                   the image slice center. If false, then relative to corner.
    %                   Default is true.
    %            'MaxStepSize' - Maximum step size in X and Y between uterations.
    %                   For 'mex' option two elements are required.
    %                   The first will be used in the 1st iteration, and
    %                   the second in the rest.
    %                   Default is [0.2 0.1].
    %            'Annulus' - [Inner, Outer] radii [pix] from which to
    %                   estimate background (if []). Default is [10 12].
    % Output : - Vector of 1st X central moment per slice.
    %          - Vector of 1st Y central moment per slice.
    %          - Iteration at which the estimation converged.
    % Author : Eran Ofek (2026 Feb) 
    % Example: [X1,Y1,Con]=imUtil.sources.moment1_cube(Cube,[]);

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
            if scalar(Args.SN)
                [Ni, Nj, Nk] = size(Cube);
                Args.SN      = repmat(Args.SN, Nk, 1);
            end
            [X1,Y1,Con] = imUtil.sources.mex.moment1_cube(Cube, Back, Args.SN, Args.MaxIter, Args.SigmaWidth, Args.TruncateSigma, Args.RelToCenter, Args.MaxStepSize(2), Args.MaxStepSize(1));

        % case 'moment1_cube_mle'
        %     Con = []
        %     MaxRadius = Args.SigmaWidth(end).*Args.TruncateSigma;
        %     [X1,Y1] = wcentroid_cube_mle(Cube, Back, 'SigmaWidth',Args.SigmaWidth, 'MaxRadius',MaxRadius, 'MaxIter',Args.MaxIter,'RelToCenter',Args.RelToCenter);
        % 

        case 'legeacy'
            [Ni, Nj, Nk] = size(Cube);
            Cube = Cube - repmat(Back, [1 1 Nk]);
            [M1] = imUtil.image.moment2(Cube, [], [], 'SubBack',false, 'MaxIter',Args.MaxIter, 'MaxStep',Args.MaxStepSize(end));

            X1  = M1.X;
            Y1  = M1.Y;
            Con = M1.Iter;
        otherwise
            error('Unknown Method option');
    end
end

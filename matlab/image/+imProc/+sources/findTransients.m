function findTransients(New, Ref, D, S, Pd, Z2, Args)
    %
    % Example: imProc.sources.findTransients(AIreg(2), S, [], Z2)

    arguments
        New AstroImage
        Ref AstroImage
        D                          = [];
        S                          = [];
        Pd                         = [];
        Z2                         = [];
        Args.NormS logical         = true;

        Args.Threshold             = 5;
        Args.findLocalMaxArgs cell = {};
        Args.BitCutHalfSize        = 3;
        % Args.CutHalfSize           = 12; must be like PSF size
        Args.psfPhotCubeArgs cell  = {};
    end

    
    Nobj = numel(New);
    for Iobj=1:1:Nobj
        % for each image

        % filter D with Pd
        if isempty(S)
            ImS = imUtil.filter.filter2_fast(D(Iobj).Image, D(Iobj).PSF);
        else
            ImS = S(Iobj).Image;
        end

        % normalize S 
        if Args.NormS
            ImS = ImS - median(ImS,'all','omitnan');
            ImS = ImS./tools.math.stat.rstd(ImS,'all');
        end

        % find positive and negative sources in S
        [PosLocalMax] = imUtil.sources.findLocalMax(ImS, 'Variance',1, 'Threshold',Args.Threshold, Args.findLocalMaxArgs{:});
        [NegLocalMax] = imUtil.sources.findLocalMax(-ImS, 'Variance',1, 'Threshold',Args.Threshold, Args.findLocalMaxArgs{:});
        % Output *LocalMax contains: [X,Y,SN,ImageIndex,LinaerIndexIn2D]
        % Merge pos/neg lists and add sign to the SN column
        NegLocalMax(:,3) = -NegLocalMax(:,3);
        LocalMax = [PosLocalMax; NegLocalMax];
        Nsrc     = size(LocalMax,1);

        % get Mask values within cutouts around pos/neg transients candidates
        
        MaskVal = New(Iobj).MaskData.bitwise_cutouts(LocalMax(:,1:2), 'or', 'HalfSize',Args.BitCutHalfSize);

        % PSF fit all candidates in the D image
        CutHalfSize = (size(D(Iobj).PSF,1)-1).*0.5;
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(D(Iobj).Image, LocalMax(:,1), LocalMax(:,2), CutHalfSize);
        % Change the sign of negative sources
        Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
        [ResultD, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF', D(Iobj).PSF, Args.psfPhotCubeArgs{:});
    
        Chi2dof = ResultD.Chi2./ResultD.Dof;
        Flag = Chi2dof>0.5 & Chi2dof<2;

        % PSF fit all candidates in the New image
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(New(Iobj).Image, LocalMax(:,1), LocalMax(:,2), CutHalfSize);
        [ResultN, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF', D(Iobj).PSF, Args.psfPhotCubeArgs{:});
    
        % PSF fit all candidates in the Ref image
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Ref(Iobj).Image, LocalMax(:,1), LocalMax(:,2), CutHalfSize);
        [ResultR, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF', D(Iobj).PSF, Args.psfPhotCubeArgs{:});
    

        %Chi2dof = ResultN.Chi2./ResultN.Dof;
        Flag = Chi2dof>0.5 & Chi2dof<2 & ResultD.SNm>Args.Threshold & ResultD.SNm./ResultN.SNm>1.1;


        

    end

end
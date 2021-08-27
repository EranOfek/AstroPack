function cutouts_photometry(Input, Args)
    %
    % Examples: AI(1) = wfast.read2AstroImage('WFAST_Balor_20200801-020634-879_F505W_0_CutoutsStack.h5z','ReadType','cutouts','calibrate',false,'InterpOverNan',false);
    %           AI(2) = wfast.read2AstroImage('WFAST_Balor_20200801-020630-880_F505W_0_CutoutsStack.h5z','ReadType','cutouts','calibrate',false,'InterpOverNan',false);
    %           
    %           wfast.cutouts_photometry(AI)
    
    
    
    arguments
        Input                                    % AstroImage or file name
        Args.read2AstroImageArgs cell          = {'Calibrate',true,'InterpOverNan',false};
        Args.AperRadius(1,:)                   = [2, 3, 4];
        Args.Annulus(1,2)                      = [4 6];
        Args.SubBack(1,1) logical              = true;
        Args.MinFluxForCentering               = 100;
    end
    
    if ~isa(Input, 'AstroImage')
        % read FileName into an AstroImage object
        [AI, CalibObj] = read2AstroImage(Input, Args.read2AstroImageArgs{:},'ReadType','cutouts');
    else
        AI = Input;
    end
    
    Nobj = numel(AI);
    for Iobj=1:1:Nobj
        SizeC = size(AI(Iobj).Image);
        Ncut  = prod(SizeC(3:end));
        Cube = reshape(AI(Iobj).Image, SizeC(1), SizeC(2), Ncut);
        X    = ceil(SizeC(2).*0.5).*ones(Ncut,1);
        Y    = ceil(SizeC(1).*0.5).*ones(Ncut,1);
        Cube = single(Cube);
        
        % with 1st moment estimation (Centered)
        [M1C(Iobj),M2C(Iobj),AperC(Iobj)] = imUtil.image.moment2(Cube, X, Y, 'NoWeightFirstIter',false,...
                                                                             'AperRadius',Args.AperRadius,...
                                                                             'Annulus',Args.Annulus,...
                                                                             'SubBack',Args.SubBack);
    
        % without 1st moment estimation (Forced)
        [M1F(Iobj),M2F(Iobj),AperF(Iobj)] = imUtil.image.moment2(Cube, X, Y, 'NoWeightFirstIter',false,'MaxIter',-1,...
                                                                             'AperRadius',Args.AperRadius,...
                                                                             'Annulus',Args.Annulus,...
                                                                             'SubBack',Args.SubBack);
    
        
        %
        M1C(Iobj)   = tools.struct.reshapeFields(M1C(Iobj), SizeC(3:end), 'first');
        M1F(Iobj)   = tools.struct.reshapeFields(M1F(Iobj), SizeC(3:end), 'first');
        M2C(Iobj)   = tools.struct.reshapeFields(M2C(Iobj), SizeC(3:end), 'first');
        M2F(Iobj)   = tools.struct.reshapeFields(M2F(Iobj), SizeC(3:end), 'first');
        AperC(Iobj) = tools.struct.reshapeFields(AperC(Iobj), SizeC(3:end), 'first');
        AperF(Iobj) = tools.struct.reshapeFields(AperF(Iobj), SizeC(3:end), 'first');
        
        % select stars which with reasnoable flux
        Flag = mean(AperC.AperPhot(:,:,2),1)>Args.MinFluxForCentering;
        MeanX = mean(M1C.X(:,Flag),2,'omitnan');
        MeanY = mean(M1C.Y(:,Flag),2,'omitnan');
        FX    = repmat(MeanX, SizeC(4), 1);
        FY    = repmat(MeanY, SizeC(4), 1);
        
        % without 1st moment estimation (Forced on mean position)
        % This is the probably the best estimator
        [M1FM(Iobj),M2FM(Iobj),AperFM(Iobj)] = imUtil.image.moment2(Cube, FX, FY, 'NoWeightFirstIter',false,'MaxIter',-1,...
                                                                             'AperRadius',Args.AperRadius,...
                                                                             'Annulus',Args.Annulus,...
                                                                             'SubBack',Args.SubBack);
    
        
        M1FM(Iobj)   = tools.struct.reshapeFields(M1F(Iobj), SizeC(3:end), 'first');
        M2FM(Iobj)   = tools.struct.reshapeFields(M2F(Iobj), SizeC(3:end), 'first');
        AperFM(Iobj) = tools.struct.reshapeFields(AperF(Iobj), SizeC(3:end), 'first');
        
        SumFlux = sum(AperFM(Iobj).AperPhot(:,Flag,2), 2)
        CorrZP  = SumFlux./mean(SumFlux)
        
        AperFM(Iobj).AperPhot = AperFM(Iobj).AperPhot .* CorrZP;
        
        'got here'
        
        
        SN = squeeze(AperC.AperPhot(:,:,2))./ sqrt( squeeze(AperC.AperPhot(:,:,2)) + AperC.AnnulusStd.^2  );
        mean(SN,2,'omitnan')
        MeanX = mean(M1C.X,2,'omitnan')
        
        
        %plot(AperC(Iobj).AperPhot(:,15,2))
    end
    
    % Merge LCs from all objects
    M1C   = tools.struct.mergeStructArray(M1C);
    M1F   = tools.struct.mergeStructArray(M1F);
    M2C   = tools.struct.mergeStructArray(M2C);
    M2F   = tools.struct.mergeStructArray(M2F);
    AperC = tools.struct.mergeStructArray(AperC);
    AperF = tools.struct.mergeStructArray(AperF);
    
    plot(AperC.AperPhot(:,15,2))
    plot(M1C.X(:,15))
    
    MeanX = mean(M1C.X(:,15));
    MeanY = mean(M1C.Y(:,15));
    
    plot(sqrt( (M1C.X(:,15)-MeanX).^2 + (M1C.Y(:,15)-MeanY).^2) )
    
    
    'a'
    
    
    
end
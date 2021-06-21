function [Result, Surface] = fitSurface(Obj, Args)
    % Fit a surface to a 2D image, with sigma clipping
    % Input  : - An AstroImage or ImageComponent object.
    %          * ...,key,val,...
    %            'DataProp' - data property on which to operate.
    %                   Default is 'Image'.
    %            'StepXY' - Steps on values to fit. Default is 1.
    %            'Fun' - A cell array of 2D functionals to fit:
    %                   Default is { @(x,y)ones(size(x)), @(x,y)x, @(x,y)y, @(x,y)2.*x.^2-1, @(x,y)2.*y.^2-1, @(x,y)x.*y }
    %            'Niter' - Number of iterations. Use >1 for sigma clipping.
    %                   Default is 2.
    %            'SigmaClip' - SIgma clipping value. Default is 3.
    % Output : - A structure array of the results including
    %            the functionals (Fun), the fitted parameters (Par), and
    %            the RMS.
    %          - An ImageComponent of surface values.
    % Author : Eran Ofek (Jun 2021)
    % Example: NOT TESTED
    
    arguments
        Obj                                 % AstroImage or ImageComponent
        Args.DataProp         = 'Image';
        Args.StepXY           = 1;
        Args.Fun cell         = { @(x,y)ones(size(x)), @(x,y)x, @(x,y)y, @(x,y)2.*x.^2-1, @(x,y)2.*y.^2-1, @(x,y)x.*y };
        Args.Niter            = 2;
        Args.SigmaClip        = 3;
    end
   
    Nobj = numel(Obj);
    if nargout>1
        Surface = ImageComponent(size(Obj));
    end
    
    for Iobj=1:1:Nobj
        % for each image
        
        Image = Obj(Iobj).(DataProp);
        
        % fit
        SizeIJ       = size(Image);
        [MatX, MatY] = meshgrid( (1:Args.StepXY:SizeIJ(2)), (1:Args.StepXY:SizeIJ(1)) );
        XX           = MatX(:);
        YY           = MatY(:);
        Ind          = imUtil.image.sub2ind_fast(SizeIJ, YY, XX);
        
        % normalizing X to -1 to 1
        MinX   = 1;
        MaxX   = SizeIJ(2);
        MidX   = 0.5.*(MinX + MaxX);
        RangeX = (MaxX - MinX);
        XN     = 2.*(XX - MidX)./RangeX;
        
        % normalizing Y to -1 to 1
        MinY   = 1;
        MaxY   = SizeIJ(1);
        MidY   = 0.5.*(MinY + MaxY);
        RangeY = (MaxY - MinY);
        YN     = 2.*(YY - MidY)./RangeY;
        
        
        ZZ = Image(Ind);
        N  = numel(ZZ);
        
        % The design matrix
        Nf = numel(Args.Fun);
        H  = zeros(N, Nf);
        for If=1:1:Nf
            H(:,If) = Args.Fun{If}(XN, YN);
        end
       
        Par   = H\ZZ;
        Resid = ZZ - H*Par;
        RMS   = std(Resid);
        
        % sigma clipping
        for Iiter=2:1:Args.Niter
            Flag = abs(Resid./RMS)<=Args.SigmaClip;
           
            H  = H(Flag,:);
            ZZ = ZZ(Flag);
            N  = numel(ZZ);
            
            Par   = H\ZZ;
            Resid = ZZ - H*Par;
            RMS   = std(Resid);
        
        end
           
        Result(Iobj).FunX = FunX;
        Result(Iobj).Par  = Par;
        Result(Iobj).RMS  = RMS;
        
        if nargout>1
            SizeIJ       = size(Image);
            [MatX, MatY] = meshgrid( (1:Args.StepXY:SizeIJ(2)), (1:Args.StepXY:SizeIJ(1)) );
            MatXN        = 2.*(MatX - MidX)./RangeX;
            MatYN        = 2.*(MatY - MidY)./RangeY;
            
            Surface(Iobj).Image = zeros(SizeIJ);
            for If=1:1:Nf
                Surface(Iobj).Image = Surface(Iobj).Image + Args.Fun(MatXN, MatYN);
            end
        end
        
    end
    
    
end
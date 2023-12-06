function Image=createLine(Image, Args)
    % Add lines with top-hat or gaussian profile to image.
    % Input  : - A 2-D image, or a [SizeI, SizeJ] vector of an image to create.
    %          * ...,key,val,...
    %            'A' - Vector of lines slope to add to the image.
    %            'B' - Vector of lines intersection to add to the image.
    %            'Fun' - Line profile function:
    %                   'flat' - A top hat function with width given by
    %                       2.*Sigma.git s
    %                   'gauss' - A Gaussian profile width sigma-width
    %                       given by Sigma.
    %                   Default is 'gauss'.
    %            'Sigma' - Function profile width.
    %                   Default is 2.
    %            'Norm'  - Function profile normalization.
    %                   Default is 1.
    %            'PixClass' - If 1st input argument is the image size, then
    %                   this will specify the class of the output image.
    %                   Default is 'single'.
    % Output : - A 2-D matrix image with lines added.
    % Author : Eran Ofek (Jul 2023)
    % Example: Image=imUtil.frt.createLine([100 200],'A',1.1,'B',30);
    %          Image=imUtil.frt.createLine([100 200],'A',[1.1;-0.3],'B',[30,20],'Fun','flat');
    
    arguments
        Image     % Image or [Y, X] size
        Args.A    % Line Slope: Y=A*X + B
        Args.B    % Line intersection: Y=A*X + B
        Args.Fun        = 'gauss';  % % 'gauss'
        Args.Sigma      = 2;
        Args.Norm       = 1;
        
        Args.PixClass   = 'single';
    end
        
    if numel(Image)==2
        % create a blank image
        Image  = zeros(Image, Args.PixClass);
        SizeIJ = size(Image);
    end
    
    VecX = (1:1:SizeIJ(2));
    VecY = (1:1:SizeIJ(1)).';
    %[MatX, MatY] = meshgrid((1:1:SizeIJ(2)), (1:1:SizeIJ(1)));
    
    X = VecX.';
    Y = Args.A(:).' .*X + Args.B(:).';
    
    Nline = numel(Args.A);
    Bt = ones(Nline,1);
    At = -Args.A(:).';
    Ct = -Args.B(:).';
    
    
    
    switch lower(Args.Fun)
        case 'gauss'
            Sigma = Args.Sigma;
            for Iline=1:1:Nline
                %Dist  = abs(At(Iline).*MatX + Bt(Iline).*MatY + Ct(Iline))./sqrt(At(Iline).^2 + Bt(Iline).^2);
                Dist  = abs(At(Iline).*VecX + Bt(Iline).*VecY + Ct(Iline))./sqrt(At(Iline).^2 + Bt(Iline).^2);
                Image = Image + Args.Norm.*exp(-0.5.*(Dist./Sigma).^2)./(Sigma.*sqrt(2.*pi));
            end
        case 'flat'
            HalfWidth = Args.Sigma;
            for Iline=1:1:Nline
                %Dist  = abs(At(Iline).*MatX + Bt(Iline).*MatY + Ct(Iline))./sqrt(At(Iline).^2 + Bt(Iline).^2);
                Dist  = abs(At(Iline).*VecX + Bt(Iline).*VecY + Ct(Iline))./sqrt(At(Iline).^2 + Bt(Iline).^2);
                Image = Image + Args.Norm.*(Dist<HalfWidth);
            end
            
        otherwise
            error('Unknown Fun option');
    end
    
    
end

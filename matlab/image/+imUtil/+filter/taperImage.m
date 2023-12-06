function Result=taperImage(Image, XY, Args)
    % Multiply a single image with a taper functions, centered at different positions.
    %   Given an image an a taper (window) function (default is Hann), and
    %   a set of [X,Y] position specifying the taper function centers,
    %   return the multiplication of the image with each taper function.
    %   Optionally, return the fft of the result.
    % Input  : - A 2-D matrix.
    %          - A two column matrix [X, Y] of positions for the center of
    %            the taper function.
    %          * ...,key,val,...
    %            'Kernel' - Taper/window function of the form:
    %                   Kernel=Fun(Pars, SizeXY, PosXY, Norm).
    %                   Default is @imUtil.kernel2.hann
    %            'KernelPars' - Vector of parameters to pass to the Kernel
    %                   function as th 1st input argument.
    %                   Default is 1 (for Hann function, this is
    %                   meaningless).
    %            'FFT' - A logical indicating if to return the fft of the
    %                   image multiplied by the taper.
    %                   Default is false.
    %            'OutType' - 'struct'|'cube'.
    %                   If 'struct', the this is a structure array
    %                   containing .X, .Y, .Image fields.
    %                   Default is 'cube'.
    % Output : - Either a structure array, or a cube of images (depanding
    %            on OutType).
    % Author : Eran Ofek (Jun 2023)
    % Example: Image=randn(2000,2000);
    %          Result=imUtil.filter.taperImage(Image, XY)
    
    arguments
        Image
        XY
        Args.Kernel        = @imUtil.kernel2.hann;
        Args.KernelPars    = 1;
        Args.FFT logical   = false;
        Args.OutType       = 'cube';   % 'struct'|'cube'
    end
    
    SizeXY  = fliplr(size(Image));
    Nxy = size(XY,1);
    Args.KernelPars = repmat(Args.KernelPars, Nxy, 1);
    
    switch lower(Args.OutType)
        case 'struct'
            for Inxy=1:1:Nxy
                % call the kernel and multiply with the image
                Result(Inxy).Image = Image.*Args.Kernel(Args.KernelPars, SizeXY, XY(Inxy,:));
                Result(Inxy).X      = XY(Inxy,1);
                Result(Inxy).Y      = XY(Inxy,2);
                if Args.FFT
                    Result(Inxy).Image = fft2(Result(Inxy).Image);
                end
            end
        case 'cube'
            Result = zeros(SizeXY(2), SizeXY(1), Nxy);
            for Inxy=1:1:Nxy
                Result(:,:,Inxy) = Image.*Args.Kernel(Args.KernelPars, SizeXY, XY(Inxy,:));
            end
            
            if Args.FFT
                Result = fft2(Result);
            end
        otherwise
            error('Unknown OutType option');
    end
    
end
   
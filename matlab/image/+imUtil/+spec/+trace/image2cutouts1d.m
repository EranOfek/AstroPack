function Result=image2cutouts1d(Array, Pos, Args)
    % Cut line-stamps from an array, around some central position.
    %   Given an array and positions along one of its dimensions.
    %   Around each position cut a line with half size given by the
    %   WinHalfSize argument and return a matrix in which the columns are
    %   the line-stamps centered around each position.
    % Input  : - An array.
    %          - Vector (or scalar) of positions. If scalar, will choose a
    %            line stamp from each column (or row, for Dim=2).
    %          * ...,key,val,...
    %            'Dim' - Dimension along to cut the lines. Default is 1.
    %            'WinHalfSize' - line stamp window half size. The window
    %                   size will be 2.*WinHlafSize + 1.
    %                   Default is 7.
    % Output : - A matrix in which the columns (regardless of Dim) are the
    %            cutout stamps.
    % Author : Eran Ofek (May 2023)
    % Example: R = rand(100,100); 
    %          Result=imUtil.trace.image2cutouts1d(R, 5)
    
    arguments
        Array
        Pos
        Args.Dim           = 1;
        Args.WinHalfSize   = 7;
    end
    
    if Args.Dim==2
        Array = Array.';
    end
    
    RoundedPos = round(Pos);
    [Nspat, Nwave] = size(Array);
    
    if any(Pos<1) || any(Pos>Nspat)
        error('All Pos elements must be within array size');
    end
    
    if numel(Pos)==1
        
        I1 = Pos - Args.WinHalfSize;
        I2 = Pos + Args.WinHalfSize;
        
        if I1<1 
            PadLow  = zeros(abs(I1)+1, Nwave);
            PadHigh = zeros(0, Nwave);
            I1 = 1;
        elseif I2>Nspat
            PadLow  = zeros(0, Nwave);
            PadHigh = zeros(I2-Nspat, Nwave);
            I2 = Nspat;
        else
            PadLow  = zeros(0, Nwave);
            PadHigh = zeros(0, Nwave);
        end
            
        Result = [PadLow; Array(I1:I2,:); PadHigh];
    else
        if numel(Pos)~=Nwave
            error('Number of Pos elemenst must be equal to the number of wavelength');
        end
        
        Result = zeros(Args.WinHalfSize.*2 + 1, Nwave);
        
        for Iwave=1:1:Nwave
            I1 = Pos(Iwave) - Args.WinHalfSize;
            I2 = Pos(Iwave) + Args.WinHalfSize;

            if I1<1
                PadLow  = zeros(abs(I1)+1, 1);
                PadHigh = zeros(0, 1);
                I1      = 1;
            elseif I2>Nspat
                PadLow  = zeros(0, 1);
                PadHigh = zeros(I2-Nspat, 1);
                I2 = Nspat;
            else
                PadLow  = zeros(0, 1);
                PadHigh = zeros(0, 1);
            end
            
            Result(:,Iwave) = [PadLow; Array(I1:I2,Iwave); PadHigh];
        end
        
    end
    
    
end

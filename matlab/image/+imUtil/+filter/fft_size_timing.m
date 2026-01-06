function [Result] = fft_size_timing(SizeToCheck, IsDouble, Nsim)
    % Test the speed of 2D fft as a function of image size.
    %   For each image size return the fft2 run time.
    % Input  : - A two column matrix of image size to test [I,J].
    %          - True for double, and false for single.
    %          - Number of simulations per image size.
    %            Default is 200.
    % Output : - A vectot of run times per image size.
    % Author : Eran Ofek (2026 Jan) 
    % Example: RR=imUtil.filter.fft_size_timing([Size Size]);

    arguments
        SizeToCheck
        IsDouble       = false;
        Nsim           = 200;
    end

    Ntest = size(SizeToCheck,1);
    Result = nan(Ntest,1);
    for Itest=1:1:Ntest
        R = rand(SizeToCheck(Itest,1:2));
        if ~IsDouble
            R = single(R);
        end
        tic;
        for I=1:1:Nsim
            A = fft2(R);
        end
        Result(Itest) = toc;
    end



end

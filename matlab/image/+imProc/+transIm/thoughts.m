function NewImage = thoughts(Image, Tran, Args)
    %
    % Example: imProc.transIm.thoughts

    arguments
        Image               = [];
        Tran                = [];
        Args.InterpMethod   = 'cubic';
    end


    Image = rand(1200,1200);
    Image = single(Image);

    SizeIm = size(Image);
    VecX   = (1:1:SizeIm(2));
    VecY   = (1:1:SizeIm(1));

    OutMatX   = VecX + VecY.';
    OutMatY   = VecX + VecY.';

    interp2(VecX, VecY, Image, OutMatX, OutMatY, Args.InterpMethod);

end

function Result = unitTest()
    % unitTest for imUtil.psf package   

    %% imUtil.psf.stamp2full

    K=imUtil.kernel2.gauss(2.*ones(100,1));
    F=imUtil.psf.stamp2full(K,[31 32],'CenterPosition','center');
    M=imUtil.image.moment2(F(:,:,2),16,16);
    if abs(M.X-16.5)>1e-4 || abs(M.Y-16)>1e-4
        error('Problem with imUtil.psf.stamp2full');
    end



    %%

	Result = true;
end

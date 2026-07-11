function [Result] = unitTest()
    % unitTest for imUtil.properCoadd


    %%
    J = (1:1:100); D=100; r0=5;
    
    Nim = 100;
    Cube = zeros(256,256,Nim);
    DX1 = 5;
    DY1 = 5;
    F1  = 0.5;
    DX2 = 15;
    DY2 = -15;
    F2  = 0.1;
    for I=1:1:Nim
        [AmpC,J,C]=telescope.Optics.zer_cj_variance(100,'Nrand',1,'D',D,'r0',r0);
        [Image_NS,Image,SumY]=telescope.Optics.zerwavefront2image(J,[],C);

        Cube(:,:,I) = Image;
    end
    %pcolor(log10(Image)), shading interp; axis square, colorbar

    PSF = Cube;  % noisless

    [Cube1]=imUtil.trans.mex.shift_lanczos3(Cube,DX1.*ones(Nim,1),DY1.*ones(Nim,1));
    [Cube2]=imUtil.trans.mex.shift_lanczos3(Cube,DX2.*ones(Nim,1),DY2.*ones(Nim,1));
    Cube = Cube + Cube1.*F1 + Cube2.*F2;

    Cube = 1e4.*Cube + randn(size(Cube));

    % adding sub-Nyquist noise
    CubeC = Cube;  % without outliers
    Cube(3,4,12) = 1e5;
    Cube(100,100,17) = 1e5;
    Cube(130,160,19) = 1e5;
    
    %%

    [R,PR,R_f,PR_f]=imUtil.properCoadd.combine_proper(CubeC, PSF, 'Full2stamp',false);
    surface(fftshift(R))
    colorbar
    shading interp

    %%
    [R1,PR1,R_f,PR_f]=imUtil.properCoadd.combine_proper(Cube, PSF, 'Full2stamp',false);
    R1 = fftshift(R1);
    surface(R1)
    colorbar
    shading interp

    %%
    [R2,P_R1,Info]=imUtil.properCoadd.properCoaddLinear(CubeC, PSF, 'Robust',false);
    surface((R2))
    colorbar
    shading interp


    %%
    [R3,P_R,Info]=imUtil.properCoadd.properCoaddLinear(Cube, PSF, 'Robust',true);
    surface((R3))
    colorbar
    shading interp

    %%



    %%

    Result = true;

end

function [Result] = unitTest()
    % unitTest for imUtil.properCoadd


    %%
    J = (1:1:100); D=100; r0=5;
    
    Nim = 100;
    Cube = zeros(256,256,Nim);
    for I=1:1:Nim
        [AmpC,J,C]=telescope.Optics.zer_cj_variance(100,'Nrand',1,'D',D,'r0',r0);
        [Image_NS,Image,SumY]=telescope.Optics.zerwavefront2image(J,[],C);
        Cube(:,:,I) = Image;
    end
    %pcolor(log10(Image)), shading interp; axis square, colorbar

    PSF = Cube;  % noisless
    Cube = 1e4.*Cube + randn(size(Cube));

    % adding sub-Nyquist noise
    CubeC = Cube;
    Cube(3,4,12) = 1e5;
    Cube(100,100,17) = 1e5;
    Cube(130,160,19) = 1e5;


    [R,PR,R_f,PR_f]=imUtil.properCoadd.combine_proper(Cube, PSF, 'Full2stamp',false);
    surface(fftshift(R))
    colorbar
    shading interp

    %%
    [R1,PR1,R_f,PR_f]=imUtil.properCoadd.combine_proper(CubeC, PSF, 'Full2stamp',false);
    R1 = fftshift(R1);
    surface(R1)
    colorbar
    shading interp

    %%
    [R1,P_R1,Info]=imUtil.properCoadd.properCoaddLinear(CubeC, PSF, 'Robust',false);
    surface((R))
    colorbar
    shading interp


    %%
    [R,P_R,Info]=imUtil.properCoadd.properCoaddLinear(CubeC, PSF, 'Robust',true);
    surface((R))
    colorbar
    shading interp

    %%


    Result = true;

end

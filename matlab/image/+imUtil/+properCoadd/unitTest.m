function [Result] = unitTest()
    % unitTest for imUtil.properCoadd


    %% Generate artificial speckle images
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

    % Art images with 3 sources
    [Cube1]=imUtil.trans.mex.shift_lanczos3(Cube,DX1.*ones(Nim,1),DY1.*ones(Nim,1));
    [Cube2]=imUtil.trans.mex.shift_lanczos3(Cube,DX2.*ones(Nim,1),DY2.*ones(Nim,1));
    Cube = Cube + Cube1.*F1 + Cube2.*F2;

    CubeNN = Cube;
    Cube = 1e3.*Cube + randn(size(Cube));

    % adding sub-Nyquist noise
    CubeC = Cube;  % without outliers
    Outlier = 1e5;
    Cube(3,4,12)     = Outlier;
    Cube(100,100,17) = Outlier;
    Cube(130,160,19) = Outlier;
    Cube(128, 130, 2) = Outlier;
    Cube(129,5, 3)    = Outlier;
    
    %%
    Z1 = -0.0005;
    Z2 = 0.003;
    plot.plotImagesGrid({CubeNN(:,:,1), CubeNN(:,:,2), CubeNN(:,:,3)}, [1 3], 'Z1Z2',[Z1 Z2]);
    colormap(flipud(gray))
    set(gcf, 'Color', 'w');

    print Coadd_Speckle3noiseless.eps -depsc2
    %%

    plot.plotImagesGrid({Cube(:,:,1), Cube(:,:,2), Cube(:,:,3)}, [1 3], 'Z1Z2',[Z1 Z2].*1e3);
    colormap(flipud(gray))
    set(gcf, 'Color', 'w');

    print Coadd_Speckle3withnoise.eps -depsc2

    %%

    Sum = sum(Cube,3);
    surface(Sum)
    colormap(flipud(gray))
    
    colorbar
    shading interp
    H = gca;
    H.ZAxis.Limits=[Z1 Z2].*Nim.*1e2;
    colormap gray
    axis off

    %%

    %[R0,PR,R_f,PR_f]=imUtil.properCoadd.combine_proper(CubeC, PSF, 'Full2stamp',false);
    [R0,PR,R_f,PR_f]=imUtil.properCoadd.properCoaddFFT(CubeC, PSF, 'Full2stamp',false);
    surface(R0)

    colorbar
    shading interp

    H = gca;
    H.ZAxis.Limits=[Z1 Z2].*Nim.*7e2;
    colormap(flipud(gray))

    axis off

    %%
    %[R1,PR1,R_f,PR_f]=imUtil.properCoadd.combine_proper(Cube, PSF, 'Full2stamp',false);
    [R1,PR,R_f,PR_f]=imUtil.properCoadd.properCoaddFFT(Cube, PSF, 'Full2stamp',false);
    surface(R1)
    colorbar
    shading interp
    H = gca;
    H.ZAxis.Limits=[Z1 Z2].*Nim.*1e3;
    colormap(flipud(gray))

    axis off

    %%
    [R2,P_R1,Info]=imUtil.properCoadd.properCoaddLinear(CubeC, PSF, 'Robust',true, 'MaxIter',1,'RobustPar',100);
    surface((R2))
    colorbar
    shading interp
    H = gca;
    H.ZAxis.Limits=[Z1 Z2].*Nim.*1e3;
    colormap(flipud(gray))    
    axis off

    %%
    [R3,P_R,Info]=imUtil.properCoadd.properCoaddLinear(Cube, PSF, 'Robust',true);
    surface((R3))
    colorbar
    shading interp
    H = gca;
    H.ZAxis.Limits=[Z1 Z2].*Nim.*1e3;
    colormap(flipud(gray))    
    axis off

    %% 

    surface((R0-R3))
    colorbar
    shading interp
    H = gca;
    Z1Z2 = [-3 3];
    H.ZAxis.Limits=Z1Z2; %[Z1 Z2].*Nim.*1e3;
    clim(Z1Z2)

    colormap(flipud(gray))    
    axis off
    set(gcf, 'Color', 'w');


    %%
    H=plot.plotImagesGrid({(Sum); (R1); (R0); (R3)}, [2 2], 'Z1Z2',[-5 Z2.*Nim.*1e2]);
    colormap(flipud(gray))    

    Ht=text(H(1),10,10,'(a)'); Ht.Color='k'; Ht.FontSize=20;
    Ht=text(H(2),10,10,'(b)'); Ht.Color='k'; Ht.FontSize=20;
    Ht=text(H(3),10,10,'(c)'); Ht.Color='k'; Ht.FontSize=20;
    Ht=text(H(4),10,10,'(d)'); Ht.Color='k'; Ht.FontSize=20;
    set(gcf, 'Color', 'w');

    print Coadd_Methods.eps -depsc2

    %% The noise in the coadd (clean) images is ~1
    % compare the rstd of the images
    tools.math.stat.rstd(R0(:)-R2(:))
    
    [R2_5,P_R1,Info]=imUtil.properCoadd.properCoaddLinear(CubeC, PSF, 'Robust',true, 'MaxIter',1,'RobustPar',5);
    [R2_50,P_R1,Info]=imUtil.properCoadd.properCoaddLinear(CubeC, PSF, 'Robust',true, 'MaxIter',1,'RobustPar',50);
    [R2_500,P_R1,Info]=imUtil.properCoadd.properCoaddLinear(CubeC, PSF, 'Robust',true, 'MaxIter',1,'RobustPar',500);
    
    tools.math.stat.rstd(R0(:)-R2_5(:))
    tools.math.stat.rstd(R0(:)-R2_50(:))
    tools.math.stat.rstd(R0(:)-R2_500(:))

    %%

    Result = true;

end

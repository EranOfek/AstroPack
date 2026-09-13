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
    tic;
    [R2,P_R1,Info]=imUtil.properCoadd.properCoaddLinear(CubeC, PSF, 'Robust',true, 'MaxIter',1,'RobustPar',100);
    toc

    tic;
    [R2,P_R1,Info]=imUtil.properCoadd.properCoaddLinearR(CubeC, PSF, 'Robust',true, 'MaxIter',1,'RobustPar',100);
    toc

    surface((R2))
    colorbar
    shading interp
    H = gca;
    H.ZAxis.Limits=[Z1 Z2].*Nim.*1e3;
    colormap(flipud(gray))    
    axis off

    %%
    [R3,P_R,Info]=imUtil.properCoadd.properCoaddLinearR(Cube, PSF, 'Robust',true);
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
    
    RP = [4.685, logspace(log10(5), log10(5000), 20)];
    ResStd = zeros(numel(RP),1);
    for Irp=1:1:numel(RP)
        [R2_v,P_R1,Info]=imUtil.properCoadd.properCoaddLinearR(CubeC, PSF, 'Robust',true, 'MaxIter',1,'RobustPar',RP(Irp));
        ResStd(Irp) = tools.math.stat.rstd(R0(:)-R2_v(:));
    end

    %%

    loglog(RP, ResStd, 'ko', 'MarkerFaceColor','k')
    set(gca, 'FontSize',28);
    H=xlabel('Tucky parameter');
    H.FontSize = 34;
    H.Interpreter = 'latex';
    H=ylabel('Robust StD');
    H.FontSize = 34;
    H.Interpreter = 'latex';
    set(gcf, 'Color', 'w');

    print TuckyPar_StD.eps -depsc2


    %%
  
    %% Oracle test: robust rejection vs. perfectly known bad pixels
% Compare:
%   R0      - proper coadd of clean images
%   R1      - non-robust proper coadd containing the outliers
%   R3      - robust proper coadd containing the outliers
%   Roracle - coadd with the exact injected bad pixels masked

Woracle = ones(size(Cube));

% Exact locations of the five injected outliers
Woracle(3,4,12)       = 0;
Woracle(100,100,17)   = 0;
Woracle(130,160,19)   = 0;
Woracle(128,130,2)    = 0;
Woracle(129,5,3)      = 0;

% Keep the mean weight of each affected image equal to 1.
% This keeps the representative scalar weights, and hence the
% prescribed global coadd PSF, equal to those of the robust solution.
Npix   = size(Cube,1).*size(Cube,2);
BadIm  = [12 17 19 2 3];
ScaleW = Npix./(Npix - 1);

for Ibad = 1:numel(BadIm)
    Iim = BadIm(Ibad);

    Tmp = Woracle(:,:,Iim);
    Flag = Tmp > 0;
    Tmp(Flag) = Tmp(Flag).*ScaleW;

    Woracle(:,:,Iim) = Tmp;
end

% Oracle solution.
% Sigma_M = 1 because the Gaussian noise added above has sigma=1.
% Woracle is the fourth positional argument.
[Roracle,P_Roracle,InfoOracle] = ...
    imUtil.properCoadd.properCoaddLinearR( ...
    Cube, PSF, 1, Woracle, ...
    'Robust',false, ...
    'CGTol',1e-8);

% Differences relative to the oracle
D_bad   = R1 - Roracle;
D_rob   = R3 - Roracle;
D_clean = R0 - Roracle;

RMS_bad   = sqrt(mean(D_bad(:).^2));
RMS_rob   = sqrt(mean(D_rob(:).^2));
RMS_clean = sqrt(mean(D_clean(:).^2));

fprintf('\n');
fprintf('Oracle rejection test:\n');
fprintf('RMS(non-robust - oracle) = %12.5g\n', RMS_bad);
fprintf('RMS(robust     - oracle) = %12.5g\n', RMS_rob);
fprintf('RMS(clean      - oracle) = %12.5g\n', RMS_clean);
fprintf('Suppression factor       = %12.5g\n', RMS_bad./RMS_rob);
fprintf('max |P_R - P_Roracle|    = %12.5g\n', ...
        max(abs(P_R(:)-P_Roracle(:))));

% Display residuals relative to the oracle solution
Zmax = max(abs(D_bad(:)));

plot.plotImagesGrid({D_bad; D_rob; D_clean}, [1 3], ...
                    'Z1Z2',[-Zmax Zmax]);
colormap(flipud(gray))
set(gcf,'Color','w');


    %%

    Result = true;

end

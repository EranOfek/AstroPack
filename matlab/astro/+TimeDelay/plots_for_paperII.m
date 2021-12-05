function plots_for_paperII(Plot,Nsim)
% PLot options:
%       'single'
%     
% Example: TimeDelay.plots_for_paperII('single');

%%


switch lower(Plot)
    case 'single'
        
        %-------------------------------------------
        %% Plot examples for simulated light curves
        %-------------------------------------------
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;
        % simulate LC
        [ResLC,ResG]=generateLC(InPar);

        %[ResLC.F_t,Slope]=TimeDelay.end_matching(ResLC.T,ResLC.F_t);
        %[ResLC.x_t,Slope]=TimeDelay.end_matching(ResLC.T,ResLC.x_t);
        %%
        
        VecInvTau = (1./100:1./300:1./10);
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];
        DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)   InPar.x0  InPar.x(1), InPar.x(2), InPar.Gamma];
        FitPar = [InPar.A0  NaN         NaN          InPar.x0  InPar.x(1), InPar.x(2), InPar.Gamma];
        
        
        Nsim = 1;
        for Isim=1:1:Nsim
            [ResLC,ResG]=generateLC(InPar);
            [Res(Isim)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,ResLC.x_t,ResLC.y_t,ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                        'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                        'EndMatching',true);
                    %[0.005 0.5]);
        end
        
        figure(1);
        for Isim=1:1:Nsim
            plot(1./Res(Isim).Tau,Res(Isim).LL_H1-Res(1).LL_H0(1),'k-','LineWidth',2)
            input('a')
            
        end
        
        
        hold on;
        
        GaussProb = 1-2.*normcdf([3],0,1,'upper');
        Npar = 9;  % Tau, Alpha2, x0, x1, x2, y0, y1, y2, gamma
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 0.1],-Level(1).*ones(1,2),'k--');
        plot([0 0.1],-Level(2).*ones(1,2),'k--');
        plot([0 0.1],-Level(3).*ones(1,2),'k--');
        
        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        axis([0.01 0.1 -600 200])
        print Ast_Sim1_single_L_Tau.eps -depsc2
                    
        figure(2)
        plot(ResLC.T,ResLC.F_t,'LineWidth',3)
        hold on;

        f_t=TimeDelay.reconstruct_ft(ResLC.F_t,ResLC.x_t,InPar.x0,InPar.x,InPar.A0,InPar.A(1),ResLC.sigma_F_hat,ResLC.sigma_x_hat);
        plot(f_t,'Color',[0.8 0.8 0.8],'LineWidth',3);

        plot(ResLC.f1_t,'LineWidth',2)
        plot(ResLC.f2_t,'LineWidth',2)

        legend('Combined','Reconstructed','Image 1','Image 2','Location','NorthWest');
        H = xlabel('Time [days]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('Flux [arbitrary]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        axis([0 1000 50 330])

        %print Sim_3_LC.eps -depsc2
        print Ast_Sim1_single_LC.eps -depsc2
        %%

        figure(3)
        plot(ResLC.x_t,'LineWidth',2,'Color',[0.9 0.7 0.05]);
        hold on;
        plot(ResLC.chi_x_t,'k-','LineWidth',2);

        legend('x(t)','\chi(t)','combined');
        H = xlabel('Time [days]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('Position [arcsec]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Ast_Sim1_single_xt.eps -depsc2
        
        %print Sim_3_xt.eps -depsc2

        %%
        if 1==0
        figure(3);
        plot(ResLC.x_t,ResLC.y_t,'.','MarkerSize',8)
        H = xlabel('x(t) [arcsec]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('y(t) [arcsec]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        %print Sim5_xt_yt.eps -depsc2

        %Gx = ResLC.F_t.*ResLC.x_t;
        %Gy = ResLC.F_t.*ResLC.y_t;
        %figure(2);
        %plot(Gx,Gy,'.')

        figure(4);
        mGx = (ResLC.F_t-mean(ResLC.F_t)).*(ResLC.x_t-mean(ResLC.x_t));
        mGy = (ResLC.F_t-mean(ResLC.F_t)).*(ResLC.y_t-mean(ResLC.y_t));
        plot(mGx,mGy,'.')
        H = xlabel('G$_x$(t) [Flux arcsec]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('G$_y$(t) [Flux arcsec]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        %print Sim5_Gx_Gy.eps -depsc2

        %plot(ResLC.T,ResLC.x_t)
        %semilogy(ResLC.w,abs(ResLC.F_w))

        end
        
        %% Ghat/Fhat example

        figure(5);
        G = ResLC.F_t.*ResLC.x_t;
        Ghat = fft(G);
        Fhat = fft(ResLC.F_t);
        w    = TimeDelay.fft_freq(numel(ResLC.F_t),1);

        Ahat = (InPar.A(1).*InPar.x(1) + InPar.A(2).*InPar.x(2).*exp(1i.*ResLC.w.*InPar.Tau))./ ...
                (InPar.A(1) + InPar.A(2).*exp(1i.*ResLC.w.*InPar.Tau));

        [~,SI] = sort(w);
        
        semilogy(w(SI),abs(Ghat(SI)./Fhat(SI)));
        hold on
        semilogy(w(SI),abs(Ahat(SI)))
        %loglog(w(SI),abs(Ahat(SI)).*[w(SI).^(-InPar.Gamma.*0.5)]')
        axis([0 0.25 3e-2 3e1])

        
        
    case 'a0a1a2'

        %-----------------------------------
        %% Fit TimeDelay with unknown A0/A1/A2
        %-----------------------------------    
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        VecInvTau = [(1./100:1./1000:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];
        Nsim = 100

        FitPar = [NaN       NaN         NaN           InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];
        DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

        tic;


        clear Res
        clear ResLC
        clear AllLC
        load Res_a0a1a2.mat
        
        for Isim=38:1:Nsim
            [Isim, Nsim]
            
            % simulate LC
            [ResLC,ResG]=generateLC(InPar);

            tic;
           
            [Res(Isim)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,ResLC.x_t,ResLC.y_t,ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                        'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                        'EndMatching',true);
                   
            toc
            AllLC(Isim) = ResLC;                                 
        end

        save -v7.3 Res_a0a1a2.mat Res AllLC

        % the fitted parameters distribution
        OutBestPar = zeros(Nsim,sum(isnan(FitPar)));
        DeltaL     = zeros(Nsim,1);
        BestTau    = zeros(Nsim,1);

        for Isim=1:1:Nsim
            [~,MinI] = min(abs(Res(Isim).Tau - InPar.Tau));
            [DeltaL(Isim),MinItau] = min(Res(Isim).LL_H1-Res(Isim).LL_H0);
            BestTau(Isim)  = Res(Isim).Tau(MinItau);
            OutBestPar(Isim,:) = Res(Isim).BestPar_H1(MinI,:);
            
            AllDL(Isim,:) = Res(Isim).LL_H1(:).'-Res(Isim).LL_H0(:).';
        end
        Mean = mean(AllDL).';
        Std  = std(AllDL).';
        
        plot.patch_band(1./Res(1).Tau(:), Mean, Std);
        hold on;
        plot(1./Res(1).Tau, mean(AllDL));
        box on;
        axis([0.02 0.2 -270 0])
        
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 9;  % Tau, Alpha2, Alpha0
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');
               
        Npar = 11;  % Tau, Alpha2, Alpha0
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],min(Mean)+Level(1).*ones(1,2),'k--');
        plot([0 1],min(Mean)+Level(2).*ones(1,2),'k--');
        plot([0 1],min(Mean)+Level(3).*ones(1,2),'k--');
           
        
        H = xlabel('$1/\tau$ [day$^{-1}$]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta\ln{\mathcal{L}(x,F,|\tau,\alpha_0,\alpha_1,\alpha_2)}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';

        
         save -v7.3 Ast_Sim2_A012.mat Res OutBestPar AllLC

        %for I=1:1:numel(Res)
        %%    plot(1./Res(I).Tau,Res(I).LL_H1-Res(I).LL_H0)
        %    hold on;
        %end
        %axis([0.02 0.2 -800 200]);

        %print Sim2_LogL_A0A1A2.eps -depsc2
        print Ast_Sim2_A0A1A2.eps -depsc2
        
        %%
        Flag=OutBestPar(:,1)>1;
        Flag=DeltaL(:,1)>-chi2inv(0.9973,2)./2;
        plot(OutBestPar(~Flag,2),OutBestPar(~Flag,3)./OutBestPar(~Flag,2),'.','MarkerSize',10)
        hold on
        plot(OutBestPar(Flag,2),OutBestPar(Flag,3)./OutBestPar(Flag,2),'.','MarkerSize',10)   
        axis([0.8 2.5 0.35 0.55])
        
        H = xlabel('$\alpha_1$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\alpha_2/\alpha_1$');
        H.FontSize = 18;
        H.Interpreter = 'latex';

        plot.hist_ofplot
        
        %print Sim2_LogL_A0A1A2_ParsA1A2.eps -depsc2
        print Ast_Sim2_A0A1A2_ParsA1A2.eps -depsc2
        
        %%

        H = plot(1./BestTau,DeltaL,'.');
        axis([0.02 0.21 -800 600]);
        H = xlabel('$1/\tau$ [day$^{-1}$]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$-\Delta{\ln{\mathcal{L}}}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';


        [Hh]=plot.hist_ofplot('NbinX',20,'NbinY',20)

        print Sim2_LogL_vsBestTau.eps -depsc2
        
        
    case 'singleexample'
        % USED
        
        %------------------------------------
        %% plot single light curve example
        %------------------------------------
        InPar=select_parameters(1);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        VecInvTau = [(1./100:1./1000:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];
        Nsim = 1

        FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
        DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

        tic;


        % simulate LC
        [ResLC,ResG]=generateLC(InPar);

        Isim = 1;
        Irot = 1;
        VecRot(Irot) = 0;
        
        RotMat = [cosd(VecRot(Irot)), -sin(VecRot(Irot)); sin(VecRot(Irot)), cos(VecRot(Irot))];
                
        XY = RotMat*[ResLC.x_t(:).'; ResLC.y_t(:).'];

        tic;

        [Res(Isim,Irot)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                    'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                    'EndMatching',true);

        toc
            

        %
        save -v7.3 Ast_Sim1_Single.mat Res InPar ResLC
        
        %% plot light curve
        figure(1)
        plot(ResLC.T,ResLC.F_t,'LineWidth',3)
        hold on;

        f_t=TimeDelay.reconstruct_ft(ResLC.F_t,ResLC.x_t,InPar.x0,InPar.x,InPar.A0,InPar.A(1),ResLC.sigma_F_hat,ResLC.sigma_x_hat);
        plot(f_t,'Color',[0.8 0.8 0.8],'LineWidth',3);

        plot(ResLC.f1_t,'LineWidth',2)
        plot(ResLC.f2_t,'LineWidth',2)

        legend('Combined','Reconstructed','Image 1','Image 2','Location','NorthEast');
        H = xlabel('Time [days]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('Flux [arbitrary]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        axis([0 1000 50 330])
        
        print Ast_Sim1_Example_Ft.eps -depsc2
        
        %% plot position curve
        
        figure(2)
        plot(ResLC.x_t,'LineWidth',2,'Color',[0.9 0.7 0.05]);
        hold on;
        plot(ResLC.chi_x_t,'k-','LineWidth',2);

        legend('x(t)','\chi(t)','combined');
        H = xlabel('Time [days]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('Position [arcsec]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        axis([0 1000 -0.2 -0.08])
        
        print Ast_Sim1_Example_xt.eps -depsc2
        
        %% DL vs. Tau
        
        figure(3)
        DLL = (Res.LL_H1 - Res.LL_H0);
        DLL = DLL(:).';
        DLL = [DLL(1:91), NaN, DLL(92:end)];
        VecInvTau1 = [VecInvTau(1:91), NaN, VecInvTau(92:end)];
        plot(VecInvTau1(:),DLL(:),'k-','LineWidth',2);
        
        hold on;
        
        GaussProb = 1-normcdf([3],0,1,'upper');
        Npar = 9;  % Tau, Alpha2, x0, x1, x2, y0, y1, y2, gamma
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([-0.1 0.1],-Level(1).*ones(1,2),'k--');
        plot([-0.1 0.1],-Level(2).*ones(1,2),'k--');
        plot([-0.1 0.1],-Level(3).*ones(1,2),'k--');
        
        plot([-0.1 0.1],min(DLL)+Level(1).*ones(1,2),'k--');
        
        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        axis([-0.1 0.1 -2500 200])
        
        print Ast_Sim1_Example_DL_Tau.eps -depsc2
        
        %% DL countours vs A1/A2
        FitPar = [InPar.Tau, InPar.A0  NaN         NaN           InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];
        DefPar = [InPar.Tau, InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

        VecA1   = logspace(-1,1,150);
        VecA2A1 = logspace(-1.5,1.5,300);
        Na1 = numel(VecA1);
        Na2 = numel(VecA2A1);
        
        LogL_xF = zeros(Na1,Na2);
        LogL_GF = zeros(Na1,Na2);
        LogL_F = zeros(Na1,Na2);
        
        for Ia1=1:1:Na1
            Ia1
            for Ia2=1:1:Na2
                
                A2 = VecA2A1(Ia2).*VecA1(Ia1);
                [LogL_xF(Ia1,Ia2),LogL_GF(Ia1,Ia2),LogL_F(Ia1,Ia2)]=TimeDelay.logl_xF([VecA1(Ia1), A2],'FitPar',FitPar,...
                                                't',ResLC.T,...
                                                'F_t',ResLC.F_t,...
                                                'x_t',ResLC.x_t,...
                                                'sigma_F',ResLC.sigma_F_hat,...
                                                'sigma_x',ResLC.sigma_x_hat,...
                                                'EndMatching',true);
            end
        end
       
        
        
        
        GaussProb = 1-normcdf([1 2 3 4 5],0,1,'upper');
        Npar = 6;  % Tau, x0, x1, x2, Theta, gamma
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot.subplot1(3,1)
        
        plot.subplot1(1);
        Min = min(LogL_F(:));
        [~,H]=contour(VecA1,VecA2A1,LogL_F.',Min+[0 Level])
        colormap(jet)
        caxis([Min, Min+max(Level)]);
        set(gca,'XS','log','YS','log')
        hold on;
        plot(InPar.A(1), InPar.A(2)./InPar.A(1),'k+','MarkerSize',10);
        axis([0.1 3 0.03 10])
        H=text(1.3,7,'$\mathcal{L}(F)$');
        H.Interpreter = 'latex';
        H.FontSize = 12;
        
        plot.subplot1(2);
        Min = min(LogL_GF(:));
        contour(VecA1,VecA2A1,LogL_GF.',Min+[0 Level])
        colormap(jet)
        caxis([Min, Min+max(Level)]);
        set(gca,'XS','log','YS','log')
        hold on;
        plot(InPar.A(1), InPar.A(2)./InPar.A(1),'k+','MarkerSize',10);
        axis([0.1 3 0.03 10])
        H = ylabel('$\alpha_{2}/\alpha_{1}$');
        H.Interpreter = 'latex';
        H.FontSize = 16;
        H=text(1.3,7,'$\mathcal{L}(G|F)$');
        H.Interpreter = 'latex';
        H.FontSize = 12;
        
        plot.subplot1(3);
        Min = min(LogL_xF(:));
        contour(VecA1,VecA2A1,LogL_xF.',Min+Level)
        colormap(jet)
        caxis([Min, Min+max(Level)]);
        hold on;
        plot(InPar.A(1), InPar.A(2)./InPar.A(1),'k+','MarkerSize',10);
        set(gca,'XS','log','YS','log')
        axis([0.1 3 0.03 10])
        H = xlabel('$\alpha_{1}$');
        H.Interpreter = 'latex';
        H.FontSize = 16;
        H=text(1.3,7,'$\mathcal{L}(x,F)$');
        H.Interpreter = 'latex';
        H.FontSize = 12;
        
        set(gcf,'PaperPosition',[1.25 3.2895 6 12]);
        
        print Ast_Sim1_A1A2_FGx.eps -depsc2
        
        
    case 'fit_ax'
        %%% USED
        
        %-----------------------------------
        %% Fit TimeDelay with unknown A0/A1/A2/x_i
        %-----------------------------------    
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;
        % this is needed for the rotation...
        % make sure the errors are applied after rotation...
        sigma_x = InPar.sigma_x;
        InPar.sigma_x = 0;
        
        VecInvTau = [(1./100:1./300:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];
        Nsim = 1

        FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
        DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

        tic;


        clear Res
        clear ResLC
        clear AllLC
        VecRot = (-90:5:90).';
        
        Nrot   = numel(VecRot);
        
        for Isim=1:1:Nsim
            [Isim, Nsim]
            
            % simulate LC
            [ResLC,ResG]=generateLC(InPar);

            
            for Irot=1:1:Nrot
                [Irot, Nrot]
                
                RotMat = [cosd(VecRot(Irot)), -sin(VecRot(Irot)); sin(VecRot(Irot)), cos(VecRot(Irot))];
                
                XY = RotMat*[ResLC.x_t(:).'; ResLC.y_t(:).'];
                % add noise to positions (after rotation)
                XY = XY + sigma_x.*randn(size(XY));
                tic;

                [Res(Isim,Irot)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',ResLC.sigma_F_hat,...
                            sigma_x,...
                            'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                            'EndMatching',true);

                toc
            end
            
            AllLC(Isim) = ResLC;                                 
        end

        %
        save -v7.3 Ast_Sim2_fit_ax_T300.mat
        
        for Irot=1:1:Nrot
            if VecRot(Irot)<0
                %plot(VecInvTau, Res(Isim,Irot).LL_H1-Res(Isim,Irot).LL_H0,'--');
            else
                if VecRot(Irot)./30==floor(VecRot(Irot)./30)
                    VecRot(Irot)
                    DLL = Res(Isim,Irot).LL_H1-Res(Isim,Irot).LL_H0;
                    DLL = [DLL(1:28); NaN; DLL(29:end)];
                    VecInvTau1 = [VecInvTau(1:28), NaN, VecInvTau(29:end)];
                    plot(VecInvTau1,DLL,'LineWidth',2);
                end
            end
            hold on;
            
            [MinL(Irot),MinI] = min(Res(Isim,Irot).LL_H1-Res(Isim,Irot).LL_H0);
            BestTau(Irot) = 1./VecInvTau(MinI);
            BestPar(Irot,:) = Res(Isim,Irot).BestPar_H1(MinI,:);
        end
        
        legend('0','30','60','90','Location','NorthEast','AutoUpdate','off');
        box on
        
        Npar = 6
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 9;  % Tau, Alpha2, Alpha0
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([-0.1 0.1],-Level(1).*ones(1,2),'k--');
        plot([-0.1 0.1],-Level(2).*ones(1,2),'k--');
        plot([-0.1 0.1],-Level(3).*ones(1,2),'k--');
        axis([-0.1 0.1 -300 200])       
        
        H = xlabel('1/$\tau$ [1/day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta\ln{\mathcal{L}(x,F,|\tau,\alpha_i,x_i,\theta=0)}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        box on
        
        print Ast_Sim2_DL_TauRot_T300.eps -depsc2
        
        
        %% plot MinL vs. Rotation
        DiffTau = abs(BestTau - InPar.Tau);
        
        H=scatter(VecRot,MinL,50,DiffTau,'filled');
        H=colorbar;
        H.Label.String='$|\tau_{fitted}-\tau_{nominal}|$';
        H.Label.Interpreter='latex';
        H.Label.FontSize=16;
        
        %plot(VecRot,MinL,'o'); %,'LineWidth',2)
        hold on;
        
        Npar = 6
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 9;  % Tau, Alpha2, Alpha0
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([-90 90],-Level(1).*ones(1,2),'k--');
        plot([-90 90],-Level(2).*ones(1,2),'k--');
        plot([-90 90],-Level(3).*ones(1,2),'k--');
        axis([-90 90 -300 200])       
        
        H = xlabel('Rotation [deg]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta\ln{\mathcal{L}(x,F,|\tau,\alpha_i,x_i,\theta)}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        box on
        
        print Ast_Sim2_DL_Rot_T300.eps -depsc2
        
        
        
        
        
        %%
        
        plot(VecRot,BestTau)
        plot(VecRot,BestPar(:,3)./BestPar(:,2))
        
        %%
        H1=plot(VecRot,BestPar(:,4))              
        hold on;
        H2=plot(VecRot,BestPar(:,5))
        H3=plot(VecRot,BestPar(:,6))
        
        plot(VecRot,InPar.x0.*ones(size(VecRot)),'--','Color',H1.Color);
        plot(VecRot,InPar.x(1).*ones(size(VecRot)),'--','Color',H2.Color);
        plot(VecRot,InPar.x(2).*ones(size(VecRot)),'--','Color',H3.Color);
        
  case 'h0'
        
      %load Ast_Sim2_fit_ax_T300.mat
      %ResH1 = Res;
        
        %-----------------------------------
        %% Fit H0
        %-----------------------------------    
        
        
        %load Ast_Sim2_h0.mat
        
        %Nstart = numel(Res) + 1;
        Nstart = 1;
        
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        InPar.A = [1 0];
        InPar.Tau = 0;
        
        VecInvTau = [(1./100:1./300:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];
        Nsim = 10000;

        FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
        DefPar = [InPar.A0  InPar.A(1)  0.5    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

        tic;

        VecRot = (-90:10:90).';
        VecRot = 0
        
        Nrot   = numel(VecRot);
        
        
        
        
        for Isim=Nstart:1:Nsim
            [Isim, Nsim]
            
            % simulate LC
            [ResLC,ResG]=generateLC(InPar);

            
            for Irot=1:1:Nrot
                [Irot, Nrot]
                
                RotMat = [cosd(VecRot(Irot)), -sin(VecRot(Irot)); sin(VecRot(Irot)), cos(VecRot(Irot))];
                
                XY = RotMat*[ResLC.x_t(:).'; ResLC.y_t(:).'];
                
                tic;

                try
                    [Res(Isim,Irot)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                                'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                                'EndMatching',true);
                catch
                    Isim
                    save -v7.3 Ast_Sim2_h0.mat Res
                    
                    kk
                end

                toc
            end
            
            AllLC(Isim) = ResLC;                                 
        end

        %
        save -v7.3 Ast_Sim2_h0.mat
        'a'
        
        %%
        Nsim = numel(Res);
        for Isim=1:1:Nsim
            DL = Res(Isim).LL_H1 - Res(Isim).LL_H0;
            [MinDL(Isim), Imin] = min(DL);
            [MinDLsel(Isim), Imin] = min(DL( abs(VecInvTau)>1./(InPar.TotTime./10) ) );
            BestTau(Isim) = VecInvTau(Imin);
        end
        
        GaussProb = 1-normcdf([1:1:5],0,1,'upper');
        
        EdgeDL = (-500:50:1000).';
        EdgeIT = (0.01:0.009.*2:0.1).';
        Nit    = numel(EdgeIT) - 1;
        Cont = zeros(Nit,3);
        for Iit=1:1:Nit
            Flag = abs(BestTau)>EdgeIT(Iit) & abs(BestTau)<=EdgeIT(Iit+1);
            Cont(Iit,:) = quantile(MinDL(Flag),1-GaussProb(1:3));
            
        end
        
        CenIT = (EdgeIT(1:end-1) + EdgeIT(2:end)).*0.5;
        
        plot(CenIT(:),Cont(:,1));
        hold on;
        plot(CenIT,Cont(:,2));
        plot(CenIT,Cont(:,3));
        
        
        [N] = histcounts2(MinDL,abs(BestTau),EdgeDL,EdgeIT,'Normalization','pdf');
        BinDL = 0.5.*(EdgeDL(1:end-1) + EdgeDL(2:end));
        BinIT = 0.5.*(EdgeIT(1:end-1) + EdgeIT(2:end));
        
        
        CN = cumsum(N,1)
        H=contour(BinIT,BinDL,CN,[1-GaussProb(1:2)]);
        Hcb=colorbar
        hold on;  
        Hcb.Label.String='CDF';
        
        Npar = 6
        GaussProb = 1-normcdf([1:1:5],0,1,'upper');
        Npar = 9;  % Tau, Alpha2, Alpha0
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0.01 0.1],-Level(1).*ones(1,2),'k--');
        plot([0.01 0.1],-Level(2).*ones(1,2),'k--');
        plot([0.01 0.1],-Level(3).*ones(1,2),'k--');
        axis([0.01 0.1 -300 500]);
        
        
        H = xlabel('$|1/\tau|$ [1/day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta\ln{\mathcal{L}(x,F,|\tau,\alpha_i,x_i,\theta=0)}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        box on
        
        %%
        
        [N,Ed] = histcounts(MinDLsel(:),(-500:1:500),'Normalization','cdf')
        BinC = (Ed(1:end-1) + Ed(2:end)).*0.5; 
        
        bar(BinC,N);
        hold on;
        
        Npar = 6
        GaussProb = 1-normcdf([1:1:5],0,1,'upper');
        Npar = 9;  % Tau, Alpha2, Alpha0
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot(-Level(1).*ones(1,2),[1e-4 1],'k--');
        plot(-Level(2).*ones(1,2),[1e-4 1],'k--');
        plot(-Level(3).*ones(1,2),[1e-4 1],'k--');
        axis([-500 200 0 0.3])       
        
        H = ylabel('Cumulative Distribution');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H =xlabel('$\Delta\ln{\mathcal{L}(x,F|\tau,\alpha_i,x_i,\theta=0)}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        box on
        
        axis([-300 200 0 0.2])
        set(gca,'YS','log')
        
%         for I=1:1:numel(ResH1)
%             DL_H1 = ResH1(I).LL_H1 - ResH1(I).LL_H0;
%             MinDLH1(I) = min(DL_H1);
%             
%         end
        
        print Ast_Sim2_h0.eps -depsc2
        
        
    case 'wronggamma'
        %--------------------------
        %% using the wrong gamma
        %--------------------------
        
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        %InPar.Gamma=2.0;
        InPar.Gamma=2.5;
        
        VecInvTau = [(1./100:1./300:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];
        Nsim = 1

        
        FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
        DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

        tic;


        VecGamma = [1.5,2,2.5,3,3.5];
        Ngamma   = numel(VecGamma);
        
        VecRot = 0; 
        
        Nrot   = numel(VecRot);
        
        for Isim=1:1:Nsim
            [Isim, Nsim]
            
            % simulate LC
            [ResLC,ResG]=generateLC(InPar);

            
            for Igamma=1:1:Ngamma
                [Igamma, Nrot]
                
                FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           VecGamma(Igamma)];
                DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    VecGamma(Igamma)];

                
                Irot = 1;
                RotMat = [cosd(VecRot(Irot)), -sin(VecRot(Irot)); sin(VecRot(Irot)), cos(VecRot(Irot))];
                
                XY = RotMat*[ResLC.x_t(:).'; ResLC.y_t(:).'];
                
                tic;

                [Res(Isim,Igamma)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                            'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                            'EndMatching',true);

                toc
            end
            
            AllLC(Isim) = ResLC;                                 
        end

        %
        save -v7.3 Ast_Sim2_fit_wronggamma_T300_g25.mat
        
        load Ast_Sim2_fit_wronggamma_T300_g25
        Res25 = Res;
        load Ast_Sim2_fit_wronggamma_T300
        
        for Igamma=1:1:Ngamma
            VecInvTau1 = [VecInvTau(1:28), NaN, VecInvTau(29:end)]
            DLL = [Res(Isim,Igamma).LL_H1 - Res(Isim,Igamma).LL_H0];
            DLL = DLL(:).';
            DLL = [DLL(1:28), NaN, DLL(29:end)];
            
            DLL25 = [Res25(Isim,Igamma).LL_H1 - Res25(Isim,Igamma).LL_H0];
            DLL25 = DLL25(:).';
            DLL25 = [DLL25(1:28), NaN, DLL25(29:end)];
            
            plot(VecInvTau1,DLL);
            hold on;
            plot(VecInvTau1,DLL25+100);
        end
        H=legend('$\gamma=1.5$','$\gamma=2.0$','$\gamma=2.5$','$\gamma=3.0$','$\gamma=3.5$','Location','NorthEast');
        H.Interpreter = 'latex';
        
        axis([-0.1 0.1 -200 200])
        plot([-0.1 0.1],[-190+chi2inv(0.68,[9 9])],'k--')
   
        H = xlabel('$1/\tau$ [1/day]');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        H = ylabel('$\Delta\ln{\mathcal{L}(x,F,|\tau,\alpha_i,x_i,\theta=0)}$');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        
        print Ast_Sim2_Tau_DLL_wronggamma.eps -depsc2
        
        
    case 'sens_a2'
        
        %-------------------------
        %% sensitivity to A2/A1
        %-------------------------
        
        
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        VecInvTau = [(1./100:1./300:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];
        Nsim = 100;

        
        FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
        DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

        tic;


        A2 = [0.05]; %0.01 0.03 0.1 0.3];
        Na2   = numel(A2);
        
        VecRot = 0; 
        
        Nrot   = numel(VecRot);
        
        for Isim=1:1:Nsim
            [Isim, Nsim]
            
            % simulate LC
            %[ResLC,ResG]=generateLC(InPar);

            
            for Ia2=1:1:Na2
                [Ia2, Na2]
                
                InPar.A(2) = A2(Ia2);
                
                [ResLC,ResG]=generateLC(InPar);
                
                FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
                DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

                
                Irot = 1;
                RotMat = [cosd(VecRot(Irot)), -sin(VecRot(Irot)); sin(VecRot(Irot)), cos(VecRot(Irot))];
                
                XY = RotMat*[ResLC.x_t(:).'; ResLC.y_t(:).'];
                
                tic;

                [Res(Isim,Ia2)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                            'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                            'EndMatching',true);

                toc
            end
            
            AllLC(Isim) = ResLC;                                 
        end

        %
        save -v7.3 Ast_Sim2_fit_sensA2_T300.mat
        'a'
        
        for Ia1=1:1:1,
            %size(Res,2)
            M(Ia2).DLL = zeros(Nsim,numel(VecInvTau));
            for Isim=1:1:size(Res,1)
                M(Ia2).DLL(Isim,:) = [Res(Isim,Ia2).LL_H1 - Res(Isim,Ia2).LL_H0].';
            end
            M(Ia2).MeanDLL = mean(M(Ia2).DLL);
            M(Ia2).StdDLL  = std(M(Ia2).DLL);
            M(Ia2).QuanDLL1 = quantile(M(Ia2).DLL,normcdf(0,1,1));
            M(Ia2).QuanDLL2 = quantile(M(Ia2).DLL,1-normcdf(0,1,1));
            
            %patch([VecInvTau(:); flipud(VecInvTau(:))], [M(Ia2).QuanDLL1(:); flipud(M(Ia2).QuanDLL2(:))],[0.8 0.8 0.8]);
            
            
            H1=plot(VecInvTau,M(Ia2).MeanDLL,'k-','LineWidth',2);
            hold on;
            %H2=plot(VecInvTau, M(Ia2).QuanDLL1,'k--');
            %H2.Color = H1.Color;
            
            %H2=plot(VecInvTau, M(Ia2).QuanDLL2,'k--');
            %H2.Color = H1.Color;
            
            
            
        end
        
        %axis([-0.1 0.1 -250 100])
        
        plot(1./[InPar.Tau InPar.Tau],[-210 -175],'--')
      
        Min = min(M(Ia2).MeanDLL);
        hold on
       
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 9;  % Tau, Alpha2, Alpha0
        Level = 0.5.*chi2inv(GaussProb,Npar);
    
        plot([-0.1 0.1],[Min Min]+Level(1),'k--')
        plot([-0.1 0.1],[Min Min]+Level(2),'k--')
        plot([-0.1 0.1],[Min Min]+Level(3),'k--')
    
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 1;  % Tau, Alpha2, Alpha0
        Level = 0.5.*chi2inv(GaussProb,Npar);
    
        plot([-0.1 0.1],[Min Min]+Level(1),'r--')
        plot([-0.1 0.1],[Min Min]+Level(2),'r--')
        plot([-0.1 0.1],[Min Min]+Level(3),'r--')
        
        
        H = xlabel('$1/\tau$ [1/day]');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        H = ylabel('$\Delta\ln{\mathcal{L}(x,F,|\tau,\alpha_i,x_i,\theta=0)}$');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        
        print Ast_Sim2_a2_005.eps -depsc2
        
    case 'test_timed'
        
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        VecInvTau = [(1./100:1./300:1./5)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];

        TimeDelayVec = (10:5:50).';
        
        FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
        DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

        tic;

        Ntd = numel(TimeDelayVec);
        for Itd=1:1:Ntd
            InPar.Tau = TimeDelayVec(Itd);
            
            [ResLC,ResG]=generateLC(InPar);

        
            VecRot = 0; 
            Irot   = 1;

        
            RotMat = [cosd(VecRot(Irot)), -sin(VecRot(Irot)); sin(VecRot(Irot)), cos(VecRot(Irot))];

            XY = RotMat*[ResLC.x_t(:).'; ResLC.y_t(:).'];


            [Res(Itd)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                        'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                        'EndMatching',true);
        end


        %
        'a'
        save Ast_Sim2_TestTimeD.mat Res InPar VecInvTau
        
        for Itd=1:2:numel(Res)
            FF=VecInvTau>0;
            H=plot(1./VecInvTau(FF),Res(Itd).LL_H1(FF)-Res(Itd).LL_H0);
            hold on;
            plot(TimeDelayVec(Itd).*ones(1,2),[-500 200],'k--','Color',H.Color);
        end
        
        axis([5 70 -450 150])
                 
        H = xlabel('$\tau$ [day]');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        H = ylabel('$\Delta\ln{\mathcal{L}(x,F,|\tau,\alpha_i,x_i,\theta=0)}$');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        
        print Ast_Sim2_VarTimeDelays.eps -depsc2
        
        
    case 'nonzero_a0_cont'
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        InPar.StdMeanRange = [0.05 1];
        
        
        
        
        
        VecInvTau = [(1./100:1./300:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];

        InPar.A0 = 2;
        [ResLC,ResG]=generateLC(InPar);
        
        t   = ResLC.T;
        F_t = ResLC.F_t;
        x_t = ResLC.x_t;
        y_t = ResLC.y_t;
        
        N   = numel(F_t);
        t_step = 1;
        
        freqs = TimeDelay.fft_freq(N, t_step);

        sigma_F_hat = ResLC.sigma_F_hat;
        sigma_x_hat = ResLC.sigma_x_hat;
        w = 2.*pi*freqs;

        F_w = fft(F_t) ./ sqrt(N);
        Gx_t = x_t.*F_t;
        Gx_w = fft(Gx_t) ./ sqrt(N);
        Gy_t = y_t.*F_t;
        Gy_w = fft(Gy_t) ./ sqrt(N);


        sigma_y = ResLC.sigma_x_hat;
        sigma_y_hat = sigma_y;
        
        N = length(F_t);

        DFT = fft(eye(N), N, 1) ./ sqrt(N);
        DFT_dagger = DFT';
        LogZ = sum(log(F_t));
        Gamma_1_ = ((DFT * diag(F_t.^2)) * DFT_dagger) * sigma_x_hat^2;
        
        
        Limits = [-300 300; 0 3; 1e-5 5;0 5;  -1 1; -2.1 2.1; -2.1 2.1;     1.5 3.5]; %  with Tau
        
        VecA0    = (0:0.1:2.5).';
        VecA2    = (0.1:0.1:0.9).';
        Na0      = numel(VecA0);
        Na2      = numel(VecA2);
        
        for Ia0=1:1:Na0
            for Ia2=1:1:Na2
                Pars = [InPar.Tau, VecA0(Ia0), InPar.A(1), VecA2(Ia2), InPar.x0, InPar.x(1), InPar.x(2), InPar.Gamma];
                FitPar = nan(size(Pars));
                
                [LogL_xF,LogL_GF,LogL_F]=TimeDelay.logl_xF(Pars,...
                                         'FitPar',Pars,...
                                         'w',w,...
                                         'Limits',Limits,...
                                         't',t,...
                                         'x_t',x_t,...
                                         'Gx_w',Gx_w,...
                                         'F_t',F_t,...
                                         'F_w',F_w,...
                                         'sigma_F',sigma_F_hat,...
                                         'sigma_x',sigma_x_hat,...
                                         'DFT',DFT,...
                                         'DFT_dagger',DFT_dagger,...
                                         'LogZ',LogZ,...
                                         'Gamma_1_',Gamma_1_,...
                                         'EndMatching',InPar.EndMatching,...
                                         'TwoD',false,...
                                         'Verbose',false);
                                     
                 LL(Ia0,Ia2) = LogL_xF;
            end
        end
        
        'a'
        
        
        
    case 'nonzero_a0'
        
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        InPar.StdMeanRange = [0.05 1];
        
        Nsim = 100;
        
        VecInvTau = [(1./100:1./300:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];

        A0Vec = 2; %(0:0.10:5:50).';
        
        
        tic;

        
        Na0 = numel(A0Vec);
        for Isim=1:1:Nsim
            for Ia0=1:1:Na0
                InPar.A0 = A0Vec(Ia0);

                [ResLC,ResG]=generateLC(InPar);

                FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
                DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];


                VecRot = 0; 
                Irot   = 1;


                RotMat = [cosd(VecRot(Irot)), -sin(VecRot(Irot)); sin(VecRot(Irot)), cos(VecRot(Irot))];

                XY = RotMat*[ResLC.x_t(:).'; ResLC.y_t(:).'];


                [Res(Isim,Ia0)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                            'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                            'EndMatching',true);
                        
                
                % start with the wrong A0:
                FitPar = [0         NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
                DefPar = [0         InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];
       
                [ResW(Isim,Ia0)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                            'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                            'EndMatching',true);     
                        'a'
            end
        end

        %
        'a'
        save Ast_Sim2_VariA0.mat Res ResW InPar VecInvTau
        
        Ia0 = 1;
        for Isim=1:1:Nsim
            %plot(VecInvTau,Res(Isim,Ia0).LL_H1 - Res(Isim,Ia0).LL_H0);
            MatAll(Isim,:) = Res(Isim,Ia0).LL_H1(:).' - Res(Isim,Ia0).LL_H0;
            MatAllW(Isim,:) = ResW(Isim,Ia0).LL_H1(:).' - ResW(Isim,Ia0).LL_H0;
            [~,MinI] = min(Res(Isim,Ia0).LL_H1(:).' - Res(Isim,Ia0).LL_H0);
            ParAll(Isim,:) = Res(1).BestPar_H1(MinI,:);
            [~,MinI] = min(ResW(Isim,Ia0).LL_H1(:).' - ResW(Isim,Ia0).LL_H0);
            ParWAll(Isim,:) = ResW(1).BestPar_H1(MinI,:);
            
            %hold on;
        end
        plot(VecInvTau,mean(MatAll),'k-','LineWidth',2)
        hold on;
        %plot(VecInvTau,mean(MatAllW),'k-','Color',[0.8 0.8 0.8],'LineWidth',2)
        
        axis([-0.1 0.1 -120 0])
        
        
        H = xlabel('$1/\tau$ [day]');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        H = ylabel('$\Delta\ln{\mathcal{L}(x,F,|\tau,\alpha_i,x_i,\theta=0)}$');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        
        print Ast_Sim2_A0_2.eps -depsc2
        
        
     
        
        
                        
    case 'sens_sigma_f'
        
        %-------------------------
        %% sensitivity to sigma_F
        %-------------------------
        
        
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        InPar.sigma_F_rel = 0.1;
        
        VecInvTau = [(1./100:1./300:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];
        Nsim = 1;

        
        FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
        DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

        tic;

        VecRot = 0; 
        
        Nrot   = numel(VecRot);
        
        VecFs = [0.03, 0.05, 0.1];
        Ns    = numel(VecFs);
        
        for Isim=1:1:Nsim
            [Isim, Nsim]
            
            for Is=1:1:Ns
                
                % simulate LC
                %[ResLC,ResG]=generateLC(InPar);


                InPar.sigma_F_rel = VecFs(Is);
                [ResLC,ResG]=generateLC(InPar);

                FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
                DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];


                Irot = 1;
                RotMat = [cosd(VecRot(Irot)), -sin(VecRot(Irot)); sin(VecRot(Irot)), cos(VecRot(Irot))];

                XY = RotMat*[ResLC.x_t(:).'; ResLC.y_t(:).'];

                tic;

                [Res(Isim,Is)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                            'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                            'EndMatching',true);

                toc
                
                AllLC(Isim) = ResLC;       
            end
            
                                      
        end

        %
        save -v7.3 Ast_Sim2_fit_sensSigmaF_T300.mat
        'a'
        
        for Is=1:1:Ns
            plot(VecInvTau,Res(Is).LL_H1-Res(Is).LL_H0,'LineWidth',2)
            hold on;
        end
        axis([-0.1 0.1 -500 200])
        H=legend('$\sigma_F/F=0.03$','$\sigma_F/F=0.05$','$\sigma_F/F=0.1$','Location','SouthWest');
        H.Interpreter='latex';
        
        
        H = xlabel('$1/\tau$ [1/day]');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        H = ylabel('$\Delta\ln{\mathcal{L}(x,F,|\tau,\alpha_i,x_i,\theta=0)}$');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        
        print Ast_Sim2_sensSigmaF.eps -depsc2
        
     case 'sens_slope'
        
        %-------------------------
        %% sensitivity to adding slope to one LC
        %-------------------------
        
        
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        InPar.sigma_F_rel = 0.05;
        
        VecInvTau = [(1./100:1./300:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];
        Nsim = 1;

        
        FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
        DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];

        tic;

        VecRot = 0; 
        
        Nrot   = numel(VecRot);
        
        VecFs = [0.05];
        Ns    = numel(VecFs);
        
        VecSlope = [0,0.03,0.1,0.3];
        Nslope   = numel(VecSlope);
        
        for Isim=1:1:Nsim
            [Isim, Nsim]
            
            for Is=1:1:Ns
                
                for Islope=1:1:Nslope
                    
                    % simulate LC
                    %[ResLC,ResG]=generateLC(InPar);


                    InPar.sigma_F_rel = VecFs(Is);
                    InPar.Slope       = VecSlope(Islope);
                    [ResLC,ResG]=generateLC(InPar);

                    FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
                    DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];


                    Irot = 1;
                    RotMat = [cosd(VecRot(Irot)), -sin(VecRot(Irot)); sin(VecRot(Irot)), cos(VecRot(Irot))];

                    XY = RotMat*[ResLC.x_t(:).'; ResLC.y_t(:).'];

                    tic;

                    [Res(Isim,Is,Islope)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',ResLC.sigma_F_hat,ResLC.sigma_x_hat,...
                                'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                                'EndMatching',true);

                    toc

                    AllLC(Isim) = ResLC;       
                end
            end
                                      
        end

        %
        save -v7.3 Ast_Sim2_fit_sensSlope.mat
        'a'
        
        GaussProb = 1-normcdf([1:1:5],0,1,'upper');
        Npar = 9;  % Tau, Alpha2, Alpha0
        Level = 0.5.*chi2inv(GaussProb,Npar);

        
        
        for Is=1:1:Nslope
            H=plot(VecInvTau,Res(Is).LL_H1-Res(Is).LL_H0,'LineWidth',2)
            hold on;
            [Min,Imin] = min(Res(Is).LL_H1-Res(Is).LL_H0);
            Hl = plot([-0.1 0.1],[Min + Level(3)].*ones(1,2),'--','LineWidth',1);
            Hl.Color = H.Color;
            
        end
        axis([-0.1 0.1 -400 200])
        H=legend('slope=0','slope=0.03','slope=0.1','slope=0.3','Location','SouthWest');
        H.Interpreter='latex';
        
        
        H = xlabel('$1/\tau$ [1/day]');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        H = ylabel('$\Delta\ln{\mathcal{L}(x,F,|\tau,\alpha_i,x_i,\theta=0)}$');
        H.Interpreter = 'latex';
        H.FontSize = 18;
        
        
        
        
        print Ast_Sim2_sensSlope.eps -depsc2
           
        
        
        
    case 'unevenly'
        
        %--------------------------------
        %% unevenly spaced simulations
        %--------------------------------
        
        InPar=select_parameters(2);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;

        VecInvTau = [(1./100:1./700:1./10)];
        VecInvTau = [-fliplr(VecInvTau), VecInvTau];
        Nsim = 100;

        
        for Isim=1:1:Nsim
        
            [ResLC,Section,PS]=generateLCuneq(InPar);

            ResLC
            VecRot = 0;


            TT = (1:1:700).';
            ResLC.F_t = interp1(ResLC.T,ResLC.F_t,TT,'pchip');
            ResLC.x_t = interp1(ResLC.T,ResLC.x_t,TT,'pchip');
            ResLC.y_t = interp1(ResLC.T,ResLC.y_t,TT,'pchip');
            ResLC.T   = TT;

            FitPar = [NaN       NaN         NaN           NaN        NaN           NaN           InPar.Gamma];
            DefPar = [InPar.A0  InPar.A(1)  InPar.A(2)    InPar.x0   InPar.x(1)    InPar.x(2)    InPar.Gamma];


            Irot = 1;
            RotMat = [cosd(VecRot(Irot)), -sin(VecRot(Irot)); sin(VecRot(Irot)), cos(VecRot(Irot))];

            XY = RotMat*[ResLC.x_t(:).'; ResLC.y_t(:).'];

            tic;


            [Res(Isim)] = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,XY(1,:).',XY(2,:).',...
                    ResLC.Base.sigma_F_hat,ResLC.Base.sigma_x_hat,...
                        'VecInvTau',VecInvTau,'DefPar',DefPar,'FitPar',FitPar,'Min_w',[0.0001 Inf],...
                        'EndMatching',true);

            toc
            
            AllLC(Isim) = ResLC;
        end
        
        save -v7.3 Ast_Unevenly_LL.mat Res AllLC
        
        plot(VecInvTau,Res.LL_H1 - Res.LL_H0);
        
        
        
        
        
    case 'alpha'
        %-------------------------
        %% Plot examples alpha vs alpha ratio
        %-------------------------------------------
        InPar=select_parameters(1);
        InPar.EndMatching = false;
        InPar.AliasFactor = 1;
        % simulate LC
        [ResLC,ResG]=generateLC(InPar);

        
        FitPar = [InPar.A0   InPar.A(1)  InPar.A(2)  InPar.x0   InPar.x(1)   InPar.x(2)    InPar.Gamma];  % [A0, A1, A2, x0, x1, x2, gamma]
        VecA1 = logspace(log10(0.3),log10(10),10);
        VecA2dA1 = logspace(log10(0.05),log10(0.96),10);

        Res=TimeDelay.fit_scan_alpha_astrometric_flux(ResLC.T, ResLC.F_t, ResLC.x_t, ResLC.sigma_F_hat, ResLC.sigma_x_hat,...
                            'Tau',InPar.Tau,'FitPar',FitPar,'VecA1',VecA1,'VecA2dA1',VecA2dA1,'Min_w',[2.*pi./200]);


        %%
        figure(1);
        Data = Res.LL_xF;
        Min=min(Data(:));
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;
        Level = 0.5.*chi2inv(GaussProb,Npar);
        contour(Res.A1,Res.A2dA1,Data'-Min,Level);
        H=colorbar;
        H.Label.String = '$\Delta$ ln[$\mathcal{L}$(x,F)]';
        H.Label.Interpreter = 'latex';
        H = xlabel('$\alpha_{1}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\alpha_{2}$/$\alpha_{1}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        set(gca,'XS','log','YS','log')
        set(gca,'XTick',[0.1 0.3 1 3 10]);
        set(gca,'YTick',[0.05 0.1 0.3 0.5 0.9]);

        %print Ast_Sim1_LLxF_A1vsA2A1.eps -depsc2
        %print Sim1_LLxF_A1vsA2A1_p_20_0_1_05_0_p01_m04_3.eps -depsc2

        %
        figure(2);
        Data = Res.LL_GF;
        Min=min(Data(:));
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;
        Level = 0.5.*chi2inv(GaussProb,Npar);
        contour(Res.A1,Res.A2dA1,Data'-Min,Level);
        H=colorbar;
        H.Label.String = '$\Delta$ ln[$\mathcal{L}$(G$|$F)]';
        H.Label.Interpreter = 'latex';
        H = xlabel('$\alpha_{1}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\alpha_{2}$/$\alpha_{1}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        set(gca,'XS','log','YS','log')
        set(gca,'XTick',[0.1 0.3 1 3 10]);
        set(gca,'YTick',[0.05 0.1 0.3 0.5 0.9]);

        %print Sim1_LLGF_A1vsA2A1_p_20_0_1_05_0_p01_m04_3.eps -depsc2

        %
        figure(3);
        Data = Res.LL_F;
        Min=min(Data(:));
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;
        Level = 0.5.*chi2inv(GaussProb,Npar);
        contour(Res.A1,Res.A2dA1,Data'-Min,Level);
        H=colorbar;
        H.Label.String = '$\Delta$ ln[$\mathcal{L}$(F)]';
        H.Label.Interpreter = 'latex';
        H = xlabel('$\alpha_{1}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\alpha_{2}$/$\alpha_{1}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        set(gca,'XS','log','YS','log')
        set(gca,'XTick',[0.1 0.3 1 3 10]);
        set(gca,'YTick',[0.05 0.1 0.3 0.5 0.9]);

        %print Sim1_LLF_A1vsA2A1_p_20_0_1_05_0_p01_m04_3.eps -depsc2




    case 't_tau'
        InPar=select_parameters(1);
        InPar.EndMatching = false;
        InPar.AliasFactor = 10;
        InPar.Tau = 20;

        VecInvTau = [(1./100:1./1000:1./5)];
        Ntau      = numel(VecInvTau);
        
        Nsim = 10
        MatChi2 = zeros(Nsim,Ntau);
        
        ParH0 = 0;
        FitParH0 = [0 InPar.A0, InPar.A(1), InPar.A(2), InPar.x0, InPar.x(1), InPar.x(2)];
        for Isim=1:1:Nsim
            [ResLC,ResG]=generateLC(InPar);

            %[Tau, A0, A1, A2, x0, x1, x2]

            [Chi2_H0,Nobs,F_t,f_t] = TimeDelay.chi2_xF(ParH0,FitParH0,'t',ResLC.T,'F_t',ResLC.F_t,'x_t',ResLC.x_t,'ErrF_t',ResLC.sigma_F_hat,'Errx_t',ResLC.sigma_x_hat);
            
            for Itau=1:1:Ntau
                Tau = 1./VecInvTau(Itau);
                Par = Tau;
                FitPar = [NaN InPar.A0, InPar.A(1), InPar.A(2), InPar.x0, InPar.x(1), InPar.x(2)];

                [Chi2(Itau),Nobs,F_t,f_t] = TimeDelay.chi2_xF(Par,FitPar,'t',ResLC.T,'F_t',ResLC.F_t,'x_t',ResLC.x_t,'ErrF_t',ResLC.sigma_F_hat,'Errx_t',ResLC.sigma_x_hat);
                
                MatChi2(Isim,Itau) = Chi2(Itau) - Chi2_H0;
            end
        end
        
        plot(VecInvTau,mean(MatChi2))
        %plot(VecInvTau,Chi2) 
        'a'
        
        
    case 't_a0a1a2'
        InPar=select_parameters(1);
        InPar.EndMatching = false;
        InPar.AliasFactor = 1;
        InPar.Tau = 20;

        VecInvTau = [(1./100:1./1000:1./5)];
        Ntau      = numel(VecInvTau);
        
        Nsim = 1000
        MatChi2 = zeros(Nsim,Ntau);
        
        FitPar = [InPar.A0, NaN         NaN       , InPar.x0, InPar.x(1), InPar.x(2)];
        DefPar = [InPar.A0, InPar.A(1), InPar.A(2), InPar.x0, InPar.x(1), InPar.x(2)];
        
        for Isim=1:1:Nsim
            [ResLC,ResG]=generateLC(InPar);

            %[Tau, A0, A1, A2, x0, x1, x2]

            %[Chi2_H0,Nobs,F_t,f_t] = TimeDelay.chi2_xF(ParH0,FitParH0,'t',ResLC.T,'F_t',ResLC.F_t,'x_t',ResLC.x_t,'ErrF_t',ResLC.sigma_F_hat,'Errx_t',ResLC.sigma_x_hat);
            
            Res(Isim)=TimeDelay.fit_astrometric_flux_simple('FitPar',FitPar,'DefPar',DefPar,...
                                                    't',ResLC.T,'F_t',ResLC.F_t,'x_t',ResLC.x_t,...
                                                    'ErrF_t',ResLC.sigma_F_hat,'Errx_t',ResLC.sigma_x_hat);
                                                
        end
        
        AllDL = zeros(Nsim,numel(Res(1).Tau));
        for Isim=1:1:Nsim
            AllDL(Isim,:) = [Res(Isim).LL_H1(:) - Res(Isim).LL_H0(:)]';
        end
        
        MeanDL = mean(AllDL);
        AllDL  = AllDL - min(AllDL,[],2);
        StdDL  = imUtil.background.rstd(AllDL);
        
        plot.patch_band(1./Res(1).Tau(:),MeanDL(:),StdDL(:));
        hold on;
        plot(1./Res(1).Tau,MeanDL)
        
        'a'
        
        print Ast_Sim2_tA0A1A2.eps -depsc2
        
    otherwise
        error('Unknwon Plot option');
end






end % end main function


%%  Simulation parameters
function InPar=select_parameters(SimName)
% generate parameters for specific simulation number
% Input  : - 11, 15

    InPar.Cyclic = false;
    InPar.x0  = 0;
    InPar.y0  = 0;
    InPar.y   = [0.0  0.0];   
    InPar.f_dc = 50;
    InPar.DeltaT  = 1;
    InPar.StdMeanRange = [0.1 0.15];
    InPar.AliasFactor  = 10;
    InPar.EndMatching  = true;
    
    switch SimName
        case 1
            % used
            InPar.Tau = 30;
            InPar.A0  = 0;
            InPar.A   = [1 0.5];
            InPar.x   = [0.2 -0.8];
            InPar.Gamma = 2.0;
            InPar.TotTime = 1000;
            InPar.sigma_x = 0.01;
            InPar.sigma_F_rel = 0.03./sum(InPar.A);  %0.02.*sum(InPar.A);
            
        case 2
            % used
            InPar.Tau = 30;
            InPar.A0  = 0;
            InPar.A   = [1 0.5];
            InPar.x   = [0.1 -0.4];
            InPar.Gamma = 2.0;
            InPar.TotTime = 300;
            InPar.sigma_x = 0.01;
            InPar.sigma_F_rel = 0.03./sum(InPar.A); %0.02.*sum(InPar.A);    
            
            InPar.Slope   = 0;
            
        case 5

            % simulation for non evenly spaced case
            InPar.DeltaT  = 0.1;

            InPar.Tau = 25;
            InPar.A0  = 0;
            InPar.A   = [1 0.5];
            InPar.x   = [-0.1 0.4];
            InPar.Gamma = 2.0;
            InPar.TotTime = 1000;
            InPar.sigma_x = 0.02;
            InPar.sigma_F_rel = 0.02.*sum(InPar.A);

    end
end


%% Plot examples for simulated light curves
function [ResLC,ResG]=generateLC(InPar)


    [ResLC,ResG]=TimeDelay.rand_lensed('A0',InPar.A0,'A',InPar.A,'Tau',InPar.Tau,...
                                    'x0',InPar.x0,'y0',InPar.y0,'x',InPar.x,'y',InPar.y,...
                                    'f_dc',InPar.f_dc,'Gamma',InPar.Gamma,...
                                    'TotTime',InPar.TotTime,...
                                    'DeltaT',InPar.DeltaT,...
                                    'sigma_x',InPar.sigma_x,...
                                    'sigma_F_rel',InPar.sigma_F_rel,...
                                    'Cyclic',InPar.Cyclic,...
                                    'Validate',true,...
                                    'Slope',InPar.Slope,...
                                    'StdMeanRange',InPar.StdMeanRange,...
                                    'AliasFactor',InPar.AliasFactor,...
                                    'EndMatching',InPar.EndMatching);
end


function [ResLC,Section,PS]=generateLCuneq(InPar)

    %T=timeseries.random_time_sequence; 
    T=timeseries.random_time_sequence(3.*365,1,270,0.05,0.8);
    ResLC=TimeDelay.rand_lensed_uneq(T,InPar);
    
    N = numel(T);
    LC = [ResLC.T, ResLC.F_t, ResLC.Base.sigma_F_hat.*ones(N,1)];
    [PS,Section]=TimeDelay.power_spectrum_sections(LC);
    
end
function plots_for_paperI(Plot,Nsim)
% Input  : - plot options:
%               'a1a2' - a1 vs a2/a1 contour map for known parameters
%               'fitgamma' - fit gamma
%               'vargamma' - effect of gamma on mean solution
%               'wronggamma' - fitting with the wrong gamma
%               'varsigmaf' - effect of sigma_F on mean solution
%               'vara2' - effect if a2 on mean solution
%               'vart'  - effect on series time length on mean solution
%               'h0' -H0
%               'uneq' - unequally spaced light curves
%          - Number of simulations (if relevant).
%            Default is 1000.
% Example : TimeDelay.plots_for_paperI('a1a2');


if nargin<2
    Nsim = 1000;
end

%%

switch lower(Plot)
    case 'ps'
        InPar=select_parameters(11);
        % simulate LC
        [ResLC,ResG]=generateLC(InPar);
        
        Pars = [InPar.Tau, InPar.A(1), InPar.A(2), InPar.Gamma];
        FitPar = [InPar.Tau, InPar.A(1), InPar.A(2), InPar.Gamma];
        
        Min_w = [2.*pi./10000 2.*pi./0.01];
        
        %[LogLikeF,Sigma_F,Sigma_phi]=TimeDelay.logl_F(Pars, FitPar, [], Min_w, ResLC.w, ResLC.F_w, ResLC.sigma_F_hat);
        [LogLikeF,Sigma_F,Sigma_phi]=TimeDelay.logl_F(Pars,'FitPar',FitPar,...
                    'Min_w',Min_w,...
                    'w',ResLC.w,...
                    'F_w',ResLC.F_w,...
                    'sigma_F',ResLC.sigma_F_hat);
        
        
        FitPar = [NaN, NaN, NaN];
        DefPar = [InPar.A(1), InPar.A(2), InPar.Gamma];
        Res = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',1./InPar.Tau,'FitPar',FitPar,'DefPar',DefPar,...
                'Min_w',Min_w);

        [BLogLikeF,BSigma_F,BSigma_phi]=TimeDelay.logl_F([InPar.Tau, Res.BestPar_H1],'FitPar',[InPar.Tau, Res.BestPar_H1],...
                    'Min_w',Min_w,...
                    'w',ResLC.w,...
                    'F_w',ResLC.F_w,...
                    'sigma_F',ResLC.sigma_F_hat);
        
        
        
        [~,SI] = sort(ResLC.w);
        semilogy(ResLC.w(SI),abs(ResLC.F_w(SI)).^2)
        hold on;
        semilogy(ResLC.w(SI),Sigma_F(SI),'LineWidth',2)
        semilogy(ResLC.w(SI),BSigma_F(SI),'LineWidth',2)
        axis([0 2 1e-1 1e4])
        H=legend('$|F(w)|^{2}$','$\Sigma_{F}$','Best fit $\Sigma_{F}$');
        H.Interpreter = 'latex';
        
        H = xlabel('$\omega$ [2$\pi$/day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('Power');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_PS_.eps -depsc2
        %print Flux_Sim11_alias10_PS_.eps -depsc2
        
        %%
        
        FitPar = [NaN, NaN, NaN];
        DefPar = [InPar.A(1), InPar.A(2), InPar.Gamma];
        VecInvTau = (1./100:1./1000:1./10).';
        Res = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,...
                'Min_w',Min_w);

        figure(2);
        plot(1./Res.Tau, Res.LL_H1-Res.LL_H0,'LineWidth',2);
        hold on;
        axis([0.01 0.1 -30 2]);
        
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');
               
        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_PS_DL_Tau.eps -depsc2
        %print Flux_Sim11_alias10_PS_DL_Tau.eps -depsc2
        
    case 'a1a2'
        %% LogL as a function of a1/a2
        % using simulation 11
        InPar=select_parameters(11);
        [ResLC,ResG]=generateLC(InPar);
        
        % simulate LC
        
        
        InPar.AliasFactor = 10;
        
        
        
        FitPar = [NaN   NaN       InPar.Gamma];
        DefPar = [1     0.2      InPar.Gamma];
        VecInvTau = 1./InPar.Tau;
        Min_w = [2.*pi./200 Inf];

        VecA1   = logspace(-1,1,180);
        VecA2A1 = logspace(-2,1,120);
        Na1   = numel(VecA1);
        Na2   = numel(VecA2A1);

        LA = zeros(Na1,Na2);
        clear LA
        Pars = [InPar.A(1), 0];  % A1 A2
        FitParH0 = [0, FitPar];
        
        IsFreq = true; %false; %true;
        if IsFreq
            % frequency domain
            %[ResLC,ResG]=generateLC(InPar);
            
            [LogLikeF_H0,Sigma_F,Sigma_phi]=TimeDelay.logl_F(Pars,'FitPar',FitParH0,...
                                                                  'w',ResLC.w,...
                                                                  'F_w',ResLC.F_w,...
                                                                  'sigma_F',ResLC.sigma_F_hat);


            for Ia1=1:1:Na1
                Ia1
                for Ia2=1:1:Na2
                    A2 = VecA2A1(Ia2).*VecA1(Ia1);
                    
                    Pars = [VecA1(Ia1), A2];
                    FitParH1 = [InPar.Tau, FitPar];
                    
                    [LogLikeF_H1,Sigma_F,Sigma_phi]=TimeDelay.logl_F(Pars,'FitPar',FitParH1,...
                                                                  'w',ResLC.w,...
                                                                  'F_w',ResLC.F_w,...
                                                                  'sigma_F',ResLC.sigma_F_hat);

                    LA(Ia1,Ia2) = LogLikeF_H1 - LogLikeF_H0;
                end
            end
        else
            % time domain
            InPar.EndMatching = false;
            [ResLC,ResG]=generateLC(InPar);
            
            [LogLikeF_H0]=TimeDelay.logl_F(Pars,'FitPar',FitParH0,...
                                                                  't',ResLC.T,...
                                                                  'F_t',ResLC.F_t,...
                                                                  'sigma_F',ResLC.sigma_F_hat);

            Tau   = InPar.Tau;
            gamma = InPar.Gamma;
            N     = numel(ResLC.T);
            DT = (0:1:N-1);
            
            FunG1  = @(Omega) (cos(Omega.*DT)  - 1).*Omega.^(-gamma);
            FunG1t = @(Omega) (cos(Omega.*Tau) - 1).*Omega.^(-gamma);
            FunG2  = @(Omega) (cos(Omega.*DT)  - 1).*cos(Omega.*Tau).*Omega.^(-gamma);

            G1  = 2.*integral(FunG1,0,Inf,'ArrayValued',true);
            G1t = 2.*integral(FunG1t,0,Inf,'ArrayValued',true);
            G2  = 2.*integral(FunG2,0,Inf,'ArrayValued',true);

            for Ia1=1:1:Na1
                Ia1
                for Ia2=1:1:Na2
                    A2 = VecA2A1(Ia2).*VecA1(Ia1);
                    
                    Pars = [VecA1(Ia1), A2];
                    FitParH1 = [InPar.Tau, FitPar];
                    
                    [LogLikeF_H1]=TimeDelay.logl_F(Pars,'FitPar',FitParH1,...
                                                                  't',ResLC.T,...
                                                                  'F_t',ResLC.F_t,...
                                                                  'sigma_F',ResLC.sigma_F_hat,...
                                                                  'G1',G1,...
                                                                  'G1t',G1t',...
                                                                  'G2',G2);

                    LA(Ia1,Ia2) = LogLikeF_H1 - LogLikeF_H0;
                end
            end
            
            
        end
        %%
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 4;
        Level = 0.5.*chi2inv(GaussProb,Npar);
        Min   = min(LA(:));
        contour(VecA1,VecA2A1,LA.',Min+Level)
        set(gca,'XS','log','YS','log')
        H=colorbar;
        H.Label.String='$\Delta{\ln\mathcal{L}}$(F)';
        H.Label.Interpreter = 'latex';
        axis([0.1 3 1e-1 1e1])
        H=xlabel('$\alpha_1$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H=ylabel('$\alpha_2$/$\alpha_1$');
        H.FontSize = 18;
        H.Interpreter = 'latex';

        hold on;
        plot(InPar.A(1),InPar.A(2)./InPar.A(1),'+','MarkerSize',10,'LineWidth',3);
        %(InPar.A(2),InPar.A(1)./InPar.A(2),'+','MarkerSize',10);

        print Flux_Sim11_a1_a1a2.eps -depsc2

       % if InPar.AliasFactor==1
       %     print Flux_Sim11_a1_a1a2.eps -depsc2
       % elseif InPar.AliasFactor==10
       %     print Flux_Sim11_alias10_a1_a1a2.eps -depsc2
       % else
       %     error('Unknown alias factor');
       % end
       
    
       
    case 'compare_ft_methods'
        InPar=select_parameters(11);
        
        InPar.AliasFactor = 10;
        InPar.EndMatching = false;
        

        VecInvTau = (1./50:1./1000:1./10);
        
        DefPar = [1     0.5              2];
        FitPar = [NaN   NaN              2];
        
        [ResLC,ResG]=generateLC(InPar);
            
        Isim = 1;
        % G0=0
        tic;
        Res1(Isim)=TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',true,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar);
        toc
        
        % with w0=1/1000
        tic;
        Res2(Isim)=TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',true,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,'w0',1./1000);
        toc
        % with w0=1/10000
        tic;
        Res3(Isim)=TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',true,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,'w0',1./10000);
        toc
    
        plot(1./Res1.Tau,Res1.LL_H1-Res1.LL_H0,'LineWidth',2)
        hold on;
        plot(1./Res2.Tau,Res2.LL_H1-Res2.LL_H0,'LineWidth',2)
        plot(1./Res3.Tau,Res3.LL_H1-Res3.LL_H0,'LineWidth',2)
        
        H=legend('$G_{0}=0$','$w_{0}=10^{-3}$','$w_{0}=10^{-4}$','Location','SouthEast','AutoUpdate','off');
        H.Interpreter = 'latex'
        axis([0.02 0.1 -18 8])
        
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');
           
        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_G0_treatment.eps -depsc2
        
    case 'test_ft'
        %% test time domain solution
        InPar=select_parameters(11);
        
        InPar.AliasFactor = 10;
        InPar.EndMatching = false;
        

        VecInvTau = (1./50:1./100:1./10);
        
        DefPar = [1     0.5              2];
        FitPar = [NaN   NaN              2];
        
        for Isim=1:1:Nsim
            [ResLC,ResG]=generateLC(InPar);
            
            tic;
            Res(Isim)=TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',true,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar);
            toc
        end
        
        InPar.EndMatching = true;
        for Isim=1:1:Nsim
            [ResLC,ResG]=generateLC(InPar);
            
            tic;
            ResFS(Isim)=TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',false,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar);
            toc
        end
        
        'a'
        save Flux_Sim11_TD.mat Res ResFS InPar VecInvTau
        
        AllDL = zeros(Nsim,numel(Res.Tau));
        for Isim=1:1:Nsim
            AllDL(Isim,:) = [Res(Isim).LL_H1.' - Res(Isim).LL_H0];
            AllDLfs(Isim,:) = [ResFS(Isim).LL_H1.' - ResFS(Isim).LL_H0];
        end
        
        X = [1./Res(1).Tau];
        X = [X;flipud(X)];
        Y = [quantile(AllDL,0.15866).';flipud(quantile(AllDL,0.84134).')];
        H=patch(X,Y,[0.7 0.7 0.7],'EdgeColor',[0.7 0.7 0.7])
        % plot.patch_band(1./Res(1).Tau,mean(AllDL).',std(AllDL).')
        hold on;
        plot(1./Res(1).Tau,mean(AllDL),'k-','LineWidth',2)
        %plot.patch_band(1./Res(1).Tau,mean(AllDLfs).',std(AllDLfs).')
        plot(1./Res(1).Tau,mean(AllDLfs),'LineWidth',2)
        plot(1./Res(1).Tau, (mean(AllDLfs) - median(mean(AllDLfs))) ./median(std(AllDLfs)./std(AllDL))  ,'LineWidth',2)
        
        
        legend('Time Domain StD','Time Domain Mean','Fourier Space Mean','Fourier Space calibrated','Location','SouthEast','AutoUpdate','off')
        
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');
                
        axis([0.02 0.1 -28 5])
        box on;
        
        
        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_TD.eps -depsc2
        
    case 'test_ft_a2'
        % time domain with A2/A1 sensitivity
        
        %% test time domain solution
        InPar=select_parameters(11);
        InPar.AliasFactor = 10;
        InPar.EndMatching = false;
        InPar.A(2) = 0.3;

        VecInvTau = (1./50:1./1000:1./10);
        
        DefPar = [1     InPar.A(2)       2];
        FitPar = [NaN   NaN              2];
        
        for Isim=1:1:Nsim
            [ResLC,ResG]=generateLC(InPar);
            [Isim,1]
            tic;
            Res1(Isim)=TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',true,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar);
            toc
        end
        
        %
        InPar.A(2) = 0.1;
        
        DefPar = [1     InPar.A(2)       2];
        FitPar = [NaN   NaN              2];
        
        for Isim=1:1:Nsim
            [ResLC,ResG]=generateLC(InPar);
            [Isim,2]
            tic;
            Res2(Isim)=TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',true,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar);
            toc
        end
        
        %
        InPar.A(2) = 0.03;
        
        DefPar = [1     InPar.A(2)       2];
        FitPar = [NaN   NaN              2];
        
        for Isim=1:1:Nsim
            [ResLC,ResG]=generateLC(InPar);
            [Isim,3]
            tic;
            Res3(Isim)=TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',true,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar);
            toc
        end
        
        %
        save Flux_Sim11_TD_A2.mat
        'a'
        
        AllDL1 = zeros(Nsim,numel(Res1(1).Tau));
        AllDL2 = zeros(Nsim,numel(Res2(1).Tau));
        AllDL3 = zeros(Nsim,numel(Res3(1).Tau));
        
        for Isim=1:1:Nsim
            AllDL1(Isim,:) = Res1(Isim).LL_H1.' - Res1(Isim).LL_H0;
            AllDL2(Isim,:) = Res2(Isim).LL_H1.' - Res2(Isim).LL_H0;
            AllDL3(Isim,:) = Res3(Isim).LL_H1.' - Res3(Isim).LL_H0;
        end
        
        X = [1./Res1(1).Tau];
        X = [X;flipud(X)];
        Y = [quantile(AllDL1,0.15866).';flipud(quantile(AllDL1,0.84134).')];
        H=patch(X,Y,[0.7 0.7 0.7],'EdgeColor',[0.7 0.7 0.7])
        hold on;
        plot(1./Res1(1).Tau,mean(AllDL1),'k-','LineWidth',2);
        plot(1./Res1(1).Tau,mean(AllDL2),'-','LineWidth',2);
        %plot(1./Res1(1).Tau,mean(AllDL3),'-','LineWidth',2);
        H=legend('Std $\alpha_{2}/\alpha_{1}=0.3$','Mean $\alpha_{2}/\alpha_{1}=0.3$','Mean $\alpha_{2}/\alpha_{1}=0.1$','AutoUpdate','off','Location','SouthEast')
        H.Interpreter = 'latex';
        
        %plot(1./Res1(1).Tau,quantile(AllDL1,0.15866),'LineWidth',1);
        %plot(1./Res1(1).Tau,quantile(AllDL1,0.84134),'LineWidth',1);
        box on
        axis([0.02 0.1 -14 1]);
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
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
        
        print Flux_Sim11_TD_A2.eps -depsc2
        
    case 'time_h0'
        %% simulations for H0
        % using simulation 11
        InPar=select_parameters(11);
        InPar.EndMatching = false;
        
        tic;
        %Nsim = 1000;
        clear Res;

        InPar.A(2) = 0;
        InPar.Tau  = 0;

        Ng = 1;
        for Ig=1:1:Ng


            FitPar = [NaN         NaN          InPar.Gamma];
            DefPar = [InPar.A(1)  InPar.A(2)   InPar.Gamma];
            VecInvTau = (1./50:1./1000:1./10);
            
            for Isim=1:1:Nsim
                [Ig, Isim, Nsim]
                % simulate LC
                try
                    [ResLC,ResG]=generateLC(InPar);
                catch
                    [ResLC,ResG]=generateLC(InPar);
                end
                
                Res(Ig,Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',true,...
                                                            'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar);
                

            end

        end
        toc
        
        save -v7.3 Res_Sim11_TD_H0.mat Res InPar
        
        %%
        
        Nsim = 100;
        for Isim=1:1:Nsim
            AllDL(Isim,:) = [Res(Isim).LL_H1 - Res(Isim).LL_H0].';
            MinDL(Isim)   = min([Res(Isim).LL_H1 - Res(Isim).LL_H0].');
        end
        MeanDL = mean(AllDL);
        StdDL  = std(AllDL);
        %StdDL  = imUtil.background.rstd(AllDL);
        
        
        plot.patch_band(1./Res(1).Tau,MeanDL(:),StdDL(:));
        hold on;
        plot(1./Res(1).Tau,mean(AllDL),'k-','LineWidth',2)
        axis([0.02 0.1 -10 150]);
        box on;
        
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');
        
        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_TD_H0_DL_Tau.eps -depsc2
        
        %%
        %histogram(AllDL(:),[-10:1:50'],'Normalization','cdf')
        histogram(MinDL(:),[-10:1:0'],'Normalization','cdf')
        
        hold on;
        axis([-10 0 0 1])
       
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot(-Level(1).*ones(1,2),[0 1],'k--');
        plot(-Level(2).*ones(1,2),[0 1],'k--');
        plot(-Level(3).*ones(1,2),[0 1],'k--');

        H = ylabel('Cumulative fraction');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = xlabel('min($\Delta{\ln\mathcal{L}}$(F))');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_TD_H0_histMin.eps -depsc2
        
        
        
        
    case 'fitgamma'
        %% fitting gamma
        % using simulation 11
        
        InPar=select_parameters(11);
        
        InPar.AliasFactor = 1;
        InPar.EndMatching = false;
        
        tic;
        %Nsim = 1000;
        clear Res;
        

        FitPar = [NaN   NaN       NaN];
        DefPar = [1     0.5      InPar.Gamma];
        VecInvTau = (1./100:1./1000:1./10).';

        for Isim=1:1:Nsim
            [Isim, Nsim]
            % simulate LC
            [ResLC,ResG]=generateLC(InPar);

            Res(Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,...
                'Min_w',[2.*pi./5000 2.*pi./0.01]);

        end

        toc
        
        save -v7.3 Res_Sim11_NoEndMatching_Alias1_fitgamma.mat Res InPar
        
        %%
        
        InPar.EndMatching = false;
        
        tic;
        %Nsim = 1000;
        clear Res;
        

        FitPar = [NaN   NaN       NaN];
        DefPar = [1     0.5      InPar.Gamma];
        VecInvTau = (1./100:1./1000:1./10).';

        for Isim=1:1:Nsim
            [Isim, Nsim]
            % simulate LC
            [ResLC,ResG]=generateLC(InPar);

            Res(Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,...
                'Min_w',[2.*pi./5000 2.*pi./0.01]);

        end

        toc
        
        save -v7.3 Res_Sim11_NoEndMatching_fitgamma.mat Res InPar
       
        %%
        InPar.EndMatching = true;

        tic;
        %Nsim = 1000;
        clear Res;
        

        FitPar = [NaN   NaN       NaN];
        DefPar = [1     0.5      InPar.Gamma];
        VecInvTau = (1./100:1./1000:1./10).';

        for Isim=1:1:Nsim
            [Isim, Nsim]
            % simulate LC
            [ResLC,ResG]=generateLC(InPar);

            Res(Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,...
                'Min_w',[2.*pi./5000 2.*pi./0.01]);

        end

        toc
        
        save -v7.3 Res_Sim11_fitgamma.mat Res InPar
        
        
        
        %%
        
        load Res_Sim11_NoEndMatching_Alias1_fitgamma.mat
        Nsim = numel(Res);
        
        [~,MinI] = min(abs(Res(1).Tau-InPar.Tau));
        AllDL = zeros(Nsim,numel(Res(1).Tau));
        BestPars = zeros(Nsim,3);
        BestDL   = zeros(Nsim,1);
        for I=1:1:Nsim
            AllDL(I,:) = [Res(I).LL_H1 - Res(I).LL_H0]';
            
            BestPars_NEM_Alias1(I,:) = Res(I).BestPar_H1(MinI,:);
            BestDL(I,1)   = Res(I).LL_H1(MinI) - Res(I).LL_H0;
        end
        Mean_NEM_Alias1 = nanmedian(AllDL);
        Std_NEM_Alias1  = nanstd(AllDL);
        %Std_NEM_Alias1  = imUtil.background.rstd(AllDL,1);

        %
        load Res_Sim11_NoEndMatching_fitgamma.mat
        Nsim = numel(Res);
        
        [~,MinI] = min(abs(Res(1).Tau-InPar.Tau));
        AllDL = zeros(Nsim,numel(Res(1).Tau));
        BestPars = zeros(Nsim,3);
        BestDL   = zeros(Nsim,1);
        for I=1:1:Nsim
            AllDL(I,:) = [Res(I).LL_H1 - Res(I).LL_H0]';
            
            BestPars_NEM_Alias10(I,:) = Res(I).BestPar_H1(MinI,:);
            BestDL(I,1)   = Res(I).LL_H1(MinI) - Res(I).LL_H0;
        end
        Mean_NEM_Alias10 = nanmedian(AllDL);
        Std_NEM_Alias10  = nanstd(AllDL);
        %Std_NEM_Alias10  = imUtil.background.rstd(AllDL,1);

        %
        load Res_Sim11_fitgamma.mat
        Nsim = numel(Res);
        
        [~,MinI] = min(abs(Res(1).Tau-InPar.Tau));
        AllDL = zeros(Nsim,numel(Res(1).Tau));
        BestPars = zeros(Nsim,3);
        BestDL   = zeros(Nsim,1);
        for I=1:1:Nsim
            AllDL(I,:) = [Res(I).LL_H1 - Res(I).LL_H0]';
            
            BestPars_EM_Alias10(I,:) = Res(I).BestPar_H1(MinI,:);
            BestDL(I,1)   = Res(I).LL_H1(MinI) - Res(I).LL_H0;
        end
        Mean_EM_Alias10 = nanmedian(AllDL);
        Std_EM_Alias10  = nanstd(AllDL);
        %Std_EM_Alias10  = imUtil.background.rstd(AllDL,1);

        %
        
               
        plot.patch_band(1./Res(1,1).Tau,Mean_EM_Alias10(:),Std_EM_Alias10(:))
        hold on;
        plot(1./Res(1,1).Tau,Mean_EM_Alias10(:),'LineWidth',2)
        plot(1./Res(1,1).Tau,Mean_NEM_Alias10(:),'LineWidth',2)
        plot(1./Res(1,1).Tau,Mean_NEM_Alias1(:),'LineWidth',2)
                
        legend('Std','Mean (EM/Alias10)','Mean (Alias10)','Mean (Alias1)','AutoUpdate','off','Location','SouthEast')
        
        
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');
                
        axis([0.01 0.1 -20 2])
        box on;
        
        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_fitgamma_wEM_DL_Tau.eps -depsc2
        
        %%
        figure(2)
        plot(BestPars_EM_Alias10(:,2)./BestPars_EM_Alias10(:,1),BestPars_EM_Alias10(:,3),'.')
        hold on;
        plot(BestPars_NEM_Alias10(:,2)./BestPars_NEM_Alias10(:,1),BestPars_NEM_Alias10(:,3),'.')
        plot(BestPars_NEM_Alias1(:,2)./BestPars_NEM_Alias1(:,1),BestPars_NEM_Alias1(:,3),'.')
        
        legend('EM/Alias10','Alias10','Alias1','AutoUpdate','off','Location','SouthEast')
        
        plot(InPar.A(2)./InPar.A(1),InPar.Gamma,'k+','MarkerSize',14,'LineWidth',3)
        axis([0 1 1.6 2.4])
        H = xlabel('$\alpha_{2}$/$\alpha_{1}$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\gamma$');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        plot.hist_ofplot
        
        print Flux_Sim11_fitgamma_wEM_A2A1_gamma.eps -depsc2
        
    case 'vargamma'         
        %% fitting as a function of gamma
        % using simulation 11
        InPar=select_parameters(11);
        
        tic;
        %Nsim = 1000;
        clear Res;

        
        gammaVec = [1.8 2 2.5 3 3.5];
        Ng = numel(gammaVec);
        for Ig=1:1:Ng
            InPar.Gamma = gammaVec(Ig);

            FitPar = [NaN   NaN       InPar.Gamma];
            DefPar = [1     0.5      InPar.Gamma];
            VecInvTau = (1./100:1./1000:1./10).';

            for Isim=1:1:Nsim
                [Ig, Isim, Nsim]
                % simulate LC
                try
                    [ResLC,ResG]=generateLC(InPar);
                catch
                    try
                        [ResLC,ResG]=generateLC(InPar);
                    catch
                        [ResLC,ResG]=generateLC(InPar);
                    end
                end
                
                Res(Ig,Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,...
                    'Min_w',[2.*pi./5000 2.*pi./0.01]);

            end

        end
        toc

        save -v7.3 Res_Sim11_EM_Alias10_vargamma.mat Res gammaVec InPar

        %%
        AllDL = zeros(Nsim,numel(Res(1).Tau));
        for Ig=1:1:Ng
            for I=1:1:Nsim
                AllDL(I,:) = [Res(Ig,I).LL_H1-Res(Ig,I).LL_H0]';

                %plot(Res(I).Tau,Res(I).LL_H1-Res(I).LL_H0);
                %hold on;
            end
            Mean{Ig} = nanmedian(AllDL);
            Std{Ig}  = nanstd(AllDL);

            plot(1./Res(1,1).Tau,Mean{Ig},'LineWidth',2)
            hold on;
        end
        H=legend('$\gamma=1.8$','$\gamma=2.0$','$\gamma=2.5$','$\gamma=3.0$','$\gamma=3.5$','AutoUpdate','off','Location','SouthEast')
        H.Interpreter = 'latex';
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');

        axis([0.01 0.1 -33 2])

        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_EM_Alias10_vargamma_DL_Tau.eps -depsc2
        
        %%    
        %AllDLN = AllDL;
        %AllDLN(AllDL>0) = NaN;
        %plot(Res(1).Tau,mean(AllDL),'k-','LineWidth',2)
        %hold on
        %plot(Res(1).Tau,nanmean(AllDLN),'k-','LineWidth',3)

        
    case 'wronggamma'            
        %% fitting with the wrong gamma
        % using simulation 11
        InPar=select_parameters(11);
        
        tic;
        %Nsim = 1000;
        clear Res;

        Min_w = [2.*pi./5000 2.*pi./0.01];
        %Min_w = [2.*pi./200 2.*pi./10];

        gammaVec = [1.5 2 2.5 3 3.5];
        Ng = numel(gammaVec);
        for Ig=1:1:Ng

            FitPar = [NaN         NaN           gammaVec(Ig)];
            DefPar = [InPar.A(1)  InPar.A(2)    gammaVec(Ig)];
            VecInvTau = (1./100:1./1000:1./10).';

            for Isim=1:1:Nsim
                [Ig, Isim, Nsim]
                % simulate LC
                try
                    [ResLC,ResG]=generateLC(InPar);
                catch
                    [ResLC,ResG]=generateLC(InPar);
                end
                
                Res(Ig,Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,'Min_w',Min_w);

            end

        end
        toc

        save -v7.3 Res_Sim11_EM_Alias10_wronggamma.mat Res gammaVec InPar

        %%
        AllDL = zeros(Nsim,numel(Res(1).Tau));
        for Ig=1:1:Ng
            for I=1:1:Nsim
                AllDL(I,:) = [Res(Ig,I).LL_H1 - Res(Ig,I).LL_H0]';
    
                %plot(Res(I).Tau,Res(I).LL_H1-Res(I).LL_H0);
                %hold on;
            end
            Mean{Ig} = nanmedian(AllDL);
            Std{Ig}  = nanstd(AllDL);

            plot(1./Res(1,1).Tau,Mean{Ig},'LineWidth',2)
            hold on;
        end
        H=legend('$\gamma=1.5$','$\gamma=2.0$','$\gamma=2.5$','$\gamma=3.0$','$\gamma=3.5$','AutoUpdate','off','Location','SouthEast')
        H.Interpreter = 'latex';
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');

        axis([1./100 1./10 -35 2])

        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_EM_Alias10_wronggamma_DL_Tau.eps -depsc2
        
        %%    
        %AllDLN = AllDL;
        %AllDLN(AllDL>0) = NaN;
        %plot(Res(1).Tau,mean(AllDL),'k-','LineWidth',2)
        %hold on
        %plot(Res(1).Tau,nanmean(AllDLN),'k-','LineWidth',3)



    case 'varsigmaf'
        
        %% simulations as a function of sigma_F
        % using simulation 11
        InPar=select_parameters(11);
        tic;

        %Nsim = 1000;
        clear Res;

        sigmaFVec = [0.001 0.003 0.01, 0.03, 0.05];
        Ng = numel(sigmaFVec);
        for Ig=1:1:Ng
            InPar.sigma_F_rel = sigmaFVec(Ig).*sum(InPar.A);


            FitPar = [NaN         NaN             InPar.Gamma];
            DefPar = [InPar.A(1)  InPar.A(2)      InPar.Gamma];
            VecInvTau = (1./100:1./1000:1./10).';

            for Isim=1:1:Nsim
                [Ig, Isim, Nsim]
                % simulate LC
                try
                    [ResLC,ResG]=generateLC(InPar);
                catch
                    try
                        [ResLC,ResG]=generateLC(InPar);
                    catch 
                        [ResLC,ResG]=generateLC(InPar);
                    end
                end
                
                Res(Ig,Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,'Min_w',[2.*pi./5000 2.*pi./0.01]);

            end

        end
        toc
        
        save -v7.3 Res_Sim11_EM_Alias10_varsigmaf.mat Res sigmaFVec InPar

        %%

        AllDL = zeros(Nsim,numel(Res(1).Tau));
        for Ig=2:1:Ng
            for I=1:1:Nsim
                AllDL(I,:) = [Res(Ig,I).LL_H1-Res(Ig,I).LL_H0]';

            end
            Mean{Ig} = nanmedian(AllDL);
            Std{Ig}  = nanstd(AllDL);

            plot(1./Res(1,1).Tau,Mean{Ig},'LineWidth',2)
            hold on;
        end
        H = legend('$\sigma{F}/\langle{F}\rangle=0.003$','$\sigma{F}/\langle{F}\rangle=0.01$','$\sigma{F}/\langle{F}\rangle=0.03$','$\sigma{F}/\langle{F}\rangle=0.05$','AutoUpdate','off','Location','SouthEast')
        H.Interpreter = 'latex';

        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');

        axis([1./100 1./10 -90 2])

        
        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_EM_Alias10_varsigmaf_DL_Tau.eps -depsc2
        
    case 'vara2'
        %% simulations as a function of A2
        % using simulation 11
        InPar=select_parameters(11);
        
        tic;

        %Nsim = 1000;
        clear Res;

        A2Vec = [0.01 0.03 0.1 0.3 0.5];
        Ng = numel(A2Vec);
        for Ig=1:1:Ng
            InPar.A(2) = A2Vec(Ig);


            FitPar = [NaN          NaN         InPar.Gamma];
            DefPar = [InPar.A(1)   InPar.A(2)  InPar.Gamma];
            VecInvTau = (1./100:1./1000:1./10).';

            for Isim=1:1:Nsim
                [Ig, Isim, Nsim]
                % simulate LC
                [ResLC,ResG]=generateLC(InPar);
                
                Res(Ig,Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,'Min_w',[2.*pi./5000 2.*pi./0.01]);

            end

        end
        toc

        save -v7.3 Res_Sim11_EM_Alias10_vara2.mat Res A2Vec InPar

        %%
        AllDL = zeros(Nsim,numel(Res(1).Tau));
        for Ig=2:1:Ng
            for I=1:1:Nsim
                AllDL(I,:) = [Res(Ig,I).LL_H1-Res(Ig,I).LL_H0]';

                %plot(Res(I).Tau,Res(I).LL_H1-Res(I).LL_H0);
                %hold on;
            end
            Mean{Ig} = nanmedian(AllDL);
            Std{Ig}  = nanstd(AllDL);

            plot(1./Res(1,1).Tau,Mean{Ig},'LineWidth',2)
            hold on;
        end
        H=legend('$\alpha_{2}/\alpha_{1}=0.03$','$\alpha_{2}/\alpha_{1}=0.1$','$\alpha_{2}/\alpha_{1}=0.3$','$\alpha_{2}/\alpha_{1}=0.5$','AutoUpdate','off','Location','SouthEast')
        H.Interpreter = 'latex';
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');

        axis([1./100 1./10 -20 2])

        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_EM_Alias10_vara2_DL_Tau.eps -depsc2
        
    case 'vart'
        %% simulations as a function of Time series length
        % using simulation 11
        InPar=select_parameters(11);
        
        tic;
        %Nsim = 1000;
        clear Res;

        TVec = [100 300 1000 3000];
        Ng = numel(TVec);
        for Ig=1:1:Ng
            InPar.Time = (1:1:TVec(Ig)).';


            FitPar = [NaN         NaN          InPar.Gamma];
            DefPar = [InPar.A(1)  InPar.A(2)   InPar.Gamma];
            VecInvTau = (1./100:1./1000:1./10).';

            for Isim=1:1:Nsim
                [Ig, Isim, Nsim]
                % simulate LC
                [ResLC,ResG]=generateLC(InPar);

                Res(Ig,Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,'Min_w',[2.*pi./5000 2.*pi./0.01]);

            end

        end
        toc
        
        save -v7.3 Res_Sim11_vart.mat Res TVec InPar

        %%
        
        AllDL = zeros(Nsim,numel(Res(1).Tau));
        for Ig=1:1:Ng
            for I=1:1:Nsim
                AllDL(I,:) = [Res(Ig,I).LL_H1 - Res(Ig,I).LL_H0]';
    
                %plot(Res(I).Tau,Res(I).LL_H1-Res(I).LL_H0);
                %hold on;
            end
            Mean{Ig} = nanmedian(AllDL);
            Std{Ig}  = nanstd(AllDL);

            plot(1./Res(1,1).Tau,Mean{Ig},'LineWidth',2)
            hold on;
        end
        legend('100','300','1000','3000','AutoUpdate','off')

        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');

        axis([1./100 1./10 -15 5])

        
    case 'varstd'
        %% simulations as a function of std/mean of flux
        % using simulation 11
        InPar=select_parameters(11);
        
        tic;
        %Nsim = 1000;
        clear Res;

        Amp = [0.04 0.08; 0.1 0.14; 0.3 0.40];
        
        Ng = size(Amp,1);
        for Ig=1:1:Ng
            InPar.StdMeanRange = Amp(Ig,:);
            

            FitPar = [NaN         NaN          InPar.Gamma];
            DefPar = [InPar.A(1)  InPar.A(2)   InPar.Gamma];
            VecInvTau = (1./100:1./1000:1./10).';

            for Isim=1:1:Nsim
                [Ig, Isim, Nsim]
                % simulate LC
                [ResLC,ResG]=generateLC(InPar);

                Res(Ig,Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,'Min_w',[2.*pi./5000 2.*pi./0.01]);

            end

        end
        toc
        
        save -v7.3 Res_Sim11_EM_Alias10_varstd.mat Res Amp InPar

        %%
        
        AllDL = zeros(Nsim,numel(Res(1).Tau));
        for Ig=1:1:Ng
            for I=1:1:Nsim
                AllDL(I,:) = [Res(Ig,I).LL_H1 - Res(Ig,I).LL_H0]';
    
                %plot(Res(I).Tau,Res(I).LL_H1-Res(I).LL_H0);
                %hold on;
            end
            Mean{Ig} = nanmedian(AllDL);
            Std{Ig}  = nanstd(AllDL);

            plot(1./Res(1,1).Tau,Mean{Ig},'LineWidth',2)
            hold on;
        end
        H=legend('std(F)$/\langle{F}\rangle=$0.04-0.08','std(F)$/\langle{F}\rangle=$0.10-0.14','std(F)$/\langle{F}\rangle=$0.30-0.40','AutoUpdate','off','Location','SouthEast')
        H.Interpreter = 'latex';

        [ResLC,ResG]=generateLC(InPar);

        axis([1./100 1./10 -60 2])
        
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');

        
        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_EM_Alias10_varstd_DL_Tau.eps -depsc2
        
        
    case 'h0'
        %% simulations for H0
        % using simulation 11
        InPar=select_parameters(11);
        
        tic;
        %Nsim = 1000;
        clear Res;

        InPar.A(2) = 0;
        InPar.Tau  = 0;

        Ng = 1;
        for Ig=1:1:Ng


            FitPar = [NaN         NaN          InPar.Gamma];
            DefPar = [InPar.A(1)  InPar.A(2)   InPar.Gamma];
            VecInvTau = (1./100:1./1000:1./10).';

            for Isim=1:1:Nsim
                [Ig, Isim, Nsim]
                % simulate LC
                try
                    [ResLC,ResG]=generateLC(InPar);
                catch
                    [ResLC,ResG]=generateLC(InPar);
                end
                
                Res(Ig,Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,'Min_w',[2.*pi./5000 2.*pi./0.01]);

            end

        end
        toc
        
        save -v7.3 Res_Sim11_EM_Alias10_H0.mat Res InPar
        
        %%
        
        for Isim=1:1:Nsim
            AllDL(Isim,:) = [Res(Isim).LL_H1 - Res(Isim).LL_H0].';
            MinDL(Isim)   = min([Res(Isim).LL_H1 - Res(Isim).LL_H0].');
        end
        MeanDL = mean(AllDL);
        StdDL  = std(AllDL);
        %StdDL  = imUtil.background.rstd(AllDL);
        
        
        plot.patch_band(1./Res(1).Tau,MeanDL(:),StdDL(:));
        hold on;
        plot(1./Res(1).Tau,mean(AllDL),'k-','LineWidth',2)
        axis([0.01 0.1 -4 4]);
        box on;
        
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');
        
        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_EM_Alias10_H0_DL_Tau.eps -depsc2
        
        %%
        %histogram(AllDL(:),[-10:1:50'],'Normalization','cdf')
        histogram(MinDL(:),[-10:1:0'],'Normalization','cdf')
        
        hold on;
        axis([-9 0 0 1])
       
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot(-Level(1).*ones(1,2),[0 1],'k--');
        plot(-Level(2).*ones(1,2),[0 1],'k--');
        plot(-Level(3).*ones(1,2),[0 1],'k--');

        H = ylabel('Cumulative fraction');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = xlabel('min($\Delta{\ln\mathcal{L}}$(F))');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        print Flux_Sim11_EM_Alias10_H0_histMin.eps -depsc2
       
        
    case 'td_uneq'
        % time domain fit - unequally spaced simulations
        
        InPar=select_parameters(11);
        InPar.EndMatching = false;
        
        InPar.Tau = 18;

        Min_w = [2.*pi./5000 2.*pi./0.01];
        %Min_w = 2.*pi./[200 10];
        
        FitPar = [NaN         NaN          InPar.Gamma];
        DefPar = [InPar.A(1)  InPar.A(2)   InPar.Gamma];
        VecInvTau = (1./50:1./1000:1./10).';

        Nsim=30
        for Isim=16:1:Nsim
            %[ResLC]=generateLC(InPar);
            [ResLC,Section,PS]=generateLCuneq(InPar);
            ResLC
            % interpolate
            %TT = (2:1:900).';
            %ResLC.F_t = interp1(ResLC.T,ResLC.F_t,TT,'pchip');
            %ResLC.T   = TT;
            tic;
            Res(Isim) = TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',true,...
                                                            'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar);
                
            toc
        end
        
        
        
        'a'
        save -v7.3 Flux_Sim11_Tau18_unevenly_TD.mat
        
        AllDL = zeros(Nsim,numel(Res(1).Tau));
        for Isim=1:1:Nsim
            AllDL(Isim,:) = Res(Isim).LL_H1.' - Res(Isim).LL_H0;
        end
        
        QL = quantile(AllDL,0.15);
        QU = quantile(AllDL,0.85);
        H=patch(1./[Res(1).Tau(:);flipud(Res(1).Tau(:))],[QL(:);flipud(QU(:))],[0.8 0.8 0.8]);
        H.EdgeColor= [0.8 0.8 0.8];
        hold on;
        plot(1./Res(1).Tau,median(AllDL),'k-','LineWidth',2);
        hold on;
        %plot(1./Res(1).Tau,quantile(AllDL,0.15),'-','Color',[0.8 0.8 0.8],'LineWidth',2)
        %plot(1./Res(1).Tau,quantile(AllDL,0.85),'-','Color',[0.8 0.8 0.8],'LineWidth',2)
                
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
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
        plot(1./InPar.Tau.*ones(1,2),[-15 5],'r-','LineWidth',1)
        axis([0.02 0.1 -14 3])
        box on;
        
        print Flux_Sim11_Tau18_unevenly_TD.eps -depsc2
        
        
        
    case 'uneq'
        %% unequally space simulatins
        % using simulation 11
        InPar=select_parameters(11);
        InPar.AliasFactor = 1;

        Min_w = [2.*pi./5000 2.*pi./0.01];
        %Min_w = 2.*pi./[200 10];
        
        FitPar = [NaN         NaN          InPar.Gamma];
        DefPar = [InPar.A(1)  InPar.A(2)   InPar.Gamma];
        VecInvTau = (1./100:1./1000:1./10).';

        
        for Isim=1:1:Nsim
            Isim
            [ResLC,Section,PS]=generateLCuneq(InPar);
            Nsec = numel(Section);
%             for Isec=1:1:Nsec
%                 NTsec(Isec) = numel(Section(Isec).Time);
%             end
%             MinNT = min(NTsec);
            for Isec=1:1:Nsec
                ResA(Isec,Isim) = TimeDelay.fit_flux(Section(Isec).OmegaVec(:),Section(Isec).FT,Section(Isec).Err(1),'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,...
                                    'Min_w',Min_w,...
                                    'InputFT',true);
                                
            end
            
%                 Time = Section(Isec).Time(1:1:MinNT);
%                 Flux = Section(Isec).Flux(1:1:MinNT);
%                 Err  = Section(1).Err(1);

                
           ResC(Isim) = TimeDelay.fit_flux(PS.OmegaVec(:),PS.FT,Section(1).Err(1),'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,...
                                    'Min_w',Min_w,...
                                    'InputFT',true);
                
            
        end
        Nseason = numel(Section);
        
        
        save -v7.3 Res_Sim11_EM_Alias10_uneq.mat ResC ResA InPar
        
        %MeanDLL = mean([ResC.DLL].');
        %StdDLL  = std([ResC.DLL].');
        %plot(ResC(1).Tau,MeanDLL)
        
        %%
        
        MeanDLL = [];
        for Isim=1:1:Nsim
            DLL = [];
            DLLH0 = [];
            for Isec=1:1:size(ResA,1)
                DLL(Isec,:) = [ResA(Isec,Isim).LL_H1 - ResA(Isec,Isim).LL_H0].';
                %DLLH0(Isec,:) = [ResA(Isec,Isim).LL_H0].';
            end
            SumDLL(Isim,:) = sum(DLL,1); % - mean(DLLH0,1);
        end
        MeanDLL =  mean(SumDLL,1);
        StdDLL  = imUtil.background.rstd(SumDLL,1);
        plot.patch_band(1./ResA(1,1).Tau(:),MeanDLL(:),StdDLL(:))
        hold on;
        plot(1./ResA(1,1).Tau',mean(SumDLL),'LineWidth',2)
        box on;
        
        
        
        GaussProb = 1-2.*normcdf([1:1:5],0,1,'upper');
        Npar = 2;  % Tau, Alpha2
        Level = 0.5.*chi2inv(GaussProb,Npar);

        plot([0 1],-Level(1).*ones(1,2),'k--');
        plot([0 1],-Level(2).*ones(1,2),'k--');
        plot([0 1],-Level(3).*ones(1,2),'k--');
        axis([0.01 0.1 -8 6]);
        

        H = xlabel('1/$\tau$ [day]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('$\Delta{\ln\mathcal{L}}$(F)');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        
        print Flux_uneq_EM_DLTau.eps -depsc2
        
%         for I=1:1:Nsim
%             %plot(Res(I).Tau,Res(I).LL_H1 - Res(I).LL_H0);
%             AllDL(I,:) = [ResC(I).LL_H1 - ResC(I).LL_H0].';
%             %hold on;
%         end
%         plot(ResC(1).Tau,mean(AllDL))
        
'a'

        
    case 'uneq_ztf'
        
        [Table,Str]=VO.ZTF.wget_ztf_phot(298.0025,29.87147,1);
        JD = Table.hjd(1:280);
        JD = JD - min(JD) + 1;
        JD = [JD; JD+JD(end)+1];
        JD = JD + randn(size(JD)).*0.1;
                
        InPar=select_parameters(11);
        InPar.AliasFactor = 10;
        InPar.DeltaT = 0.1;
        InPar.Tau    = 50;
        InPar.A  = [1 0];
        
        Min_w = [2.*pi./5000 2.*pi./0.001];
        %Min_w = 2.*pi./[200 10];
        
        FitPar = [NaN         NaN          InPar.Gamma];
        DefPar = [InPar.A(1)  InPar.A(2)   InPar.Gamma];
        VecInvTau = (1./100:1./1000:1./20).';

        Nsim = 10;
        for Isim=1:1:Nsim
            Isim
            
            [ResLC,Section,PS]=generateLCuneq(InPar,JD)

            Res0(Isim)=TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat,'TimeDomain',true,'VecInvTau',VecInvTau,'FitPar',FitPar,'DefPar',DefPar,'Min_w',Min_w);
            
        end
        save -v7.3 ResAst_uneqztf_H0.mat Res0
        
        
        'a'
        
        Nsim = numel(Res1);
        for Isim=1:1:Nsim
            MinDL1(Isim) = min(Res1(Isim).LL_H1 - Res1(Isim).LL_H0);
            MinDL0(Isim) = min(Res0(Isim).LL_H1 - Res0(Isim).LL_H0);
        end
        
        for I=1:1:Nsim
            plot(1./Res0(I).Tau,Res0(I).LL_H1-Res0(I).LL_H0)
            hold on;
        end
        
        
    otherwise
        error('Unknown option');
end

end % end main fun

%%  Simulation parameters
function InPar=select_parameters(SimName)
% generate parameters for specific simulation number
% Input  : - 11, 15

    InPar.Cyclic = false;
    InPar.x0  = 0;
    InPar.y0  = 0;
    InPar.y   = [0.0  0.0];    InPar.f_dc = 50;
    InPar.DeltaT  = 1;
    InPar.StdMeanRange = [0.1 0.15];
    InPar.AliasFactor  = 10;
    InPar.EndMatching  = true;
    
    switch SimName
        case 11
            % used
            InPar.Tau = 25;
            InPar.A0  = 0;
            InPar.A   = [1 0.5];
            InPar.x   = [0.1 -0.4];
            InPar.Gamma = 2.0;
            InPar.TotTime = 1000;
            InPar.sigma_x = 0.02;
            InPar.sigma_F_rel = 0.02.*sum(InPar.A);
        case 15

            % simulation for non evenly spaced case
            InPar.DeltaT  = 0.1;

            InPar.Tau = 25;
            InPar.A0  = 0;
            InPar.A   = [1 0.5];
            InPar.x   = [0.1 -0.4];
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
                                    'StdMeanRange',InPar.StdMeanRange,...
                                    'AliasFactor',InPar.AliasFactor,...
                                    'EndMatching',InPar.EndMatching);
end


function [ResLC,Section,PS]=generateLCuneq(InPar,JD)

    if nargin<2
        JD = [];
    end
    %T=timeseries.random_time_sequence; 
    InPar.TotTime = 2000;
    InPar.DeltaT  = 0.1;
    InPar.AliasFactor = 5;
    [ResLC,ResG]=generateLC(InPar)
    
    if isempty(JD)
        T=timeseries.random_time_sequence(3.*365,1,270,0.05,0.8);
    else
        T = JD;
    end
    
    ResLC.F_t = interp1(ResLC.T,ResLC.F_t,T,'pchip');
    ResLC.T   = T;
    Section = [];
    PS      = [];
    
%     ResLC=TimeDelay.rand_lensed_uneq(T,InPar);
%     
%     N = numel(T);
%     LC = [ResLC.T, ResLC.F_t, ResLC.Base.sigma_F_hat.*ones(N,1)];
%     [PS,Section]=TimeDelay.power_spectrum_sections(LC);
    
end

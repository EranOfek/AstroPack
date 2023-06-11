function Result=simulateThreshold(P, B, F, Args)
    % Estimate a matched-filter detection threshold using simulations.
    %   For Poisson noise match filter, analytic estimate of the detection
    %   threshold corresponding to some false alarm probability is not
    %   possible and require simulations.
    %   This function calculates the thresholds for a set of
    %   PSFs/Back/flux.
    % Input  : - A PSF or a cube of PSFs in which the PSF index is in the
    %            3rd dimension.
    %          - A Vector Of background levels,
    %          - A vector of source flux.
    %          * ...,key,val,...
    %            'ThreshP' - A vector of false alarm probability for which
    %                   to calculate detection threshold. 
    %                   Default is [1e-3, 1e-4].
    %            'Nsim' - Number of simulations per PSF/back/flux.
    %                   Default is 1e6.
    %            'Nbatch' - Number of simualtaion in a batch in each
    %                   simulation. This parameter controls the run time.
    %                   Default is 1e3.
    % Output : - A structure array with element per PSF/back/flxu, with the
    %            following fields:
    %            .P - The PSF
    %            .B - The background.
    %            .F - The flux.
    %            .SimT - A vector of all simulated thesholds.
    %            .ThreshP - Vector of inputs probabilities for which to
    %                   calculate the tresholds.
    %            .Thresh - The vector of estimated thresholds (one per
    %                   probability).
    % Author : Eran Ofek (Feb 2023)
    % Example: P = imUtil.kernel2.gauss([1.5;2]); B=0.005; F=3;
    %          Pp = imUtil.poissNoise.poissonMatchedFilter(P, B, F);
    %          Result = imUtil.poissNoise.simulateThreshold(Pp, B, F);
    %          

    
    arguments
        P
        B
        F
        Args.ThreshP    = 1e-5; %[1e-3, 1e-4, 1.3e-3];
        Args.Nsim       = 1e6;
        Args.Nbatch     = 1e6; 
    end
    
    SizeP = size(P);
    SizeP = SizeP(1:2);
    Npsf  = size(P,3);
    Nb    = numel(B);
    Nf    = numel(F);
    
    % make sure P, B, F have the same number of elements
    Nmax  = max(Nb, Nf);
    Nmax  = max(Nmax, Npsf);
    B     = B.*ones(Nmax,1);
    F     = F.*ones(Nmax,1);
    if Npsf==1
        P = repmat(P,[1 1 Nmax]);
    end
    
    Nloop     = ceil(Args.Nsim./Args.Nbatch);
    Args.Nsim = Nloop.*Args.Nbatch;
    
    Result = struct('P',cell(Nmax,1), 'B', cell(Nmax,1), 'F', cell(Nmax,1), 'SimT',cell(Nmax,1), 'ThreshP',cell(Nmax,1), 'Thresh',cell(Nmax,1));
    for Imax=1:1:Nmax
        Result(Imax).P    = P(:,:,Imax);
        Result(Imax).B    = B(Imax);
        Result(Imax).F    = F(Imax);
        Result(Imax).SimT = nan(Args.Nsim,1);
        
        for Isim=1:1:Nloop
            Image = poissrnd(B(Imax), SizeP(1), SizeP(2), Args.Nbatch);
            
            IndS  = (Isim-1).*Args.Nbatch +1;
            IndE  = Isim.*Args.Nbatch; 
            Result(Imax).SimT(IndS:IndE) = squeeze(sum(Image.*P(:,:,Imax),[1 2]));
        end
        
        Result(Imax).ThreshP = Args.ThreshP;
        Result(Imax).Thresh  = quantile(Result(Imax).SimT, 1-Args.ThreshP);
        
    end
    
    
end

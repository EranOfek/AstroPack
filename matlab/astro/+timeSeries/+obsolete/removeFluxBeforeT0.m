function [NewLC]=removeFluxBeforeT0(LC, ExpTime, Args)
    % Given a LC, remove baseline flux, and any flux detected before t0.
    % Input  : - LC [T, Mag, Err]
    %          - ExpTime.
    %          * ...,key,val,...
    %            'T0' - t0, before zero flux is expected.
    %                   Default is 0.
    %            'ErrT0' - Error in t0. Default is 0.2.
    %            'Mag' - baseline magnitude to subtract from all
    %                   measurments. Default is 0.
    %            'ErrMag0' - Error in baseline mag. Default is 0.1.
    %            'Luptitude' - Logical indicating if to use luptitude in
    %                   the back conversion. Default is true.
    % Output : - [Output LC [T, Mag, Err]
    % Author : Eran Ofek (Dec 2022)
    % Example: LC = [-4 16 0.1; -1 15 0.1;2 15.1 0.1; 6 15.2 0.1];
    %          [NewLC]=timeSeries.removeFluxBeforeT0(LC, 5, 'Mag0',16)
    
    arguments
        LC
        ExpTime
        Args.T0       = 0;
        Args.ErrT0    = 0.2;
        Args.Mag0     = 0;
        Args.ErrMag0  = 0.1;
        Args.Luptitude logical  = true;
    end
    
    N = size(LC,1);
    if numel(ExpTime)==1
        ExpTime = ExpTime.*ones(N,1);
    end
    
    T      = LC(:,1);
    Mag    = LC(:,2);
    MagErr = LC(:,3);
        
    Tt0    = T - Args.T0;
    Flux   = 10.^(-0.4.*Mag);    
    ErrFlux= MagErr./1.086;
    
    F0     = 10.^(-0.4.*Args.Mag0);
    ErrF0  = Args.ErrMag0./1.086;
    
    Flux   = Flux - F0;
    % propgate F0 error to flux
    ErrFlux= sqrt(ErrFlux.^2 + ErrF0.^2);

    Tend   = T + ExpTime.*0.5;
    Tstart = T - ExpTime.*0.5;
    
    Flag   = Tend>0 & Tstart<0;
    
    TmidF  = Tend(Flag).*0.5;
    FluxF  = Flux(Flag).*ExpTime(Flag)./Tend(Flag);
    ErrFluxTall = zeros(N,1);
    ErrFluxTall(Flag) = Flux(Flag).*ExpTime(Flag)./Args.ErrT0;
    
    Flux(Flag) = FluxF;
    % propagate T0 error to flux
    ErrFlux = sqrt(ErrFlux.^2 + (Flux./ErrFluxTall).^2);
    
    if Args.Luptitude
        NewMag   = convert.luptitude(Flux);
    else
        NewMag   = -2.5.*log10(Flux);
    end
    NewErr   = MagErr;
    NewErr(Flag)   = 1.086.*ErrFlux(Flag);
    NewT     = T;
    NewT(Flag) = TmidF;
    NewLC = [NewT, NewMag, NewErr];
end

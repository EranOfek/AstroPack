function [Result] = zp_corr(Mag, MagErr, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 May) 
    % Example: R=imUtil.relPhot.zp_corr(Mag,MagErr);

    arguments
        Mag
        MagErr
        Args.MaxMagErr         = 0.01;
        Args.MedMag            = [];
        Args.MedMagErr         = [];
        Args.MinNgoodSrc       = 20;
    end

    if isempty(Args.MedMag)
        Args.MedMag = median(Mag, 1, 'omitnan');
    end

    if isempty(Args.MedMagErr)
        Args.MedMagErr = median(MagErr, 1, 'omitnan');
    end
    
    [Nepoch, Nsrc] = size(Mag);

    Nnn     = sum(~isnan(Mag), 1);

    IndGood = find(Args.MedMagErr<Args.MaxMagErr & Nnn==Nepoch);
    Ngood   = numel(IndGood);

    if Ngood<Args.MinNgoodSrc
        % number of good sources is smaller than MinNgoodSrc

    else
        MagGood = Mag(:,IndGood);
        CorrC    = corr(MagGood(:,1), MagGood);
        CorrC = corr(MagGood(:,1), MagGood, 'Rows', 'pairwise');
        'a'

    end


end

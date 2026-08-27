function [AI, Result] = limmag(AI, Args)
    % Calculate limiting magnitude from catalog magnitude and S/N.
    %
    % The function estimates the limiting magnitude by fitting a linear
    % relation between magnitude and log10(S/N):
    %
    %   Mag = a.*log10(SN) + b
    %
    % using sources with:
    %
    %   Args.MinSN < SN < Args.MaxSN
    %
    % The limiting magnitude is then evaluated at Args.LimSN.
    % The result is written to the AstroImage header using Args.KeyLimMag.
    %
    % Input  : - AI, an AstroImage object or array of AstroImage objects.
    %          * ...,key,val,...
    %            'ColMag' - Catalog column name containing the magnitude.
    %                   Default is 'MAG_PSF'.
    %            'ColSN' - Catalog column name containing the signal-to-noise
    %                   ratio.
    %                   Default is 'SN'.
    %            'Plot' - Logical indicating whether to plot S/N versus
    %                   magnitude.
    %                   Default is false.
    %            'MaxSN' - Maximum S/N to use in the fit.
    %                   Default is 50.
    %            'MinSN' - Minimum S/N to use in the fit.
    %                   Default is 4.
    %            'LimSN' - S/N value at which to evaluate the limiting
    %                   magnitude.
    %                   Default is 5.
    %            'MinNsrc' - Minimum number of sources in the S/N window
    %                   required for the fit. If the number of sources is
    %                   smaller, then the limiting magnitude is set to NaN
    %                   (written to the header as an undefined value).
    %                   Default is 10.
    %            'KeyLimMag' - Header keyword in which to store the limiting
    %                   magnitude. If empty, the header is not updated.
    %                   The keyword is written also when the limiting
    %                   magnitude can not be estimated (e.g., an empty
    %                   catalog), in which case its value is NaN.
    %                   Default is 'LIMMAG'.
    %            ''KeyLimMagErr' - Header keyword in which to store the limiting
    %                   magnitude. Frfault is 'LIMMAGER'.
    % Output : - AI, AstroImage object with updated header keyword containing
    %                   the limiting magnitude.
    %          - Result, structure array with one element per AstroImage.
    %                   Fields:
    %                   .LimMag  - Estimated limiting magnitude, or NaN if
    %                              less than Args.MinNsrc sources are available.
    %                   .Par     - Best-fit polynomial parameters, as returned
    %                              by polyfit.
    %                   .Nsrc    - Number of sources used in the fit.
    %                   .Flag    - Logical vector of sources used in the fit.
    % Author : Eran Ofek (2026 Jun)
    % Example: [AI,Res] = imProc.calib.limmag(AI);
    %          [AI,Res] = imProc.calib.limmag(AI, 'ColMag','MAG_APER', 'LimSN',5);

    arguments
        AI
        Args.ColMag        = 'MAG_PSF';
        Args.ColSN         = 'SN';
        Args.Plot          = false;

        Args.MaxSN         = 50;
        Args.MinSN         = 4;
        Args.LimSN         = 5;
        Args.MinNsrc       = 10;

        Args.KeyLimMag     = 'LIMMAG';
        Args.KeyLimMagErr  = 'LIMMAGER';
    end

    Nai = numel(AI);

    if nargout > 1
        Result = struct( ...
            'LimMag', cell(size(AI)), ...
            'Par',    cell(size(AI)), ...
            'Nsrc',   cell(size(AI)), ...
            'Flag',   cell(size(AI)));
    end

    for Iai = 1:1:Nai
        Data = AI(Iai).CatData.getColMulti({Args.ColMag, Args.ColSN});

        Mag = Data(:,1);
        SN  = Data(:,2);

        Flag = isfinite(Mag) & isfinite(SN) & ...
               SN > Args.MinSN & SN < Args.MaxSN;

        if sum(Flag) < Args.MinNsrc
            % too few sources for a meaningful fit (e.g., an empty catalog):
            % polyfit returns [0 0] for empty input, so guard it explicitly
            Par       = [NaN NaN];
            LimMag    = NaN;
            LimMagErr = NaN;
        else
            [Par,S] = polyfit(log10(SN(Flag)), Mag(Flag), 1);
            LimMag = polyval(Par, log10(Args.LimSN));
            LimMagErr = S.normr;
        end

        if Args.Plot
            semilogy(Mag, SN, '.');
            hold on;
            semilogy(Mag(Flag), SN(Flag), '.');
            xlabel(Args.ColMag);
            ylabel(Args.ColSN);
        end

        % Update header
        if ~isempty(Args.KeyLimMag)
            AI(Iai).HeaderData.replaceVal(Args.KeyLimMag, LimMag);
            if ~isempty(Args.KeyLimMagErr)
                AI(Iai).HeaderData.replaceVal(Args.KeyLimMagErr, LimMagErr);
            end
        end

        if nargout > 1
            Result(Iai).LimMag = LimMag;
            Result(Iai).Par    = Par;
            Result(Iai).Nsrc   = sum(Flag);
            Result(Iai).Flag   = Flag;
        end
    end
end
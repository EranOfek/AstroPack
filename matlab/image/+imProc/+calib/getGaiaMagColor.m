function [Mag, MagErr, Color, ColorErr, SelectedInd] = getGaiaMagColor(Cat, Args)
    % Extract GAIA magnitude and color with quality selection and system conversion
    %     Extract a primary magnitude and color from a GAIA AstroCatalog,
    %     optionally convert S/N to magnitude errors, apply quality and
    %     magnitude range selection, and convert from Vega to AB system.
    % Input  : - AstroCatalog containing a GAIA catalog
    %          * ...,key,val,... 
    %            'ColMag' - Gaia catalog column name containing the primary
    %                   magnitude to extract from GAIA.
    %                   Default is 'phot_bp_mean_mag'.
    %            'ColMagErr' - Gaia catalog column name containing the primary
    %                   magnitude error to extract from GAIA.
    %                   Default is 'phot_bp_mean_flux_over_error'.
    %            'ColColor' - Gaia catalog column name containing the primary
    %                   color to extract from GAIA. Alternatively, if a
    %                   cell array of two column names (blue band first),
    %                   then will use the two bands to calculate color
    %                   (Color = Band1 - Band2).
    %                   Default is {'phot_bp_mean_mag','phot_rp_mean_mag'}.
    %            'ColColorErr' - Like 'ColColor', but for its errors.
    %                   If two columns are provided, errors are propagated
    %                   in quadrature.
    %                   Default is {'phot_bp_mean_flux_over_error','phot_rp_mean_flux_over_error'}.
    %            'IsErrSN' - If true, assumes error columns contain
    %                   flux-over-error (S/N) and converts them to
    %                   magnitude errors using 1.086./(S/N).
    %                   Default is true.
    %            'MaxErr' - Maximum allowed magnitude error for source
    %                   selection.
    %                   Default is 0.02.
    %            'MagRange' - Two-element vector specifying the allowed
    %                   magnitude range [Min Max] for selection.
    %                   Default is [13 18].
    %            'SelectCrit' - Cell array specifying additional selection
    %                   criteria in pairs of:
    %                   {ColumnName, [Min Max], ...}.
    %                   Default is {'Plx',[0.05 1000], ''}.
    %            'MagSys' - Output magnitude system. Options are:
    %                   'Vega' - leave GAIA magnitudes in Vega system.
    %                   'AB'   - convert magnitudes and colors to AB system
    %                            using survey zero points.
    %                   Default is 'AB'.
    %            'CatZP' - Survey name used to retrieve Vega–AB zero point
    %                   offsets via astro.mag.survey_ZP.
    %                   Default is 'GAIADR3'.
    %            
    % Output : - A column vector of extracted magnitude only for selected
    %            sources.
    %          - A column vector of magnitude errors for selected sources.
    %          - A column vector of extracted or calculated color for
    %            selected sources.
    %          - A column vector of color errors for selected sources.
    %          - A column vector of indices of selected sources in the
    %            original catalog.
    % Author : Eran Ofek (2026 Feb) 
    % Example: [Mag, MagErr, Color, ColorErr, SelectedInd] = imProc.calib.getGaiaMagColor(Cat);
    arguments
        Cat
        Args.ColMag                 = 'phot_bp_mean_mag'; %'Mag_BP';
        Args.ColMagErr              = 'phot_bp_mean_flux_over_error'; %'ErrMag_BP';  
        Args.ColColor               = {'phot_bp_mean_mag','phot_rp_mean_mag'};  %{'Mag_RP','Mag_G'};   % red to blue...
        Args.ColColorErr            = {'phot_bp_mean_flux_over_error','phot_rp_mean_flux_over_error'};
        Args.IsErrSN                = true;
        
        
        Args.MaxErr                 = 0.02;
        Args.MagRange               = [13 18];
        Args.SelectCrit             = {'Plx',[0.05 1000], ''}

        Args.MagSys                 = 'AB';
        Args.CatZP                  = 'GAIADR3';

    end

    Mag         = Cat.getCol(Args.ColMag);
    MagErr      = Cat.getCol(Args.ColMagErr);
    Color       = Cat.getCol(Args.ColColor);
    ColorErr    = Cat.getCol(Args.ColColorErr);

    if Args.IsErrSN
        % convert S/N to errors
        MagErr      = 1.086./MagErr;
        ColorErr    = 1.086./ColorErr;
    end

    if size(Color,2)==2
        % convert bands to color
        Color    = Color(:,1) - Color(:,2);
        ColorErr = sqrt(ColorErr(:,1).^2 + ColorErr(:,2).^2);
    end

    % Select stars
    Flag = MagErr<Args.MaxErr & Mag>Args.MagRange(1) & Mag<Args.MagRange(2);
    Nselect = numel(Args.SelectCrit);
    for Iselect=1:2:Nselect-1
        SelectCol = Cat.getCol(Args.SelectCrit{Iselect});
        Flag      = Flag & SelectCol>Args.SelectCrit{Iselect+1}(1) & SelectCol<Args.SelectCrit{Iselect+1}(2);
    end
    % select rows
    SelectedInd = find(Flag);
    Mag         = Mag(SelectedInd);
    MagErr      = MagErr(SelectedInd);
    Color       = Color(SelectedInd);
    ColorErr    = ColorErr(SelectedInd);

    switch lower(Args.MagSys)
        case 'vega'
            % do nothing GAIA is already in Vega sys
        case 'ab'
            %if 1==0
            %VegaToAB_Filters  = {'Mag_G','Mag_BP','Mag_RP'};
            VegaToAB_Filters  = {'phot_g_mean_mag','phot_bp_mean_mag','phot_rp_mean_mag'};
            
            GAIA_EDR3_ZP_VegaMinusAB = astro.mag.survey_ZP(Args.CatZP, 'VegaMinusAB');

            I1 = strcmp(Args.ColMag, VegaToAB_Filters);
            Mag = Mag - GAIA_EDR3_ZP_VegaMinusAB(I1);

            I2 = ismember(VegaToAB_Filters, Args.ColColor);
            Color = Color + diff(GAIA_EDR3_ZP_VegaMinusAB(I2),1,2);  % note the + sign here is because its B-R, while diff do R-B...
            %end
        otherwise
            error('Unknown MagSys option');
    end


end

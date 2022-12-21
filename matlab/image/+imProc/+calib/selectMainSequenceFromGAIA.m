function [Result, FlagRes, FitPar] = selectMainSequenceFromGAIA(Obj, Args)
    % Select main sequence stars from GAIA catalog in AstroCatalog object.
    % Input  : - An AstroCatalog object containing a GAIA EDR3 catalog
    %            (e.g., using catsHTM).
    %          * ...,key,val,...
    %            'CreateNewObj' - true|false. Create a new copy of the input catalog.
    %                   Default is true.
    %               Selection parameters
    %            'PlxLimit' - Default is 1 mas
    %            'PlxSNLimit' - Default is 10.
    %            'ExcessNoiseLimit' - Default is 500 micro arcsec.
    %            'ExcessNoiseSigLimit' - Default is 2.
    %               Fitting parameters
    %            'ParMS' - A vector of main-sequence abs mag vs. color
    %                   polynomials. If empty, will fit polynomial.
    %                   Default is [].
    %            'PolyOrder' - Polynomial order to fit. Default is 9.
    %            'DiffClip' - A vector of difference clipping.
    %                   In each iteration will clip sources with residuals
    %                   larger than the corresponding value.
    %                   Default is [2 1 0.3].
    %            'ColorLimit' - Color boundries. Default is [0.5 2].
    %            'Plot - A logical indicating if to plot the best fit data.
    % Output : - An AstroCatalog object with the selected main sequence
    %            stars.
    %          - As tructure array containing a 'Flag' field for the
    %            selected rows per catalog.
    %          - A structure array with the best fitted polynomial
    %            parameters.
    % Author : Eran Ofek (Sep 2021)
    % Example: DataSampleDir = tools.os.getTestDataDir; cd(DataSampleDir);
    %          GM = io.files.load2('AstrometricCat_PTF_Cropped.mat');
    %          [Result, FlagRes, Par] = imProc.calib.selectMainSequenceFromGAIA(GM)

    arguments
        Obj AstroCatalog    % Astrocatalog object
        Args.CreateNewObj logical = true;   

        Args.PlxLimit            = 1;
        Args.PlxSNLimit          = 10;
        Args.ExcessNoiseLimit    = 500;
        Args.ExcessNoiseSigLimit = 2;

        Args.ParMS               = [-0.11834 1.0836 -3.4117 3.3404 2.6857 -3.2869 -9.3112 13.814 -0.5878 2.5379];  % if empty - fit
        Args.PolyOrder           = 9;
        Args.DiffClip            = [2 1 0.3];
        Args.ColorLimit          = [0.5 2];
        Args.Plot logical        = false;
    end


    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    Cols   = {'Plx', 'ErrPlx', 'Mag_G', 'Mag_BP', 'Mag_RP', 'ExcessNoise','ExcessNoiseSig'};
    

    FitPar = [];
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        DataSt(Iobj) = getCol2struct(Obj(Iobj), Cols);
        Flag = DataSt(Iobj).Plx > Args.PlxLimit & ...
               (DataSt(Iobj).Plx./DataSt(Iobj).ErrPlx) > Args.PlxSNLimit & ...
               DataSt(Iobj).ExcessNoise < Args.ExcessNoiseLimit & ...
               DataSt(Iobj).ExcessNoiseSig < Args.ExcessNoiseSigLimit & ...
               (DataSt(Iobj).Mag_BP - DataSt(Iobj).Mag_RP) > Args.ColorLimit(1) & ...
               (DataSt(Iobj).Mag_BP - DataSt(Iobj).Mag_RP) < Args.ColorLimit(2);


        DistMod =  5.*log10(1000./DataSt(Iobj).Plx) - 5;  % m-M
        AbsMagG = DataSt(Iobj).Mag_G - DistMod;
        Color   = DataSt(Iobj).Mag_BP - DataSt(Iobj).Mag_RP;

        % Fit
        if isempty(Args.ParMS)
            Color_F   = Color;
            AbsMagG_F = AbsMagG;
            for Id=1:1:numel(Args.DiffClip)
                Par  = polyfit(Color_F, AbsMagG_F, Args.PolyOrder);
                Y    = polyval(Par, Color_F);
                Flag = abs(AbsMagG_F - Y) < Args.DiffClip(Id);
                Color_F   = Color_F(Flag);
                AbsMagG_F = AbsMagG_F(Flag);
            end
            Args.ParMS  = polyfit(Color_F, AbsMagG_F, Args.PolyOrder);
            FitPar(Iobj).ParMS = Args.ParMS;
        end

        % select
        Y = polyval(Args.ParMS, Color);
        FlagRes(Iobj).Flag = abs(AbsMagG - Y) < Args.DiffClip(end) & ...
                             Color > Args.ColorLimit(1) & ...
                             Color < Args.ColorLimit(2);

        Result(Iobj) = selectRows(Obj(Iobj), FlagRes(Iobj).Flag);
        
        if Args.Plot
            % plot H-R
            plot(Color, AbsMagG, '.');
            hold on;
            plot(Color(FlagRes(Iobj).Flag), AbsMagG(FlagRes(Iobj).Flag), 'ko','MarkerFaceColor','k','MarkerSize',3);
            X = linspace(Args.ColorLimit(1), Args.ColorLimit(2), 100);
            Y = polyval(Args.ParMS, X);
            plot(X, Y, 'k-');
        end
    end
end
function [RadiusVec, AngleVec, MeanSector, MedianSector, StdSector, NSector] = radialAnnulusStatsByAngle(Image, X, Y, Args)
    % Calculate statistics in annulus-angle sectors
    %   For each annular sector around position (X,Y), calculate the number of
    %   pixels, mean, median, and standard deviation. The annuli have radial
    %   width Args.StepRadius, and the angular sectors have opening angle
    %   Args.Angle degrees.
    %
    % Input  : - 2-D image matrix.
    %          - X position (column index, j).
    %          - Y position (row index, i).
    %          * ...,key,val,...
    %            'MaxRad'     - Maximum radius in pixels. Default is 100.
    %            'StepRadius' - Radial bin width in pixels. Default is 2.
    %            'Angle'      - Opening angle in degrees. Default is 30.
    %
    % Output : - (RadiusVec) Column vector of radial bin centers.
    %          - (AngleVec) Row vector of angular bin centers in degrees.
    %          - (MeanSector) Mean value in each sector.
    %            MeanSector, MedianSector, StdSector, and NSector are matrices of size:
    %            NumberOfRadii X NumberOfSectors
    %          - (MedianSector) Median value in each sector.
    %          - (StdSector) Standard deviation in each sector.
    %          - (NSector) Number of pixels in each sector.
    % Author : Eran Ofek + ChatGPT (Mar 2026)
    %
    % Example:
    %   [RadiusVec, AngleVec, MeanSector, MedianSector, StdSector, NSector] = ...
    %       radialAnnulusStatsByAngle(Image, 512, 512);
    %
    %   [RadiusVec, AngleVec, MeanSector, MedianSector, StdSector, NSector] = ...
    %       radialAnnulusStatsByAngle(Image, 512, 512, StepRadius=5, Angle=20);
    
    arguments
        Image (:,:) {mustBeNumeric}
        X (1,1) double {mustBeFinite, mustBeReal}
        Y (1,1) double {mustBeFinite, mustBeReal}
        Args.MaxRad (1,1) double {mustBePositive, mustBeFinite, mustBeReal} = 100
        Args.StepRadius (1,1) double {mustBePositive, mustBeFinite, mustBeReal} = 2
        Args.Angle (1,1) double {mustBePositive, mustBeFinite, mustBeReal} = 30
    end

    if abs(round(360./Args.Angle) - 360./Args.Angle) > 1e-10
        error('Args.Angle must divide 360 exactly.');
    end

    NumberOfSectors = round(360./Args.Angle);
    NumberOfRadii   = ceil(Args.MaxRad./Args.StepRadius);

    RadiusVec = ((1:NumberOfRadii).' - 0.5) .* Args.StepRadius;
    AngleVec  = ((1:NumberOfSectors) - 0.5) .* Args.Angle;

    [SizeY, SizeX] = size(Image);

    Xmin = max(1, floor(X - Args.MaxRad));
    Xmax = min(SizeX, ceil(X + Args.MaxRad));
    Ymin = max(1, floor(Y - Args.MaxRad));
    Ymax = min(SizeY, ceil(Y + Args.MaxRad));

    [GridX, GridY] = meshgrid(Xmin:Xmax, Ymin:Ymax);

    Dx = GridX - X;
    Dy = GridY - Y;

    Radius = sqrt(Dx.^2 + Dy.^2);
    Theta  = atan2d(Dy, Dx);
    Theta(Theta < 0) = Theta(Theta < 0) + 360;

    RadiusBin = floor(Radius ./ Args.StepRadius) + 1;
    AngleBin  = floor(Theta ./ Args.Angle) + 1;
    AngleBin(AngleBin > NumberOfSectors) = NumberOfSectors;

    SubImage = double(Image(Ymin:Ymax, Xmin:Xmax));

    Valid = Radius <= Args.MaxRad & ...
            RadiusBin >= 1 & RadiusBin <= NumberOfRadii & ...
            isfinite(SubImage);

    RadiusBin = RadiusBin(Valid);
    AngleBin  = AngleBin(Valid);
    Values    = SubImage(Valid);

    MeanSector   = NaN(NumberOfRadii, NumberOfSectors);
    MedianSector = NaN(NumberOfRadii, NumberOfSectors);
    StdSector    = NaN(NumberOfRadii, NumberOfSectors);
    NSector      = zeros(NumberOfRadii, NumberOfSectors);

    for IRad = 1:NumberOfRadii
        for ISector = 1:NumberOfSectors
            Flag = (RadiusBin == IRad) & (AngleBin == ISector);
            SectorValues = Values(Flag);

            NSector(IRad, ISector) = numel(SectorValues);

            if ~isempty(SectorValues)
                MeanSector(IRad, ISector)   = mean(SectorValues);
                MedianSector(IRad, ISector) = median(SectorValues);
                StdSector(IRad, ISector)    = std(SectorValues);
            end
        end
    end
end
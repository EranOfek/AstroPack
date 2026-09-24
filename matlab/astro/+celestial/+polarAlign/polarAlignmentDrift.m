function [DotAlphaDeg, DotDeltaDeg, Result] = polarAlignmentDrift(DeltaE, DeltaZ, HA, Dec, Args)
    % Predict image drift caused by polar misalignment and refraction.
    %   Drift of the stars elative to the camera fixed coordinates.
    %
    %   This function calculates the instantaneous drift rate of a target on the
    %   celestial sphere relative to the camera-fixed coordinates of an equatorial
    %   mount that tracks at the sidereal rate.
    %
    %   The drift may be caused by:
    %       1) Polar misalignment of the mount RA axis relative to the true NCP.
    %       2) Atmospheric refraction, which changes the apparent sky position with
    %           time and therefore introduces a tracking-rate error even for a mount
    %           with perfect polar alignment.
    %
    %   The output drift components are returned in the tangent basis of the true
    %   equatorial coordinate system:
    %
    %   DotAlphaDeg - drift along the local direction of increasing RA,
    %                 measured on a great circle [deg/s]
    %
    %   DotDeltaDeg - drift along the local direction of increasing Dec [deg/s]
    %
    % Input  : - (DeltaE) RA-axis azimuth error [deg] measured eastward.
    %          - (DeltaZ) RA-acis altitude error [deg] measured upward.
    %          - (HA) Vector of HA [deg].
    %          - (Dec) Vector of Dec [deg] (each element corresponds to element
    %            in HA.
    %          * ...,key,val,...
    %            'Phi'      - observer latitude [deg]. Default: 30.05298.
    %                   If negative, then assume the solution is corresponding
    %                   to the south celestial pole.
    %            'ApplyRefraction' - logical flag. Default: true
    %            'Pressure'       - pressure [mbar]. Default: 1010
    %            'TemperatureC'   - temperature [C]. Default: 15
    %            'StepHA'         - HA step for numerical derivative [deg]. Default: 1e-4
    %            'MinAltitude'    - minimum altitude for refraction law [deg]. Default: 3
    %            'ReturnStruct'   - logical flag. Default: false
    % Output : - Drift rate in Right Asc. direction [deg/s on great circle].
    %          - Drift rate in declination [deg/s].
    %          - Structure with additional results.
    %
    % Author : ChatGPT + Eran Ofek (Apr 2026)
    % Example: [DotA, DotD]=polarAlignmentDrift(1,1,[30;10],[0;0],'ApplyRefraction',0); [DotA, DotD].*86400
    
    
    arguments
        DeltaE                 (1,1) double
        DeltaZ                 (1,1) double
        HA                     (:,:) double
        Dec                    (:,:) double
        Args.Phi               (1,1) double = 30.05298  % if - then south celestial pole
        Args.ApplyRefraction   (1,1) logical = true
        Args.Pressure          (1,1) double = 1010
        Args.TemperatureC      (1,1) double = 10
        Args.StepHA            (1,1) double = 1e-4
        Args.MinAltitude       (1,1) double = 3
        Args.ReturnStruct      (1,1) logical = false
    end
    
    
    % Keep original size for output reshaping
    SizeHA  = size(HA);
    SizeDec = size(Dec);
    
    % Convert HA/Dec to paired row vectors
    [HA, Dec, OutputSize] = matchPairVectors(HA, Dec, SizeHA, SizeDec);
    
    % Sidereal angular speed [rad/s]
    OmegaSidRad = 2.*pi./86164.0905;
    
    % Convert angles to radians
    H   = deg2rad(HA);
    D   = deg2rad(Dec);
    Lat = deg2rad(Args.Phi);
    DE  = deg2rad(DeltaE);
    DZ  = deg2rad(DeltaZ);
    DH  = deg2rad(Args.StepHA);
    
    % True pole in ENU coordinates
    
    % Mount pole in ENU coordinates
    if Args.Phi>0
        P = [0; cos(Lat); sin(Lat)];
    else
        P = [0; -cos(Lat); -sin(Lat)];
    end
    
    Pm = [cos(Lat + DZ).*sin(DE); ...
          cos(Lat + DZ).*cos(DE); ...
          sin(Lat + DZ)];
    
    % Apparent or true target direction
    if Args.ApplyRefraction
        A = localApparentVector(H, D, Lat, Args.Pressure, Args.TemperatureC, Args.MinAltitude);
    
        Aplus  = localApparentVector(H + DH, D, Lat, Args.Pressure, Args.TemperatureC, Args.MinAltitude);
        Aminus = localApparentVector(H - DH, D, Lat, Args.Pressure, Args.TemperatureC, Args.MinAltitude);
    
        dAdH = (Aplus - Aminus) ./ (2.*DH);
        dAdt = dAdH .* OmegaSidRad;
    else
        A = trueVectorENU(H, D, Lat);
    
        % For HA positive westward:
        % dS/dt = -OmegaSidRad * (P x S)
        dAdt = -cross3(repmatCol(P .* OmegaSidRad, size(A, 2)), A);
    end
    
    % Total camera-fixed drift
    Vtot = dAdt + cross3(repmatCol(Pm .* OmegaSidRad, size(A, 2)), A);
    
    % Split into refraction-only and polar-only components
    Vref = dAdt + cross3(repmatCol(P .* OmegaSidRad, size(A, 2)), A);
    Vpol = cross3(repmatCol((Pm - P) .* OmegaSidRad, size(A, 2)), A);
    
    % Tangent basis at apparent position
    PA = dotCol(P, A);
    
    Tmp = P - A .* PA;
    NormTmp = sqrt(sum(Tmp.^2, 1));
    eDelta = Tmp ./ NormTmp;
    
    Tmp = -cross3(repmatCol(P, size(A, 2)), A);
    NormTmp = sqrt(sum(Tmp.^2, 1));
    eAlpha = Tmp ./ NormTmp;
    
    % Project relative motion onto the tangent basis [rad/s]
    DotAlphaRad = dotCol(eAlpha, Vtot);
    DotDeltaRad = dotCol(eDelta, Vtot);
    
    % Convert to [deg/s]
    DotAlphaDeg = reshape(rad2deg(DotAlphaRad), OutputSize);
    DotDeltaDeg = reshape(rad2deg(DotDeltaRad), OutputSize);
    
    if nargout > 2 || Args.ReturnStruct
        Result = struct;
    
        Result.DotAlphaDeg       = DotAlphaDeg;
        Result.DotDeltaDeg       = DotDeltaDeg;
        Result.DotAlphaArcsec    = DotAlphaDeg .* 3600;
        Result.DotDeltaArcsec    = DotDeltaDeg .* 3600;
    
        Result.DotAlphaRad       = reshape(DotAlphaRad, OutputSize);
        Result.DotDeltaRad       = reshape(DotDeltaRad, OutputSize);
    
        Result.ApparentVector    = A;
        Result.dAdt              = dAdt;
        Result.Vtot              = Vtot;
        Result.Vref              = Vref;
        Result.Vpol              = Vpol;
    
        Result.DotAlphaRefDeg    = reshape(rad2deg(dotCol(eAlpha, Vref)), OutputSize);
        Result.DotDeltaRefDeg    = reshape(rad2deg(dotCol(eDelta, Vref)), OutputSize);
        Result.DotAlphaPolDeg    = reshape(rad2deg(dotCol(eAlpha, Vpol)), OutputSize);
        Result.DotDeltaPolDeg    = reshape(rad2deg(dotCol(eDelta, Vpol)), OutputSize);
    
        Result.DotAlphaRefArcsec = Result.DotAlphaRefDeg .* 3600;
        Result.DotDeltaRefArcsec = Result.DotDeltaRefDeg .* 3600;
        Result.DotAlphaPolArcsec = Result.DotAlphaPolDeg .* 3600;
        Result.DotDeltaPolArcsec = Result.DotDeltaPolDeg .* 3600;
    
        Result.P                 = P;
        Result.Pm                = Pm;
        Result.OmegaSidRad       = OmegaSidRad;
        Result.eAlpha            = eAlpha;
        Result.eDelta            = eDelta;
        Result.Args              = Args;
        Result.OutputSize        = OutputSize;
    else
        Result = [];
    end

end


function [HA, Dec, OutputSize] = matchPairVectors(HA, Dec, SizeHA, SizeDec)
% matchPairVectors Match HA and Dec into paired row vectors.
%
% Rules:
%   1) If both are scalar -> one pair.
%   2) If one is scalar and the other is vector/array -> expand scalar.
%   3) If both are non-scalar -> they must have the same number of elements,
%      and pairing is done element-by-element in linear indexing order.
%
% Output:
%   HA, Dec      - 1xN row vectors
%   OutputSize   - size of returned outputs

Nha  = numel(HA);
Ndec = numel(Dec);

if Nha==1 && Ndec==1
    OutputSize = [1 1];
    HA  = HA(:).';
    Dec = Dec(:).';
elseif Nha==1
    OutputSize = SizeDec;
    HA  = repmat(HA, OutputSize);
    HA  = HA(:).';
    Dec = Dec(:).';
elseif Ndec==1
    OutputSize = SizeHA;
    Dec = repmat(Dec, OutputSize);
    HA  = HA(:).';
    Dec = Dec(:).';
else
    if Nha~=Ndec
        error('HA and Dec must have the same number of elements, or one of them must be scalar.');
    end

    if isequal(SizeHA, SizeDec)
        OutputSize = SizeHA;
    else
        OutputSize = [1, Nha];
    end

    HA  = HA(:).';
    Dec = Dec(:).';
end

end


function A = localApparentVector(H, D, Lat, Pressure, TemperatureC, MinAltitude)
% localApparentVector Apparent refracted target direction in ENU coordinates.

S = trueVectorENU(H, D, Lat);

AltTrue = asind(S(3,:));
R = refractionAngleRad(AltTrue, Pressure, TemperatureC, MinAltitude);

CosZ = S(3,:);
CosZ = max(-1, min(1, CosZ));
Z = acos(CosZ);
SinZ = sin(Z);

U = [0; 0; 1];
A = S;

Flag = SinZ > 1e-12;
if any(Flag)
    F1 = sin(Z(Flag) - R(Flag)) ./ SinZ(Flag);
    F2 = sin(R(Flag)) ./ SinZ(Flag);
    A(:, Flag) = S(:, Flag).*F1 + U.*F2;
end

FlagZenith = ~Flag;
if any(FlagZenith)
    A(:, FlagZenith) = S(:, FlagZenith);
end

A = A ./ sqrt(sum(A.^2, 1));

end


function S = trueVectorENU(H, D, Lat)
% trueVectorENU True unrefracted target direction in ENU coordinates.

SinH   = sin(H);
CosH   = cos(H);
SinD   = sin(D);
CosD   = cos(D);
SinLat = sin(Lat);
CosLat = cos(Lat);

S = [ -CosD .* SinH; ...
       SinD .* CosLat - CosD .* CosH .* SinLat; ...
       SinD .* SinLat + CosD .* CosH .* CosLat ];

end


function R = refractionAngleRad(AltTrueDeg, Pressure, TemperatureC, MinAltitude)
% refractionAngleRad Scalar atmospheric refraction angle [rad].

AltEval = max(AltTrueDeg, MinAltitude);

Scale = (Pressure ./ 1010) .* (283 ./ (273 + TemperatureC));

Rarcmin = (1.02 ./ tand(AltEval + 10.3 ./ (AltEval + 5.11))) .* Scale;
Rdeg = Rarcmin ./ 60;
R = deg2rad(Rdeg);

end


function C = cross3(A, B)
% cross3 Column-wise cross product.

C = [A(2,:).*B(3,:) - A(3,:).*B(2,:); ...
     A(3,:).*B(1,:) - A(1,:).*B(3,:); ...
     A(1,:).*B(2,:) - A(2,:).*B(1,:)];

end


function D = dotCol(A, B)
% dotCol Column-wise dot product.

D = sum(A.*B, 1);

end


function M = repmatCol(V, N)
% repmatCol Replicate a 3x1 vector into a 3xN matrix.

M = V .* ones(1, N);

end
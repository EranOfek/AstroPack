function [OutRA, OutDec, Alt, Refraction, Aux] = apparent_toJ2000(RA, Dec, JD, varargin)
    % Apparent coordinates to J2000 coordinates (approximation!)
    %     Using one iteration of celestial.convert.j2000_toApparent
    %     Will be fixed in the future.
    % Input  : See celestial.convert.j2000_toApparent for arguments.
    % Output : See celestial.convert.j2000_toApparent for output.
    % Author : Eran Ofek (2024 Jan) 
    % Example: [OutRA, OutDec, Alt, Refraction, Aux] = celestial.convert.apparent_toJ2000(180, 0, celestial.time.julday([1 1 2024]))
    %          [OutRA, OutDec,Alt,~,Aux] = celestial.convert.j2000_toApparent(180, 20, celestial.time.julday([1 1 2024]))
    %          [OutRA1, OutDec1, ~, ~, Aux1] = celestial.convert.apparent_toJ2000(OutRA, OutDec, celestial.time.julday([1 1 2024])


    [RA1, Dec1, Alt, Refraction, Aux1] = celestial.convert.j2000_toApparent(RA, Dec, JD, varargin{:});

    DRA  = RA1 - RA;
    DDec = Dec1 - Dec;

    OutRA  = RA - DRA;
    OutDec = Dec - DDec;

    FN = fieldnames(Aux1);
    Nfn = numel(FN);
    for Ifn=1:1:Nfn
        switch lower(FN{Ifn}(1:2))
            case 'de'
                DD = Aux1.(FN{Ifn}) - Dec;
                Aux.(FN{Ifn}) = Aux1.(FN{Ifn}) - 2.*DD;
            case 'ra'
                DD = Aux1.(FN{Ifn}) - RA;
                Aux.(FN{Ifn}) = Aux1.(FN{Ifn}) - 2.*DD;
                Aux.(FN{Ifn}) = mod(Aux.(FN{Ifn}), 360);
            case 'ha'
                TempFN = FN{Ifn};
                TempFN(1:2) = 'RA';
                II = find(strcmp(FN, TempFN));
                DD = Aux1.(FN{II}) - RA;
                Aux.(FN{Ifn}) = Aux1.(FN{Ifn}) + 2.*DD;
            otherwise
                % skip
        end


end

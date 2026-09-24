function Names = nedPretypeNames
    % Canonical NED 'pretype' object-type list (extragalactic + galactic).
    % Package: VO.prep
    % Description: Return the canonical list of NED pretype values used
    %              to encode the 'pretype' string column of
    %              NEDTAP.objdir as positional integer codes for storage
    %              in catsHTM numeric matrices.
    %              Codes = position in this list (1..63).
    %              Code 0 indicates an unknown pretype (sentinel).
    %              Names with a leading '!' denote Milky Way / Galactic
    %              objects (e.g., '!*' = galactic star, '!SNR' = galactic
    %              supernova remnant).
    %              Source: NED Documents/Guides ("Resolution" / object types).
    % Output : - 1x63 cell array of pretype name strings.
    % Author : Dana Kovaleva (May 2026)
    % Example: Names = VO.prep.nedPretypeNames;
    %          Code = find(strcmp(Names, 'QSO'), 1);
    %          [~, Codes] = ismember(string(T.pretype), string(Names));
    %
    Names = { ...
        '*',      '**',     '*Ass',   '*Cl',    'AbLS',   'Blue*',  'C*',     ...
        'EmLS',   'EmObj',  'exG*',   'Flare*', 'G',      'GammaS', 'GClstr', ...
        'GGroup', 'GPair',  'GTrpl',  'G_Lens', 'HII',    'IrS',    'MCld',   ...
        'Neb',    'Nova',   'Other',  'PN',     'PofG',   'Psr',    'QGroup', ...
        'QSO',    'Q_Lens', 'RadioS', 'Red*',   'RfN',    'SN',     'SNR',    ...
        'UvES',   'UvS',    'V*',     'VisS',   'WD*',    'WR*',    'XrayS',  ...
        '!*',     '!**',    '!*Ass',  '!*Cl',   '!Blue*', '!C*',    '!EmObj', ...
        '!Flar*', '!HII',   '!MCld',  '!Neb',   '!Nova',  '!PN',    '!Psr',   ...
        '!RfN',   '!Red*',  '!SN',    '!SNR',   '!V*',    '!WD*',   '!WR*'};
end

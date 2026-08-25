function [Dict, Code] = backDictionary(Method)
    % Dictionary of background/variance estimation methods and their codes.
    %
    % The dictionary provides a compact (uint8) representation of the
    % background and variance estimation methods, suitable for storage in
    % image headers (BCKMTHD, VARMTHD keywords) and in the data base.
    % Codes are allocated in blocks and must not be reused: 1-9 for the
    % functions that estimate the background and the variance together,
    % 10-19 for the mex implementations, and 20 and above for the
    % pre-defined options of imUtil.background.backgroundOption.
    % Code 0 is reserved for a method which is not in the dictionary
    % (e.g., a user supplied function handle).
    %
    % Input  : - A method, as passed to imUtil.background.backVar: a
    %            function handle, a char array of a pre-defined method, or
    %            a cell array of two such elements for the background and
    %            the variance methods, respectively.
    %            If not given, then only the dictionary is returned.
    % Output : - Dict, the dictionary - a struct array with the fields:
    %            .Code        - The uint8 code of the method.
    %            .Name        - The canonical method name.
    %            .Alt         - A cell array of alternate names which are
    %                           mapped to the same code.
    %            .Description - A short description of the method.
    %          - Code, a uint8 array with the code of the requested method.
    %            For a cell array input this is [BackCode, VarCode], and
    %            otherwise a scalar (a single method estimates both).
    %            An unrecognized method is coded as 0.
    % Author : A.M. Krassilchtchikov (Aug 2026)
    % Example: Dict = imUtil.background.backDictionary;
    %          [~,Code] = imUtil.background.backDictionary(@imUtil.background.modeVar_LogHist);
    %          [~,Code] = imUtil.background.backDictionary({@median,'rvar'});
    %          % decode:
    %          Name = Dict([Dict.Code]==Code(1)).Name;

    arguments
        Method = [];
    end

    Dict = localDictionary;

    if isempty(Method)
        Code = uint8.empty(1,0);
    else
        if iscell(Method)
            % separate methods for the background and the variance
            Code = uint8([localCode(Dict, Method{1}), localCode(Dict, Method{2})]);
        else
            % a single method estimates both
            Code = uint8(localCode(Dict, Method));
        end
    end

end

function Code = localCode(Dict, Method)
    % Code of a single method given as a function handle or a char array
    if isa(Method, 'function_handle')
        Name = func2str(Method);
    else
        Name = char(Method);
    end

    Code = uint8(0);
    Ndic = numel(Dict);
    for Idic=1:1:Ndic
        if strcmpi(Name, Dict(Idic).Name) || any(strcmpi(Name, Dict(Idic).Alt))
            Code = Dict(Idic).Code;
            break;
        end
    end
end

function Dict = localDictionary
    % The dictionary itself - codes are permanent, do not reuse them

    Entries = {
        %Code Name                                     Alt                                                            Description
          0, 'unknown',                                {},                                                            'Method which is not in the dictionary'
          1, 'imUtil.background.modeVar_Hist',         {'modeVar_Hist'},                                              'Mode and variance from a histogram of the image'
          2, 'imUtil.background.modeVar_LogHist',      {'modeVar_LogHist'},                                           'Mode and variance from a histogram of log(image)'
          3, 'imUtil.background.modeVar_LeftHist',     {'modeVar_LeftHist'},                                          'Mode and variance from the left side of the histogram'
          4, 'imUtil.background.modeVar_QuantileHist', {'modeVar_QuantileHist'},                                      'Mode and variance from a quantile-based histogram'
          5, 'imUtil.background.modeVar_SampleHist',   {'modeVar_SampleHist'},                                        'Mode and variance from a sampled histogram'
          6, 'imUtil.background.modeVar',              {'modeVar'},                                                   'Mode and variance of the image'
         10, 'backBertin',                             {'imUtil.background.mex.backBertin'},                           'Bertin (SExtractor-like) background and variance (mex)'
         11, 'backBertinLowerRMS',                     {'imUtil.background.mex.backBertinLowerRMS'},                   'Bertin background with a lower RMS estimate (mex)'
         20, 'poiss',                                  {},                                                            'Poisson variance: background + RN^2'
         21, 'median',                                 {'imProc.stat.median'},                                        'Median'
         22, 'mean',                                   {'imProc.stat.mean'},                                          'Mean'
         23, 'rmean',                                  {'tools.math.stat.rmean'},                                     'Robust mean'
         24, 'std',                                    {'imProc.stat.std'},                                           'Standard deviation'
         25, 'var',                                    {'imProc.stat.var'},                                           'Variance'
         26, 'rstd',                                   {'tools.math.stat.rstd','imUtil.background.rstd'},              'Robust standard deviation'
         27, 'rstd_mex',                               {'tools.math.stat.mex.rstd1_mex'},                              'Robust standard deviation (mex)'
         28, 'rvar',                                   {'imUtil.background.rvar'},                                     'Robust variance'
         29, 'rvar_mex',                               {},                                                            'Robust variance (mex)'
         30, 'quantile',                               {'imProc.stat.quantile'},                                      'Quantile of the image (default 0.25)'
        };

    Dict = struct('Code', num2cell(uint8([Entries{:,1}])), ...
                  'Name', Entries(:,2).', ...
                  'Alt',  Entries(:,3).', ...
                  'Description', Entries(:,4).');
end

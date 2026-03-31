function HeaderData = extractHeaderData(AI, Args)
    % Extract selected header keywords from AstroImage arrays
    % Description: Reads header keywords from each crop/epoch AstroImage
    %              and stores them as [Nepochs x Ncrop] matrices in a struct.
    %
    % Input  : - AI cell(Nepochs,1) of AstroImage arrays (from loadVisitData).
    %          * ...,key,val,...
    %            'HeaderKeys' - Cell array of FITS header keywords to extract.
    %                        Default is {'FWHM','AIRMASS','EXPTIME','JD','BACK_IM'}.
    %            'Ncrop'   - Number of crops. Default is 24.
    %            'Verbose' - Print progress. Default is false.
    % Output : - HeaderData struct with one field per keyword, each
    %            [Nepochs x Ncrop] matrix. NaN where keyword is missing.
    % Author : D. Kovaleva (Mar 2026)
    % Example: HD = pipeline.last.quality.extractHeaderData(AI);
    %          HD = pipeline.last.quality.extractHeaderData(AI, ...
    %               'HeaderKeys', {'FWHM','AIRMASS','JD'});

    arguments
        AI cell
        Args.HeaderKeys = {'FWHM', 'AIRMASS', 'EXPTIME', 'JD', 'BACK_IM'}
        Args.Ncrop      = 24
        Args.Verbose logical = false
    end

    Nepochs = numel(AI);
    Nkeys = numel(Args.HeaderKeys);

    % Initialize all fields as NaN matrices
    HeaderData = struct();
    for Ik = 1:Nkeys
        FieldName = matlab.lang.makeValidName(Args.HeaderKeys{Ik});
        HeaderData.(FieldName) = nan(Nepochs, Args.Ncrop);
    end

    for Iv = 1:Nepochs
        if isempty(AI{Iv}); continue; end
        for Ic = 1:min(Args.Ncrop, numel(AI{Iv}))
            if isempty(AI{Iv}(Ic).HeaderData); continue; end
            for Ik = 1:Nkeys
                try
                    Val = AI{Iv}(Ic).HeaderData.getVal(Args.HeaderKeys{Ik});
                    if isnumeric(Val) && isscalar(Val) && isfinite(Val)
                        FieldName = matlab.lang.makeValidName(Args.HeaderKeys{Ik});
                        HeaderData.(FieldName)(Iv, Ic) = Val;
                    end
                catch
                    % Skip if keyword not found
                end
            end
        end
        if Args.Verbose
            fprintf('  Epoch %d: headers extracted\n', Iv);
        end
    end
end

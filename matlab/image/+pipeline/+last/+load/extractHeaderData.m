function HeaderData = extractHeaderData(AI, Args)
    % Extract selected header keywords from AstroImage arrays
    % Description: Reads header keywords from each crop/epoch AstroImage
    %              and stores them as [Nepochs x Ncrop] matrices in a struct.
    %
    % Input  : - AI cell(Nepochs,1) of AstroImage arrays (from pipeline.last.load.loadVisitCatHdr).
    %          * ...,key,val,...
    %            'HeaderKeys' - Cell array of FITS header keywords to extract.
    %                        Default is {'FWHM','AIRMASS','EXPTIME','JD','BACK_IM'}.
    %            'Ncrop'   - Number of crops. Default is 24.
    %            'Verbose' - Print progress. Default is false.
    % Output : - HeaderData struct with one field per keyword, each
    %            [Nepochs x Ncrop] matrix. NaN where keyword is missing.
    % Author : D. Kovaleva (Mar 2026)
    % Example: HD = pipeline.last.load.extractHeaderData(AI);
    %          HD = pipeline.last.load.extractHeaderData(AI, ...
    %               'HeaderKeys', {'FWHM','AIRMASS','JD'});

    arguments
        AI cell
        Args.HeaderKeys = {'FWHM', 'AIRMASS', 'EXPTIME', 'JD', 'BACK_IM', 'PT_ARMS', 'PH_RMS'}
        Args.Ncrop      = 24
        Args.Verbose logical = false
    end

    Nepochs = numel(AI);
    Nkeys = numel(Args.HeaderKeys);

    % Precompute valid-name mapping once
    FieldNames = cell(1, Nkeys);
    for Ik = 1:Nkeys
        FieldNames{Ik} = matlab.lang.makeValidName(Args.HeaderKeys{Ik});
    end

    % Initialize all fields as NaN matrices
    HeaderData = struct();
    for Ik = 1:Nkeys
        HeaderData.(FieldNames{Ik}) = nan(Nepochs, Args.Ncrop);
    end

    for Iv = 1:Nepochs
        if isempty(AI{Iv}); continue; end
        % Batched header read: one call returns [1 x Ncrop_v] struct array
        try
            St = AI{Iv}.getStructKey(Args.HeaderKeys);
        catch
            continue;
        end
        Ncrop_v = min(Args.Ncrop, numel(St));
        for Ic = 1:Ncrop_v
            for Ik = 1:Nkeys
                V = St(Ic).(Args.HeaderKeys{Ik});
                if isnumeric(V) && isscalar(V) && isfinite(V)
                    HeaderData.(FieldNames{Ik})(Iv, Ic) = V;
                end
            end
        end
        if Args.Verbose
            fprintf('  Epoch %d: headers extracted\n', Iv);
        end
    end
end

function MatVersion = saveMat(FileName, S, MatVersion, MaxBytes)
    % Save the fields of a scalar struct as variables in a MAT file, verified.
    %   save() only warns, and writes nothing for the variable, when a variable
    %   is too large for the requested MAT version (2 GB for '-v7' and '-v6'),
    %   and warnings are off in AstroPack. The file is therefore read back
    %   with whos('-file'), and if a variable is missing it is saved again as
    %   '-v7.3'. For objects save() errors instead ("could not be closed",
    %   issue #1447), so a variable estimated to be larger than MaxBytes is
    %   saved as '-v7.3' directly, and a failed save is repeated as '-v7.3'.
    %   Used by io.files.saveAsync and imProc.io.saveProductMat.
    % Input  : - Output MAT file name.
    %          - Scalar struct. Each field becomes a variable in the file.
    %          - MAT-file version flag for save(), e.g. '-v7', '-v7.3',
    %            {'-v7','-nocompression'}. Default is '-v7.3'.
    %          - Size [bytes] of a variable above which '-v7.3' is used.
    %            See io.files.chooseMatVersion. Default is 1.8e9.
    % Output : - The MAT-file version actually written.
    % Author : A.M. Krassilchtchikov (Oct 2026)
    % Example: S.A = rand(10); io.files.saveMat('A.mat', S, '-v7');

    arguments
        FileName
        S(1,1) struct
        MatVersion = '-v7.3';
        MaxBytes   = 1.8e9;
    end

    MatVersion = io.files.chooseMatVersion(S, MatVersion, MaxBytes);

    try
        save(FileName, '-struct', 'S', MatVersion{:});
    catch ME
        if any(strcmp(MatVersion, '-v7.3'))
            rethrow(ME);
        end
        MatVersion = {'-v7.3'};
        save(FileName, '-struct', 'S', MatVersion{:});
    end

    Missing = missingVars(FileName, S);
    if ~isempty(Missing)
        if any(strcmp(MatVersion, '-v7.3'))
            error('io:files:saveMat:NotSaved', 'Variable(s) %s not saved in %s', strjoin(Missing, ', '), FileName);
        end
        MatVersion = {'-v7.3'};
        save(FileName, '-struct', 'S', MatVersion{:});
        Missing = missingVars(FileName, S);
        if ~isempty(Missing)
            error('io:files:saveMat:NotSaved', 'Variable(s) %s not saved in %s', strjoin(Missing, ', '), FileName);
        end
    end
    MatVersion = strjoin(MatVersion, ' ');
end


function Missing = missingVars(FileName, S)
    % Names of the fields of S that are not variables in the MAT file
    W       = whos('-file', FileName);
    Missing = setdiff(fieldnames(S), {W.name});
end

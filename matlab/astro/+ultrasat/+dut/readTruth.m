function T = readTruth(File, Args)
    % The truth catalog of a DUT image: the star rows of its star image, then its own rows (the transients)
    %   A new exposure written with Cfg.New.StarTruth 'pool' keeps only its
    %   transients in its truth catalog; the keyword STARCAT of that catalog
    %   names the star truth catalog of its star image (in the same
    %   directory), whose rows are put first. The result is the same table
    %   as a truth catalog written with all sources (StarTruth 'image').
    %   A truth catalog without STARCAT (the references, the earlier sets,
    %   a star truth catalog itself) is returned as it is.
    % Input  : - File: the truth catalog (Product Cat) or its image (Product
    %               Image: the truth catalog name is made from it); a .mat
    %               with the variable Truth (the star truth of the earlier
    %               sets) is also read.
    %          * ...,key,val,...
    %            'Stars' - join the star truth catalog named by STARCAT.
    %                   Default is true.
    %            'VersionTruth' - the last part of the truth Version, for
    %                   an image File (as Cfg.Naming.VersionTruth). Default is 0.
    % Output : - T: the truth table (columns as ultrasat.dut.simulateTile);
    %               T.Properties.UserData.StarCat is the star truth catalog
    %               ('' if none).
    % Author : D. Kovaleva (Nov 2026)
    % Example: T = ultrasat.dut.readTruth(ImageFile);
    %          Tr = T(T.TransientID > 0, :);

    arguments
        File (1,:) char
        Args.Stars (1,1) logical = true
        Args.VersionTruth (1,1) double {mustBeNonnegative, mustBeInteger} = 0
    end

    [Dir, Name, Ext] = fileparts(File);
    if strcmp(Ext, '.mat')
        T = io.files.load2(File, 'Truth');
        T.Properties.UserData.StarCat = '';
    else
        if contains(Name, '_Image_')
            Name = regexprep(Name, '_Image_(.+)-\d+$', sprintf('_Cat_$1-%d', Args.VersionTruth));
            File = fullfile(Dir, [Name Ext]);
        end
        T = readCat(File);
        Star = starCatKey(File);
        if Args.Stars && ~isempty(Star)
            S = readCat(fullfile(Dir, Star));
            T = [S; T];
        end
        T.Properties.UserData.StarCat = Star;
    end
end

function T = readCat(File)
    % a FITS truth table as a MATLAB table, with its units
    A = FITS.readTable1(File);
    if istable(A)
        T = A;
    else
        T = array2table(A.Catalog, 'VariableNames', A.ColNames);
        if ~isempty(A.ColUnits) && numel(A.ColUnits) == width(T)
            T.Properties.VariableUnits = cellstr(A.ColUnits);
        end
    end
end

function Star = starCatKey(File)
    % the value of STARCAT in the table header ('' if absent)
    I = fitsinfo(File);
    K = I.BinaryTable(1).Keywords;
    J = find(strcmp(K(:,1), 'STARCAT'), 1);
    Star = '';
    if ~isempty(J)
        Star = strtrim(char(K{J,2}));
    end
end

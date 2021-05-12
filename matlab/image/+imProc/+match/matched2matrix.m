function [Res, Summary, N_Ep] = matched2matrix(MatchedObj, Cols, IsEpochByInd)
    %A matched AstroCatalog object into a matrix of epochs by index
    % AstCat object to a matrix of matched sources.
    % Description: Given an AstroCatalog object containing multiple elements, in
    %              which each element containing the same number of rows
    %              (e.g., the output of Match/match.m), return a matrix
    %              that contains, for selected columns, the requested column
    %              in all the AstroCatalog elements. E.g., the number of columns
    %              in the matrix is equal to the number of AstroCatalog elements
    %              (column per element) and the number of rows is equal to
    %              the number of rows in each AstroCatalog element.
    % Input  : - An AstroCatalog object containing multiple element, in
    %            which each element containing the same number of rows
    %            (e.g., the output of AstCat/match.m)
    %          - Column indices or column names (string or a cell array of
    %            strings) for which to prepare the array.
    %          - A logical indicating if the ouput matrix is
    %            Epoch by Ind (true), or Ind by Epoch (false).
    %            Default is true.
    % Output : - A structure in which each field name corresponds to a
    %            requested column name and contains the matrix of all the
    %            column entries in all the AStroCatalog object elements.
    %          - A structure array containing a summary.
    %            The following fields are available:
    %            .Nsrc - Number of sources (size of the 1st dimension of the
    %                    output matrix).
    %            .Nepoch - Number of epochs (size of the 2nd dimension of the
    %                    output matrix).
    %            .Nnn - number of not NaNs for each source in the first Column.
    %          - Vector of length equal to the number of epochs. The value in
    %            each element is the number of stars that appears in exactly
    %            I epochs.
    % License: GNU general public license version 3
    % Tested : Matlab R2015b
    %     By : Eran O. Ofek                    May 2016
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: 
    %          FFU : MODIFY!!! and TEST!!!

    %          M=match(Ref,AstC);
    %          Res=astcat2matched_array(M,{'XWIN_IMAGE','YWIN_IMAGE','MAG_APER'});
    %          % step by step application:
    %          S=images2sim('PTF_2015*.fits');
    %          S=gain_correct(S);
    %          S=background(S,'Block',[64 64]);
    %          S=mextractor(S);
    %          [~,I]=max(sizecat(S)); 
    %          Sref = S(I);
    %          [M,UM]=match(S,Sref);
    %          [Res,Summary]=astcat2matched_array(M,{'MAG_PSF'});
    %          II=find(Summary.Nnn==Summary.Nepoch); % find sources that appears in all epochs
    %          Res.MAG_PSF(:,II)
    % Reliable: 2
    %--------------------------------------------------------------------------

    arguments
        MatchedObj   % FFU:  what about AstroImage???
        Cols
        IsEpochByInd(1,1) logical           = true;
    end

    CatField   = 'Catalog';

    Ncat        = numel(MatchedObj);
    [CatRow,~]  = sizecat(MatchedObj);
    if all(CatRow(1)==CatRow)
        Nrow = CatRow(1);
    else
        error('Number of rows in all MatchedObj elemnts must be equal');
    end

    if char(Cols)
        Cols = {Cols};
    end

    % get column names and indices
    ColInd  = colname2ind(MatchedObj(1),Cols);
    ColName = ind2colname(MatchedObj(1),Cols);
    Ncol    = numel(ColInd);

    % Initiate Res
    for Icol=1:1:Ncol
        Res.(ColName{Icol}) = zeros(Nrow,Ncat);
    end

    if IsEpochByInd
        DimSrc   = 2;
        DimEpoch = 1;
    else
        DimSrc   = 1;
        DimEpoch = 2;
    end

    % For each selected column
    for Icol=1:1:Ncol
        % for each catalog
        for Icat=1:1:Ncat
            % E.g., Res.XWIN_IMAGE(:,10) = MatchedObj(10).Cat(:,Column)
            Res.(ColName{Icol})(:,Icat) = MatchedObj(Icat).(CatField)(:,ColInd(Icol));
        end
        if IsEpochByInd
            % transpose the matrix
            Res.(ColName{Icol}) = Res.(ColName{Icol}).';
        end
    end


    if (nargout>1)
        % calculate summary
        Icol = 1;
        Summary.Nsrc   = size(Res.(ColName{Icol}),DimSrc);
        Summary.Nepoch = size(Res.(ColName{Icol}),DimEpoch);
        Summary.Nnn    = sum(~isnan(Res.(ColName{Icol})),DimEpoch);

        if (nargout>2)
            % calculate the number of stars that appears in N images
            N_Ep = zeros(Summary.Nepoch,1);
            for Iep=1:1:Summary.Nepoch
                % Summary.Nnn is the number of epochs in each stars appears
                % N_Ep is the number of stars that appears in exactly Iep
                % epochs.
                N_Ep(Iep) = sum(Summary.Nnn==Iep);
            end
        end
    end

end

function [Val, Key, Comment, Nfound] = getValBySynonym(CellHeader, KeySynonym, Args)
    % Return the first key/val in the list of synonyms that appears in the cell-header.
    % Input  : - A 3 column cell array of {Key, Val, Comment}
    %          - A cell array of synonyms to search in the cell key column.
    %          * ...,key,val,...
    %           'CaseSens'     - Case sensetive search. Default is true.
    %           'SearchAlgo'   - ['strcmp'] | 'regexp'
    %           'Occur'        - For each synonym return the ['first'] 
    %                   or 'last' match.
    %           'Fill' - Fill value for the keyword Val, in case that the
    %                   key is not found, or is found with an undefined
    %                   (blank) value. Default is NaN (comment will be
    %                   '', except for an undefined value, for which the
    %                   comment is returned).
    %           'Val2Num' - Attempt to convert the value to numeric.
    %                   Default is true.
    %           'ColKey' - Keywords column. Default is 1.
    %           'ColVal' - Keywords column. Default is 2.
    %           'ColCommeny' - Keywords column. Default is 3.
    % Output : - Keyword value.
    %          - Keyword name.
    %          - Keyword comment.
    %          - For the synonym found, number of matches.
    % Author : Eran Ofek (Apr 2021)
    % Example: [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''}, {'C','A'})
    %          [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''}, {'A','C'})
    %          [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''; 'A',2,''}, {'A','C'})
    %          [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''}, {'A','C'},'SearchAlgo','regexp')
    %          [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''; 'A',2,''}, {'Aaa','Caa'})
    %          [Val, Key, Comment, Nfound] =
    %          imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''}, []) % return Args.Fill
    
    arguments
        CellHeader(:,3) cell
        KeySynonym
        Args.CaseSens(1,1) logical           = true;
        Args.SearchAlgo char {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp';
        Args.Occur                           = 'first';  % 'first' | 'last'
        Args.Fill                            = NaN;
        Args.Val2Num(1,1) logical            = true;
        Args.ColKey                          = 1;
        Args.ColVal                          = 2;
        Args.ColComment                      = 3;
    end
    
    if isempty(KeySynonym)
        IndSyn        = [];
        KeySynonym{1} = Args.Fill;
        Nfound        = 0;
    else
        
        if ischar(KeySynonym)
            KeySynonym = {KeySynonym};
        end
        Nsyn = numel(KeySynonym);


        switch Args.SearchAlgo
            case 'strcmp'
                if Args.CaseSens
                    strcmpFun = @strcmp;
                else
                    strcmpFun = @strcmpi;
                end
            case 'regexp'
                if Args.CaseSens
                    strcmpFun = @(x,y) ~tools.cell.isempty_cell(regexp(x,y,'match'));
                else
                    strcmpFun = @(x,y) ~tools.cell.isempty_cell(regexpi(x,y,'match'));
                end
            otherwise
                error('Unknown SearchAlgo option');
        end

        IndSyn  = [];
        IndCell = [];
        Nfound  = 0;
        Cont = true;
        Isyn = 0;
        while Cont
            Isyn   = Isyn + 1;
            Flag   = strcmpFun(CellHeader(:,Args.ColKey), KeySynonym{Isyn});
            Nfound = sum(Flag);
            if any(Flag)
                IndSyn  = Isyn;
                IndCell = find(Flag, 1, Args.Occur);
            end
            if ~isempty(IndSyn) || Isyn==Nsyn 
                Cont = false;
            end
        end
    end

    if isempty(IndSyn)
        Key     = KeySynonym{1};
        Val     = Args.Fill;
        Comment = '';
    else
        Key     = CellHeader{IndCell, Args.ColKey};
        Val     = CellHeader{IndCell, Args.ColVal};
        Comment = CellHeader{IndCell, Args.ColComment};
        
        % An undefined (blank) value - the FITS representation of a keyword that
        % carries no value, e.g. a NaN written by the mex header writers (#1194) -
        % reads back as an empty char (io.fits.mex.read_header) or an empty
        % logical (FITS.readHeader1). Treat it as a missing value, so that a
        % normally-numeric keyword stays numeric instead of poisoning any
        % concatenation of values over several headers.
        if isempty(Val)
            Val = Args.Fill;

        % convert to number
        elseif Args.Val2Num && ischar(Val)
            MatchedND = regexp(Val, '[^\d]', 'match');
            if isempty(MatchedND)
                %ValNum  = str2double(Val);
                ValNum  = real(str2doubleq(Val));  % faster
                if isnan(ValNum) && ~strcmpi(Val,'nan')
                    % string
                    % do nothing
                else
                    % number
                    Val = ValNum;
                end
            else
                % can't convert
            end
        end
        
    end
    
    
end

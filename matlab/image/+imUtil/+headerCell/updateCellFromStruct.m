function C=updateCell_fromStruct(S,C,Args)
% update cell header by adding elements in struct not in header
% Description: Given a 3 column cell array (i.e., cell header)
%              and a structure with keywords and values.
%              First make sure all the header keywords have no
%              illegal characters. Then look for fields in the
%              structure that are not in the cell header and
%              add them to the cell header.
% Input  : - A structure.
%          - A cell header.
% Output : - An updated cell header.
% Example: C=imUtil.headerCell.updateCellFromStruct(struct('A',1,'B',2'),{'D',1;'END',''})
% C=imUtil.headerCell.updateCellFromStruct(struct('A',1,'B',2'),{'A',2;'D',1;'END',''})

    arguments
        S struct
        C cell             = cell(0,3);
        Args.Pos           = 'end-1';
    end

    Col = 1;
    FN   = fieldnames(S);
    Nfs  = numel(FN);  % number of fields in struct
    Nkey = size(C,1);
    
    % Look for new Key values that donot appear in Header
    Cl = imUtil.headerCell.replace_illegal_char(C);
    [NewKey, NewKeyInd] = setdiff(FN,Cl(:,Col));
    SC        = struct2cell(S);
    NewHeader = [NewKey(:), SC(NewKeyInd)];
    %NewHeader = headCl.fixColCell(NewHeader);

    C = imUtil.headerCell.insertKey(C,NewHeader,Args.Pos);

end
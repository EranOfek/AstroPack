function Cell=struct2keyval(St,TwoCol)
% Convert a structure into a cell array of key,val,...
% Package: +tools.struct
% Input  : - A structure.
%          - A logical indicating if the output is a row cell vector
%            (false), or a two column cell array [key, val] (true).
%            Default is false.
% Output : - A cell array of key,val, in which the even elements are field
%            names, and the corresponding odd elements are values.
% Example : St.A = 1; St.B = 2; C=tools.struct.struct2keyval(St);

arguments
    St
    TwoCol        = false;
end

Key = fieldnames(St);
Val = struct2cell(St);

Cell = [Key(:).'; Val(:).'];
Cell = Cell(:).';

if TwoCol
    Cell = reshape(Cell,2,numel(Cell).*0.5).';
end
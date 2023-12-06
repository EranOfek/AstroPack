function y = str2doubles (cs)
%STR2DOUBLES:  Faster alternative to builtin str2double

    if ischar(cs),  y = str2double(cs);  return;  end
    siz = size(cs);
    cs = cs(:);
    cs = deblank(cs);  % (it changes the shpe of 3d input)    
    idx = ~cellfun(@isempty, cs);
    cs2 = cs(idx);
    y2 = sscanf(sprintf('%s#', cs2{:}), '%g#');  % faster
    %y2 = cellfun(@(csi) sscanf(csi, '%g#'), cs2);  % slower
    y = NaN(siz);
    y(idx) = y2;
end

%!test
%! assert(str2doubles('123.45e7') == 123.45e7)
%! assert(str2doubles('123 + 45i') == (123 + 45i))
%! assert(str2doubles('3.14159') == 3.14159)
%! assert(str2doubles('2.7i - 3.14') == (2.7i - 3.14))
%! assert(isequal(str2doubles({'2.71' '3.1415'}), [2.71  3.1415]))
%! assert(str2doubles('1,200.34') == 1200.34)
%! assert(isequaln(str2doubles({'1', ' ', '3'}), [1, NaN, 3]))
%! assert(isequaln(str2doubles(reshape({'1', '2'}, [1,1,2])), reshape([1, 2], [1,1,2])))

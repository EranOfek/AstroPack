function c = sq_nv(s)
%SQ_NV  Struct of unmatched parameters -> {name, value, ...} for forwarding.
f = fieldnames(s);
c = cell(1, 2*numel(f));
for k = 1:numel(f)
    c{2*k-1} = f{k};
    c{2*k}   = s.(f{k});
end
end

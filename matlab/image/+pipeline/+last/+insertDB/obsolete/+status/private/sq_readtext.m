function txt = sq_readtext(file)
%SQ_READTEXT  Read a whole text file as UTF-8 char (private helper).
fid = fopen(file, 'r');
if fid < 0
    error('sqstatus:io', 'Cannot open "%s" for reading.', file);
end
c = onCleanup(@() fclose(fid));
bytes = fread(fid, Inf, '*uint8').';
if isempty(bytes)
    txt = '';
else
    txt = native2unicode(bytes, 'UTF-8');
end
end

function Files = dirSortedByDate(Str)
    % dir command, where the files are sorted by date
    % Input  : - If empty, use dir, if string then use dir(string),
    %            if struct, then assume this is the dir ouput.
    % Output : - Dir output sorted by time.
    % Author : Eran Ofek (Jan 2022)
    % Example: Files = io.files.dirSortedByDate
    %          Files = io.files.dirSortedByDate('*.fits')
    
    if nargin==0
        Files = dir;
    else
        if isstruct(Str)
            Files = Str;
        else
            Files = dir(Str);
        end
    end
    
    
    [~,SI] = sort([Files.datenum]);
    Files  = Files(SI);
end

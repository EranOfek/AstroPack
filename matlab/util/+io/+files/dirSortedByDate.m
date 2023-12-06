function Files = dirSortedByDate(Str)
    % dir command, where the files are sorted by date
    % Author : Eran Ofek (Jan 2022)
    % Example: Files = io.files.dirSortedByDate
    %          Files = io.files.dirSortedByDate('*.fits')
    
    if nargin==0
        Files = dir;
    else
        Files = dir(Str);
    end
    
    [~,SI] = sort([Files.datenum]);
    Files  = Files(SI);
end

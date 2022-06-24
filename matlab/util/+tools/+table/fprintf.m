function fprintf(FID, Format, Table, varargin)
    % fprintf for tables
    % Input  : - File identifier. Use fopen to generate.
    %          - Format. See fprintf for options.
    %          - Table
    %          * Additional arguments to pass to fprintf.
    % Output : null
    % Author : Eran Ofek (Jun 2022)
    % Example: tools.table.fprintf(FID, '%d %d',Table);
    
    [N, Ncol] = size(Table);
    
    for I=1:1:N
        Cell = table2cell(Table(I,:));
        fprintf(FID, Format, Cell{:}, varargin{:});
    end    

end
function writeCSVforBulkInjection(AH,FileName,Args)
    % write an AstroHeader to a csv text file
    % Input  : - An AstroHeader object or a vector of AH objects
    %          - name of the file to write to
    %        * ...,key,val,...
    %        'Append'   - append to an existing CSV file (no need to
    %                     make a new file and write a line with column names
    %        'Delimiter' - field delimiter
    %        'Filter'    - whether to remove the fields not present in the DB table
    %        'FiltrList' - a cell array of the DB table fields to match
    % Output : - a csv file
    % Author : A. Krassilchtchikov (Feb 2024)
    % Example: ColumnList = Obj.getListOfTableColumns(Table); % Obj is of db.AstroDb, Table is the name of a particular DB table
    %          imProc.header.writeCSVforBulkInjection(AH, HeaderFileName,'Filter',true,'FilterList',ColumnList);
    arguments
        AH
        FileName             = 'astroheader.csv' % output file name
        Args.Append logical  = false % append or overwrite
        Args.Delimiter       =  ',' % '\t' is tab
        Args.Filter  logical = false
        Args.FilterList      = {}
    end

    AH1 = AH.copy;

    Nobj = length(AH1);
    Keys = [AH1.Data];
    Keys = reshape(Keys,[size(Keys,1),3,Nobj]);

    % clear out repeating keywords
    [~,Ind,~] = unique(Keys(:,1,1),'stable');
    Keys = Keys(Ind,:,:);

    % keep only the keywords from the FilterList
    if Args.Filter
        Ind = ismember(Keys(:,1,1), upper(Args.FilterList'));
        Keys = Keys(Ind,:,:);
    end

    % if not appending, start with a line with keywords
    if ~Args.Append
        FirstLine = Keys(:,1,1)';
        writecell(FirstLine,FileName,'Delimiter',Args.Delimiter);
    end

    Keys = squeeze(Keys(:,2,:));
    writecell(Keys',FileName,'Delimiter',Args.Delimiter,'WriteMode','append');

end
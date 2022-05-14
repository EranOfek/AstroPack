function generateFunListWebPage(Args)
    %
    % Example: tools.code.generateFunListWebPage
   
    arguments
        Args.WebFileName    = 'funlist.html';
    end
    
    List = tools.code.getAllFun;
    TableCell = [{List.FunName}.', {List.FunFullName}.', {List.ClassName}.', {List.Year}.', {List.DescriptionLine}.'];
    Header = {'Fun Name', 'Full Name', 'Class', 'Year', 'Brief description'};
    
    www.html_table(Args.WebFileName,'TableCell',TableCell,'TableStatment','class="sortable"','TableHead',Header);

    % add sortable javascript
    FileStr = io.files.file2str(Args.WebFileName, 'str');
    Comment = sprintf('<b>Table is sortable - click on column to sort.</b><br> IsStatic indicate if the function is a static class.<br> Number of functions: %d.<br> Last update: %s.<br>',numel(List),date);
    FileStr = sprintf('<script src="sorttable.js"></script>\n\n %s\n<br>\n %s',Comment,FileStr);
    FID = fopen(Args.WebFileName,'w');
    fprintf(FID,'%s',FileStr);
    fclose(FID);
    
end

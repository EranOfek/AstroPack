function generateFunListWebPage(Args)
    % Generate an html file with sortable table of all functions and methods in AstroPack.
    % Input  : 'WebFileName' - html file name to write.
    %               Default is 'AstroPack_funlist.html'
    % Author : Eran Ofek (Jan 2023)
    % Example: tools.code.generateFunListWebPage
   
    arguments
        Args.WebFileName    = 'AstroPack_funlist.html'; 
    end
    
    List = tools.code.getAllFun;

    Header = {'Function', 'Full Name', 'Class', 'Year', 'Mon', 'Brief description'};
        
    Ncell = numel(List);
        for Icell=1:1:Ncell
            % a local machine link:
            List(Icell).Link = sprintf('%s%s%s%s%s', '<a href="', List(Icell).FullPath,'">',List(Icell).FunName,'</a>');
            % relink to Github:
            List(Icell).Link = regexprep(List(Icell).Link, 'href=".*AstroPack/','href="https://github.com/EranOfek/AstroPack/tree/dev1/'); 
        end
        
    TableCell = [{List.Link}.', {List.FunFullName}.', {List.ClassName}.', {List.Year}.', {List.Month}.', {List.DescriptionLine}.'];
        
    www.html_table(Args.WebFileName,'TableCell',TableCell,'TableStatment','class="sortable"','TableHead',Header);

    % add sortable javascript (do not forget to put the sorttable.js file)
    % into the same catalog as AstroPack_funlist.html 
    FileStr = io.files.file2str(Args.WebFileName, 'str');
    
    Comment = sprintf('<b>The table is sortable: click on a column header to sort</b> <br> Number of functions: %d<br> Last update: %s<br>',numel(List),date);
    FileStr = sprintf('<script src="sorttable.js"></script>\n\n %s\n<br>\n %s',Comment,FileStr);
   
    FID = fopen(Args.WebFileName,'w');
    fprintf(FID,'%s',FileStr);
    fclose(FID);
    
end

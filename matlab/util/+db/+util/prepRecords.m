function Res = prepRecords(Obj, Args)
    % convert a stack of AI/AH/AC into a struct of DB records for bulk injection
    %     Optional detailed description
    % Input  : - a stack of objects
    %          * ...,key,val,... 
    %        'Level'      - data level ('raw','proc','coadd')
    %        'OutputType' - type of output 
    %        'CSVname'    - name of the output CSV file
    %        
    % Output : - a structure array containing the record data  
    %          - a CSV file (option)
    % Author : A.M. Krassilchtchikov (2024 Sep) 
    % Example:
    % 
    arguments
        Obj
        Args.Level      = 'raw';
        Args.OutputType = 'csv';
        Args.CSVname    = 'preparedRecords.csv';
    end
    %
    if strcmpi(class(Obj),'astroimage')
        % extract the headers, convert some of the keywords
        
    elseif strcmpi(class(Obj),'astrocatalog')
        % dump the catalog into a csv 
        
    else
        error('Incorrect input object class')    
    end

end

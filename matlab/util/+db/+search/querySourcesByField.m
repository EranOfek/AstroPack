function [Result] = querySourcesByField(Fields, Args)
    % One line description
    %     Optional detailed description
    % Input  : - a list of field names (string array) 
    %          - 
    %          * ...,key,val,... 
    % Output : - a query string 
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: Start = celestial.time.date2jd([2025, 04, 18]);
    %          Q = db.search.querySourcesByField(["1678"],'Mount',3,'Camera',2,'Crop',13,'JDstart',Start)
    %          Result = DB.query(Q);
    arguments
        Fields
        Args.JDstart           = 0;
        Args.JDstop            = 1e7;
        Args.Mount             = [];
        Args.Camera            = [];
        Args.Crop              = [];
              
        Args.ImageTable        = 'visit_images';
        Args.ImageID           = 'id_visit'
        Args.SourceTable       = 'visit_src';
        Args.ImageIDSrcTab     = 'id_visit_im'
    end
    %             
    F = "( 1<0";
    for Ifield = 1:numel(Fields)
        Add = sprintf(" or fieldid = '%s' ",Fields(Ifield));
        F   = strcat(F,Add);
    end
    F   = strcat(F,") ");
    
    Jd   = "";
    Mt   = "";
    Cam  = "";
    Crop = "";
    if Args.JDstart > 0 || Args.JDstop < 3e6         
        Jd = sprintf("and (jd_start > %e and jd_start < %e) ",Args.JDstart,Args.JDstop); 
    end    
    if ~isempty(Args.Mount)
        Mt = sprintf(" and ( mountnum = %d )",Args.Mount);
    end   
    if ~isempty(Args.Camera)
        Cam = sprintf(" and ( camnum = %d ) ",Args.Camera);
    end        
    if ~isempty(Args.Crop)
        Crop = sprintf(" and ( cropid = %d ) ",Args.Crop);
    end
    
    W = strcat(F,Jd,Mt,Cam,Crop);
    
    Result = sprintf("SELECT s.* FROM %s AS s INNER JOIN %s AS i " +...
        "ON s.%s = i.%s where %s",Args.SourceTable,Args.ImageTable,...
        Args.ImageIDSrcTab,Args.ImageID,W);

end
    

%     Q = sprintf('SELECT s.* FROM visit_src AS s INNER JOIN visit_images AS i ON s.id_visit_im = i.id_visit where %s',W);
%     
%  
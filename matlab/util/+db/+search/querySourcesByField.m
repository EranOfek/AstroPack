function [Result] = querySourcesByField(Fields, Args)
    % Given the Field name(s), construct a query to search a source DB table on the fields 
    %     with optional constraints on time (JD), mount, camera, subimage 
    % Input  : - a list of field names (an array of strings) 
    %          * ...,key,val,... 
    %        'JDstart' - the star JD of the search
    %        'JDstop'  - the end JD of the search
    %        'Mount'   - the mount number
    %        'Camera'  - the camera number
    %        'Crop'    - the crop number
    %        'MinMag'  - the minimal magnitude
    %        'MagMax'  - the maximal magnitude
    %        'MagColumnName' - the name of the mag column in the DB src table
    %        'ImageTable' - the name of the image table used to fiter the original images 
    %        'ImageID'    - the image ID column in the image table
    %        'SourceTable'- the source table 
    %        'ImageIDSrcTab' - the matching image ID column in the source table
    %        'Method'  - 'hpix' via healpix (approximate, but faster) or 'image' - via matching with image table (slower, more accurate) 
    % Output : - a query string 
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: Start = celestial.time.date2jd([2025, 04, 18]);
    %          D = db.Db; D.User = ..; D.Password = .., D.useDB('last');
    %          Q = db.search.querySourcesByField(["1678"],'Mount',3,'Camera',2,'Crop',13,'JDstart',Start,'MaxMag',19,'DB',D)
    %          Result = DB.query(Q);
    arguments
        Fields
        Args.JDstart           = 0;
        Args.JDstop            = 1e7;
        Args.Mount             = [];
        Args.Camera            = [];
        Args.Crop              = [];
        Args.MinMag            = 0;
        Args.MaxMag            = 50;
        Args.MagColumnName     = 'mag_aper_3';      
        
        Args.ImageTable        = 'visit_images';
        Args.ImageID           = 'id_visit'
        Args.SourceTable       = 'visit_src';
        Args.ImageIDSrcTab     = 'id_visit_im'
        
        Args.Method            = 'hpix'; % 'hpix' via healpix (approximate, but faster) or 'image' - via matching with image table (slower, more accurate) 
        Args.NsideLow          = 2^8;
        Args.NsideHigh         = 2^16;
        Args.DB                = [];     % a DB object
    end
    %                 
    Jd   = "";
    Mt   = "";
    Cam  = "";
    Crop = "";
    if ~isempty(Args.Mount)
        Mt = sprintf(" and ( mountnum = %d )",Args.Mount);
    end   
    if ~isempty(Args.Camera)
        Cam = sprintf(" and ( camnum = %d ) ",Args.Camera);
    end        
    if ~isempty(Args.Crop)
        Crop = sprintf(" and ( cropid = %d ) ",Args.Crop);
    end
    if Args.MinMag > 0 || Args.MaxMag < 50
        Mag = sprintf("and ( %s > %e and %s < %e) ",Args.MagColumnName,Args.MinMag,Args.MagColumnName,Args.MaxMag);
    end
    %
    if strcmpi(Args.Method,'image')
        if Args.JDstart > 0 || Args.JDstop < 3e6
            Jd = sprintf("and (jd_start > %e and jd_start < %e) ",Args.JDstart,Args.JDstop);
        end
        %
        F = "(1<0";
        for Ifield = 1:numel(Fields)
            Add = sprintf(" or fieldid = '%s' ",Fields(Ifield));
            F   = strcat(F,Add);
        end
        F = strcat(F,") ");
        %
        W = strcat(F,Jd,Mt,Cam,Crop,Mag);
        %
        Result = sprintf("SELECT s.* FROM %s AS s INNER JOIN %s AS i " +...
            "ON s.%s = i.%s where %s",Args.SourceTable,Args.ImageTable,...
            Args.ImageIDSrcTab,Args.ImageID,W);
    % 
    elseif strcmpi(Args.Method,'hpix')        
        if Args.JDstart > 0 || Args.JDstop < 3e6
            Jd = sprintf("and (jd > %e and jd < %e) ",Args.JDstart,Args.JDstop);
        end
        %       
        F = "(1<0";
        for Ifield = 1:numel(Fields)
            Q = sprintf("select top 1 upix_low from %s where fieldid = '%s'", Args.ImageTable, Fields(Ifield)); % is it correct to take only 1 line?
            Res  = Args.DB.query(Q);
            [~,Ipix] = celestial.healpix.uniqueId2pix(Args.NsideLow,Res.upix_low); % convert Uniq to Ipix
            Neighb = celestial.healpix.neighbors(Ipix,Args.NsideLow,'IncludeSelf',true);            
            for In = 1:numel(Neighb)
                UpixHigh = celestial.healpix.increasePixelResolution(Neighb(In),Args.NsideLow,Args.NsideHigh); 
                UniqHigh = celestial.healpix.pix2uniqueId(Args.NsideHigh,UpixHigh); % convert Ipix to Uniq 
                F = strcat(F, sprintf(" or ( upix_high < %d and upix_high > %d ) ",UniqHigh(end),UniqHigh(1)) );
            end
        end
        F = strcat(F,") ");
        W = strcat(F,Jd,Mt,Cam,Crop,Mag);
        %
        Result = sprintf("SELECT * FROM %s where %s",Args.SourceTable, W);
    end 
end
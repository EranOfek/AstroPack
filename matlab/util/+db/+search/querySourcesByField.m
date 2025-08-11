function [Result] = querySourcesByField(Fields, Args)
    % Given the Field name(s), construct a query to search a source DB table on the fields 
    %     with optional constraints on time (JD), mount, camera, subimage 
    % Input  : - a list of field names (an array of strings) 
    %          * ...,key,val,... 
    %        'JDstart' - the start JD of the search
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
    Mag  = "";
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
        Mag = sprintf("and ( %s > %.3f and %s < %.3f) ",Args.MagColumnName,Args.MinMag,Args.MagColumnName,Args.MaxMag);
    end
    % method 1
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
    % method 2
    elseif strcmpi(Args.Method,'hpix')        
        if Args.JDstart > 0 || Args.JDstop < 3e6
            Jd = sprintf("and (jd > %e and jd < %e) ",Args.JDstart,Args.JDstop);
        end
        %       
        F = "(1<0";
        for Ifield = 1:numel(Fields)
            Fid = sprintf("fieldid = '%s'",Fields(Ifield));
            W = strcat(Fid,Mt,Cam,Crop);
            Q = sprintf("select upix_low from %s where %s", Args.ImageTable, W); 
            Res  = Args.DB.query(Q);
            [~,Ipix] = celestial.healpix.uniqueId2pix(Args.NsideLow,unique(Res.upix_low)); % convert Uniq to Ipix
            for Ip = 1:numel(Ipix)
                Neighb = celestial.healpix.neighbors(Ipix(Ip),Args.NsideLow,'IncludeSelf',true); % find 8 neighbors 
%                 Neighb = Ipix(Ip); % test
                for In = 1:numel(Neighb)
                    UpixHigh = celestial.healpix.increasePixelResolution(Neighb(In),Args.NsideLow,Args.NsideHigh); % convert to NsideHigh
                    UniqHigh = celestial.healpix.pix2uniqueId(Args.NsideHigh,UpixHigh);                            % convert Ipix back to Uniq
                    F = strcat(F, sprintf(" or ( upix_high < %d and upix_high > %d ) ",UniqHigh(end),UniqHigh(1)) );
                end
            end
        end
        F = strcat(F,") ");
        W = strcat(F,Jd,Mag);
        %
        Result = sprintf("SELECT * FROM %s where %s",Args.SourceTable, W);
    else 
        error('Unknown method');
    end 
end
%
function test 
    Start = celestial.time.date2jd([2025, 04, 18]); 
    
    Q1 = db.search.querySourcesByField(["1678"],'Mount',3,'Camera',2,'Crop',13,'JDstart',Start,'MaxMag',19,'DB',D,...
        'ImageTable','N3_visit_images','SourceTable','N3_visit_src');
    Q2 = db.search.querySourcesByField(["1678"],'Mount',3,'Camera',2,'Crop',13,'JDstart',Start,'MaxMag',19,'Method','image',...
        'ImageTable','N3_visit_images','SourceTable','N3_visit_src');
    Q2m = db.search.querySourcesByField(["1678"],'Mount',3,'Camera',2,'Crop',13,'JDstart',Start,'MaxMag',30,'Method','image',...
        'ImageTable','N3_visit_images','SourceTable','N3_visit_src');
    Q3 = db.search.querySourcesByField(["1678"],'Mount',3,'Camera',2,'Crop',13,'JDstart',Start,'MaxMag',19,'DB',D,...
        'ImageTable','N3_visit_images','SourceTable','proc_src');
    Q3a = db.search.querySourcesByField(["1678"],'Mount',3,'Camera',2,'Crop',13,'JDstart',Start,'MaxMag',19,'DB',D,...
        'ImageTable','visit_images','SourceTable','proc_src');  
    tic;R1 = D.query(Q1); toc; size(R1)
    tic;R2 = D.query(Q2); toc; size(R2)
    tic;R2m = D.query(Q2m); toc; size(R2m)     
    tic;R3 = D.query(Q3); toc; size(R3)     
    tic;R3a = D.query(Q3a); toc; size(R3a)         
end
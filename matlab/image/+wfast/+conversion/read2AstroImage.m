function read2AstroImage(FileName,Args)
    %
   
    arguments
        FileName
        
        Args.ReadType = 'image';
    end

    
    Info = h5info(FileName);
    
    HeaderCell = [{Info.Groups.Attributes.Name}.', {Info.Groups.Attributes.Value}.'];
    WCSCell    = [{Info.Groups.Groups(1).Attributes.Name}', {Info.Groups.Groups(1).Attributes.Value}'];
    HeaderCell = [HeaderCell; WCSCell];
    % go over header and reformat it
    Nkey = size(HeaderCell,1);
    for Ikey=1:1:Nkey
        if iscell(HeaderCell{Ikey,2})
            HeaderCell{Ikey,2} = HeaderCell{Ikey,2}{1};
        end
        
        if numel(HeaderCell{Ikey,1})>8
            HeaderCell{Ikey,1} = HeaderCell{Ikey,1}(1:8);
        end
    end
    
    % fix the CRVAL, CRPIX, CDELT keywords
    
    

    
    
    switch lower(Args.ReadType)
        case 'image'
            Image = h5read(FileName, '/images');
            
        otherwise
            error('Not supported ReadType option');
    end

    
    
    
end
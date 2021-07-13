function Result = read2AstroImage(FileName, Args)
    %
    % Example: Result = wfast.conversion.read2AstroImage('WFAST_Balor_20210529-011410-415_F505W_0_Image.h5z');
   
    arguments
        FileName
        
        Args.ReadType                  = 'image';
        Args.UseRegExp(1,1) logical    = true;
    end

    
    List  = io.files.filelist(FileName,Args.UseRegExp);
    Nlist = numel(List);
    Result = AstroImage([Nlist,1]);
    for Ilist=1:1:Nlist
        FileName = List{Ilist};
    
    
        % build the header

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
        KeyName = 'CRPIX';
        Flag = strcmp(HeaderCell(:,1),KeyName);
        I = find(Flag);
        Val1 = HeaderCell{I,2}(1);
        Val2 = HeaderCell{I,2}(2);
        HeaderCell{I,1} = sprintf('%s%d',KeyName,1);
        HeaderCell{I,2} = Val1;
        HeaderCell = imUtil.headerCell.insertKey(HeaderCell,{sprintf('%s%d',KeyName,2), Val2},I+1);

        KeyName = 'CRVAL';
        Flag = strcmp(HeaderCell(:,1),KeyName);
        I = find(Flag);
        Val1 = HeaderCell{I,2}(1);
        Val2 = HeaderCell{I,2}(2);
        HeaderCell{I,1} = sprintf('%s%d',KeyName,1);
        HeaderCell{I,2} = Val1;
        HeaderCell = imUtil.headerCell.insertKey(HeaderCell,{sprintf('%s%d',KeyName,2), Val2},I+1);

        KeyName = 'CDELT';
        Flag = strcmp(HeaderCell(:,1),KeyName);
        I = find(Flag);
        Val1 = HeaderCell{I,2}(1);
        Val2 = HeaderCell{I,2}(2);
        HeaderCell{I,1} = sprintf('%s%d',KeyName,1);
        HeaderCell{I,2} = Val1;
        HeaderCell = imUtil.headerCell.insertKey(HeaderCell,{sprintf('%s%d',KeyName,2), Val2},I+1);
        
        % add a comment column
        Nkey = size(HeaderCell,1);
        HeaderCell = [HeaderCell, cell(Nkey,1)];

        Result(Ilist).HeaderData.Data = HeaderCell;

        % Read the image

        switch lower(Args.ReadType)
            case 'image'
                Image = h5read(FileName, '/images');
                
            otherwise
                error('Not supported ReadType option');
        end

        % calibrate the image
        % mask image
        MaskFlag = isnan(Image);
        Result(Ilist).MaskData = maskSet(Result(Ilist).MaskData, MaskFlag, 'HighRN', 1);
        
        % interp over NaN
        Result(Ilist).Image = Image;
        Result(Ilist).cast('double');
        imProc.image.interpOverNan( Result(Ilist) );
        Result(Ilist).cast('single');

    end
    
end
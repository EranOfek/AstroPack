function [Result, CalibObj] = read2AstroImage(Files, Args)
    % Read W-FAST image from HDF5 file into an AstroImage and calibrate the
    %   image using the wfast calibration object (dark subtraction, flat
    %   correction). Populate the header, mask image, image, and interpolate over
    %   NaNs.
    % REQUIREMENTS: addpath('/home/eran/matlab/GUYN/util')
    %               addpath('/home/eran/matlab/GUYN/WFAST/wfast')
    % Input  : - File name, or file name with regular expression.
    %          * ...,key,val,...
    %            'ReadType' - Which dataset to read:
    %                   'image' | 'stack' | 'cutouts'
    %                   Default is 'image'.
    %            'UseRegExp' - A logical indicating if to use regular
    %                   expressions when interpreting file name.
    %                   Default is true.
    %            'Calibrate' - Apply dark subtraction and flat correction.
    %                   Default is true.
    %            'CalibDir' - W-FAST calibration directory.
    %            'CalibFile' - W-FAST calibration file name.
    %            'CalibObj' - Optional calibration object (from calibration
    %                   file). If empty, thenm will attempt to load from
    %                   calibration file. Default is [].
    %            'InterpOverNan' - Interpolate over NaNs. Default is true.
    % Output : - An AstroImage object with loaded images, headers, and
    %            masks.
    %          - The calibration object.
    % Example: cd /data/euler/archive/WFAST/2021/2021-05-28/quadrature_run1
    %          Result = wfast.read2AstroImage('WFAST_Balor_20210529-011410-415_F505W_0_Image.h5z');
    %          AI =
    %          wfast.read2AstroImage('WFAST_Balor_20200801-020630-880_F505W_0_CutoutsStack.h5z','ReadType','cutouts','calibrate',false,'InterpOverNan',false);
    
    
    arguments
        Files
        
        Args.Gain                       = 0.8;
        
        Args.ReadType                   = 'image';    % 'iamge' | 'stack' | 'cutouts'
        Args.UseRegExp(1,1) logical     = true;
        Args.Calibrate(1,1) logical     = true;
        Args.CalibDir                    = '/data/euler/archive/WFAST/calibration';
        Args.CalibFile                  = 'calibration_2021-03-07_WFAST_Balor.mat';
        Args.CalibObj                   = [];
        Args.InterpOverNan(1,1) logical = true; 
    end
    
    % load Calibration object
    if isempty(Args.CalibObj) && Args.Calibrate
        PWD = pwd;
        cd(Args.CalibDir);
        cd  /data/euler/archive/WFAST/calibration
        CalibObj = io.files.load2(Args.CalibFile);
        cd(PWD);
    else
        CalibObj = Args.CalibObj;
    end
        
    
    List  = io.files.filelist(Files,Args.UseRegExp);
    Nlist = numel(List);
    Result = AstroImage([Nlist,1]);
    for Ilist=1:1:Nlist
        FileName = List{Ilist};
    
    
        % build the header

        Info = h5info(FileName);

        HeaderCell = [{Info.Groups.Attributes.Name}.', {Info.Groups.Attributes.Value}.'];
        WCSCell    = [{Info.Groups.Groups(1).Attributes.Name}', {Info.Groups.Groups(1).Attributes.Value}'];
        HeaderCell = [HeaderCell; WCSCell];
        
        ValidKeys = [fieldnames(Result(Ilist).Config.Data.WFAST.Header.KeyNames); fieldnames(Result(Ilist).Config.Data.WFAST.Header.WCS)];
        
        Flag = ismember(HeaderCell(:,1), ValidKeys);
        HeaderCell = HeaderCell(Flag,:);
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
        HeaderCell = imUtil.headerCell.deleteKey(HeaderCell,KeyName);
        
        % EQUINOX
        HeaderCell = imUtil.headerCell.replaceKey(HeaderCell, {'EQUINOX'}, {2000});
        
        % copy STARTTIM to DATEOBS
        Itime = find(strcmp(HeaderCell(:,1),'STARTTIM'));
        HeaderCell(end+1,:) = {'DATEOBS', HeaderCell{Itime,2}};
        
        
        % add a comment column
        Nkey = size(HeaderCell,1);
        HeaderCell = [HeaderCell, cell(Nkey,1)];

        Result(Ilist).HeaderData.Data = HeaderCell;
        
        % Read the image

        switch lower(Args.ReadType)
            case 'image'
                Image = h5read(FileName, '/images');
            case 'stack'
                Image = h5read(FileName, '/stack');
            case 'cutouts'
                Image = h5read(FileName, '/cutouts');
            otherwise
                error('Not supported ReadType option');
        end

        
        % calibrate the image (dark, flat, bad pixels)
        % using guy code
        if Args.Calibrate
            Image = CalibObj.input(Image);

            % mask image
            MaskFlag = isnan(Image);
            Result(Ilist).MaskData = maskSet(Result(Ilist).MaskData, MaskFlag, 'HighRN', 1);

            % gain correction
            Image = Image.*Args.Gain;
        end
        
        Result(Ilist).Image = Image;
        
        % interp over NaN
        if Args.InterpOverNan
            Result(Ilist).cast('double');
            imProc.image.interpOverNan( Result(Ilist) );
            %Result(Ilist).cast('single');
        end
        
    end
    
end
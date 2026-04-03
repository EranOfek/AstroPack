function [Result, MidJD] = coaddHeader(Obj, Args)
    % A utility function for preparing a header of a coadd image from an AstroImage array.
    %     The function update the following header keywords:
    %       - The ExpTime and JD.
    %       - 'NCOADD' - will contains the sum of NCOADD over all coadded
    %         images, or the number of images.
    %       - 'COADDOP' - Coaddition method
    %       - 'AVNCOADD' - mean of the Ncoadd argument supplied by the user.
    %                   This typically include only the latest coaddition step.
    %       - 'MINCOADD' - Like AVNCOADD, but for the min. number of images coadd 
    %                   in each pixel, in the last step of the coaddition.
    %       - 'MIDJD' - Exposure time weighted avg. of the JD.
    %       - 'MINJD' - MIDJD of first coadded observation.
    %       - 'MAXJD' - MIDJD of last coadded observation.
    %
    % Input  : - An AstroImage array
    %          * ...,key,val,... 
    %              'HeaderCopy1' - A logical indicating if to copy
    %                   the header from the 1st coadd image.
    %                   Default is true.
    %              'NewHeader' - An header to add to the coadd
    %                   image header. This can be a 3 column cell
    %                   array, an AstroHeader or AstroImage. If
    %                   empty do nothing. Default is [].
    %              'UpdateTimes' - A logical indicatin if to add
    %                   keywords regarding the number of coadded
    %                   images and update the EXPTIME and MIDJD.
    %                   Default is true.
    %              'SumExpTime' - A logical indicating if to sum
    %                   the EXPTIME in the new header, or to use
    %                   the mean (false). Default is true.
    %              'UpdateImagePathKeys' - A logical indicating if to
    %                   add the LEVEL, SUBLEVEL and CROPID keywords to
    %                   header. Default is true.
    %              'StackMethod' - Char array of stack method to write in
    %                   the header. Default is ''.
    %              'CoaddN' - An optional image indicating the number of
    %                   images used in each pixel. Default is NaN.
    %              'KeyNcoadd' - Header keyword name containing the number
    %                   of coadd images. Default is 'NCOADD'.
    %              'SumNcoadd' - If false, then NCOADD header keyword is
    %                   simply the number of images in the input object.
    %                   If true, then will read the NCOADD header keywod
    %                   from the individual images and sum them. If the sum
    %                   is NaN, then will replace this value with the
    %                   numbre of images. Default is true.
    %              'KeyExpTime' - EXPTIME header keyword name.
    %                   Default is 'EXPTIME'.
    % Output : - An AstroHeader for the coadd image.
    % Author : Eran Ofek (2024 May) 
    % Example: CoaddHeader = imProc.stack.coaddHeader(AI)

    arguments
        Obj
        Args.HeaderCopy1 logical                    = true;
        Args.NewHeader                              = [];
        Args.UpdateTimes(1,1) logical               = true;
        Args.SumExpTime(1,1) logical                = true;
        Args.UpdateImagePathKeys logical            = true;
        Args.StackMethod                            = '';
        Args.CoaddN                                 = NaN;
        Args.KeyNcoadd                              = 'NCOADD';
        Args.SumNcoadd                              = true;

        Args.KeyExpTime                             = 'EXPTIME';
    end
    
    Nim    = numel(Obj);
    Result = AstroHeader;
    
    % FFU: update header
    if Args.HeaderCopy1
        % copy image header from first image
        Result.Data = Obj(1).HeaderData.Data;  % Note this is a by value copy!
    end
    
    if ~isempty(Args.NewHeader)
        if isa(Args.NewHeader,'AstroHeader')
            Result = Args.NewHeader;
        elseif iscell(Args.NewHeader)
            Result.Data = Args.NewHeader;
        elseif isa(Args.NewHeader,'AstroImage')
            Result = Args.NewHeader.HeaderData;
        else
            error('Unknown NewHeader option');
        end
    end
    
    if Args.UpdateTimes
        if Args.SumNcoadd
            StN = Obj.getStructKey(Args.KeyNcoadd);
            Ncoadd = sum([StN.(Args.KeyNcoadd)]);
            if isnan(Ncoadd)
                Ncoadd = Nim;
            end
        else
            Ncoadd = Nim;
        end

        % update ExpTime, and MIDJD + add info re coaddition
        Filter     = getVal(Obj(1).HeaderData,'FILTER');
        Type       = getVal(Obj(1).HeaderData,'IMTYPE');
        
        StKey      = Obj.getStructKey(Args.KeyExpTime);
        VecExpTime = [StKey.(Args.KeyExpTime)].';

        VecJD      = julday(Obj);
        MidJD = sum(VecJD.*VecExpTime)/sum(VecExpTime);
        InfoCell = {'IMTYPE',Type,'';...
                    'FILTER',Filter,'';...
                    Args.KeyNcoadd, Ncoadd, 'Number of coadded images';...
                    'COADDOP',Args.StackMethod,'Coaddition method';...
                    'AVNCOADD',mean(Args.CoaddN,'all'),'Mean number of last-step coadded images per pixel';...
                    'MINCOADD',min(Args.CoaddN,[],'all'),'Minimum number of last-step coadded images per pixel';...
                    'MIDJD',MidJD,'Weighted Mean time of observations';...
                    'MINJD',min(VecJD),'MIDJD of first coadded observation';...
                    'MAXJD',max(VecJD),'MIDJD of last coadded observation'};
               
                %'MIDJD',0.5.*(max(VecJD)+min(VecJD)),'Middle time of observations';...
            
        Result = insertKey(Result, InfoCell, 'end');

        if Args.SumExpTime
            Result = replaceVal(Result, 'EXPTIME', {sum(VecExpTime)});
        else
            Result = replaceVal(Result, 'EXPTIME', {mean(VecExpTime)});
        end

    else
        MidJD = NaN;
    end
    
    % Update header ImagePath parameters
    
    if Args.UpdateImagePathKeys
        %CCDID, CROPID, FieldID,
        
        CropID   = getVal(Obj(1).HeaderData,'CROPID');
        CCDID    = getVal(Obj(1).HeaderData,'CCDID');
        FieldID  = getVal(Obj(1).HeaderData,'FieldID');
        TimeZone = getVal(Obj(1).HeaderData,'TIMEZONE');
        
        InfoCell = {'LEVEL','coadd','';...
                    'SUBLEVEL','','';...
                    'CROPID',CropID,'';...
                    'CCDID',CCDID,'';...
                    'FIELDID',FieldID,'';...
                    'TIMEZONE',TimeZone,''};
                    
        %Result.HeaderData = insertKey(Result.HeaderData, InfoCell, 'end');
        Result = replaceVal(Result, InfoCell(:,1), InfoCell(:,2), 'Comment',InfoCell(:,3), 'AddPos','end');
        
    end
    
end

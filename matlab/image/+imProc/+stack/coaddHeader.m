function [Result] = coaddHeader(Obj, Args)
    % A utility function for preparing a header of a coadd image from an AstroImage array.
    %     The function update the ExpTime and JD.
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
        
        Args.KeyExpTime                             = 'EXPTIME';
    end
    
    
    Result = AstroHeader;
    
    % FFU: update header
    if Args.HeaderCopy1
        % copy image header from first image
        Result.Data = Obj(1).HeaderData.Data;
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
        % update ExpTime, and MIDJD + add info re coaddition
        Filter     = getVal(Obj(1).HeaderData','FILTER');
        Type       = getVal(Obj(1).HeaderData','IMTYPE');
        
        StKey      = Obj.getStructKey(Args.KeyExpTime);
        VecExpTime = [StKey.(Args.KeyExpTime)].';
        
        VecJD      = julday(Obj);
                
        InfoCell = {'IMTYPE',Type,'';...
                    'FILTER',Filter,'';...
                    'NCOADD',Nim,'Number of coadded images';...
                    'COADDOP',Args.StackMethod,'Coaddition method';...
                    'AVNCOADD',mean(CoaddN,'all'),'Mean number of coadded images per pixel';...
                    'MINCOADD',min(CoaddN,[],'all'),'Minimum number of coadded images per pixel';...
                    'MIDJD',sum(MidJD.*VecExpTime)/sum(VecExpTime),'Weighted Mean time of observations';...
                    'MINJD',min(VecJD),'MIDJD of first coadded observation';...
                    'MAXJD',max(VecJD),'MIDJD of last coadded observation'};
               
                %'MIDJD',0.5.*(max(VecJD)+min(VecJD)),'Middle time of observations';...
            
        Result = insertKey(Result, InfoCell, 'end');

        if Args.SumExpTime
            Result = replaceVal(Result, 'EXPTIME', {sum(VecExpTime)});
        else
            Result = replaceVal(Result, 'EXPTIME', {mean(VecExpTime)});
        end

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

function [Result, InfoCCDSEC] = image2subimages(Obj, BlockSize, Args)
    % Partition an AstroImage image into sub images
    % Input  : - An AstroImage object with a single element.
    %          - BlockSize [X, Y] of sub images. or [X] (will be copied as [X, X]).
    %            If empty, will use imUtil.cut.subimage_grid
    %            Default is [256 256].
    %          * Arbitrary number of pairs of input arguments ...,key,val,...
    %            The following keywords are available:
    %            'Output' - Output type {['cell'] | 'struct'}
    %            'FieldName' - Field name in a struct output in which to store
    %                       the sub images. Default is 'Im'.
    %            'SubSizeXY' - Sub image size [X,Y]. Default is [128 128].
    %            'CCDSEC' - CCDSEC of image to partition. If empty,
    %                   use full image. Default is empty.
    %            'Nxy' - Number of sub images along each dimension [Nx, Ny].
    %                    If empty then use SubSizeXY. Default is [].
    %            'OverlapXY' - Overlapping extra [X, Y] to add to SubSizeXY
    %                    from each side. Default is [32 32].
    %            'UpdateMask' - A logical indicating if to update the mask
    %                   image with near edge bit and overlap bit.
    %                   Default is true.
    %            'EdgeDist' - Distance from edge, which to flag as a near
    %                   edge pixel. Default is 10 pix.
    %            'NearEdge_BitName' - NearEdge bit name.
    %                   Deafult is 'NearEdge'.
    %            'Overlap_BitName' - Overlap bit name.
    %                   Default is 'Overlap'.
    %            'BitDict' - Bit dictionary to create, if not exits yet.
    %                   Default is BitDictionary('BitMask.Image.Default').
    %            'UpdateCat' - A logical indicating if to update the
    %                   CatalogData. Including:
    %                   crop sources outside image.
    %                   Default is true.
    %            'UpdateXY' - A logical indicating if to update the X/Y coordinates 
    %                   according to the new image boundries.
    %                   Default is true.
    %            'ColX' - A cell array of X column dictionary names by
    %                   which to perform the catalog cropping, and
    %                   shifting.
    %                   Default is AstroCatalog.DefNamesX.
    %            'ColY' - Like 'ColX', but for the Y axis.
    %                   Default is AstroCatalog.DefNamesY.
    %            'AddX' - A cell array of additional X column names to
    %                   shift. Default is {}.
    %            'AddY' - Like 'AddX', but for the Y-axis. Default is {}.
    %            'UpdateWCS' - Update WCS. Default is true.
    %            'UpdatePSF' - Update PSF. Default is true.
    %            'KeyCropID' - Haeder keyword name in which to store the
    %                   running index of the sub image.
    %                   If empty, then do not add a keyword.
    %                   Default is 'CROPID'.
    % Output : - An AstroImage of sub images.
    %          - A structure with CCDSEC info, including:
    %            EdgesCCDSEC
    %            ListCenters
    %            NoOverlapCCDSEC
    %            NewNoOverlap
    % Author : Eran Ofek (May 2021)
    % Example: AI = AstroImage({rand(1024, 1024)},'Back',{rand(1024, 1024)});
    %          AI.HeaderData.insertKey({'A',1});
    %          Result = imProc.image.image2subimages(AI,[256 256])

    arguments
        Obj(1,1)
        BlockSize             = [256 256];   % If empty, will use imUtil.cut.subimage_grid
        Args.CCDSEC           = [];   % [xmin xmax ymin ymax] If given, override BlockSize
        Args.Nxy              = [];   % If empty then use SubSizeXY. Default is [].
        Args.OverlapXY        = 10;   % Optionally [overlapX overlapY]

        Args.CopyHeader(1,1) logical       = true;
        Args.KeyCropID                     = 'CROPID';
        Args.UpdateMask(1,1) logical       = true;
        Args.EdgeDist                      = 10;
        Args.NearEdge_BitName char         = 'NearEdge';
        Args.Overlap_BitName char          = 'Overlap';
        Args.BitDict(1,1) BitDictionary    = BitDictionary('BitMask.Image.Default');

        Args.UpdateCat(1,1) logical        = true;
        Args.UpdateXY(1,1) logical         = true;
        Args.ColX                          = AstroCatalog.DefNamesX;  %{'X','XWIN_IMAGE','XWIN','XPEAK','X_PEAK'};
        Args.ColY                          = AstroCatalog.DefNamesY;  %{'Y','YWIN_IMAGE','YWIN','YPEAK','Y_PEAK'};
        Args.AddX                          = {};  % additional X-coo to update
        Args.AddY                          = {};
        
        Args.UpdateWCS logical             = true;
        Args.UpdatePSF logical             = true;
    end

    
    % find the correct partition
    PropList = {'ImageData','BackData','VarData','MaskData'};
    Nprop    = numel(PropList);
    Ind      = 0;
    Nsub     = NaN;
    Result   = [];
    for Iprop=1:1:Nprop
        Prop = PropList{Iprop};
        if ~isempty(Obj.(Prop).Data)
            Ind = Ind + 1;
            if Ind==1
                [Sub,EdgesCCDSEC,ListCenters,NoOverlapCCDSEC,NewNoOverlap] = ...
                        imUtil.cut.partition_subimage(Obj.(Prop).Image, Args.CCDSEC,...
                               'Output','struct',...
                               'FieldName','Im',...
                               'SubSizeXY',BlockSize,...
                               'Nxy',Args.Nxy,...
                               'OverlapXY',Args.OverlapXY);
                Nsub   = numel(Sub);
                Result = AstroImage([1,Nsub]);
                Args.CCDSEC = EdgesCCDSEC;  
                if nargout>1
                    InfoCCDSEC.EdgesCCDSEC     = EdgesCCDSEC;
                    InfoCCDSEC.ListCenters     = ListCenters;
                    InfoCCDSEC.NoOverlapCCDSEC = NoOverlapCCDSEC;
                    InfoCCDSEC.NewNoOverlap    = NewNoOverlap;
                end
            else
                [Sub] = ...
                        imUtil.cut.partition_subimage(Obj.(Prop).Image, Args.CCDSEC,...
                               'Output','struct',...
                               'FieldName','Im',...
                               'SubSizeXY',BlockSize,...
                               'Nxy',Args.Nxy,...
                               'OverlapXY',Args.OverlapXY);
            end

            for Isub=1:1:Nsub
                Result(Isub).(Prop).Data   = Sub(Isub).Im;
                Result(Isub).(Prop).Scale  = [];
                Result(Isub).(Prop).CCDSEC = EdgesCCDSEC(Isub,:);
                % copy full header from original image
                if Iprop==1 && Args.CopyHeader 
                    % a new copy of the header
                    Result(Isub).HeaderData = Obj.HeaderData.copy;
                    
                    if ~isempty(Args.KeyCropID)
                        Result(Isub).HeaderData.replaceVal(Args.KeyCropID, Isub);
                    end
                end
                
                 % update the WCS (for all subimages but only at the first
                 %   iteration of Iprop!)
                if Iprop==1 && Args.UpdateWCS && Result(Isub).WCS.Success
                    Result(Isub).WCS.CRPIX = Result(Isub).WCS.CRPIX - EdgesCCDSEC(Isub,[1 3]) + [1 1];
                    Result(Isub).propagateWCS('UpdateCat',false);
                end
                
            end
        end
    end

    if ~isnan(Nsub)
        % set the Mask data for edge and overlapping pixels
        % update the header
        Result = imProc.transIm.updateHeaderCCDSEC(Result, 'EdgesCCDSEC',EdgesCCDSEC,...
                                                           'NoOverlapCCDSEC',NoOverlapCCDSEC,...
                                                           'NewNoOverlap',NewNoOverlap);

        % update Mask
        if Args.UpdateMask

            % add edge bit and overlap bit
            for Isub=1:1:Nsub
                % make sure that BitDictionary is populated
                if isempty(Result(Isub).MaskData.Dict)
                    % populate the BitDictionary
                    Result(Isub).MaskData.Dict = Args.BitDict;
                end
                SizeIJ = size(Result(Isub).ImageData.Image);

                % near edge
                Flag   = imUtil.ccdsec.selectNearEdges(SizeIJ, Args.EdgeDist);
                Result(Isub) = maskSet(Result(Isub), Flag, Args.NearEdge_BitName, true, 'CreateNewObj',false);

                % ovelaping
                Flag = imUtil.ccdsec.flag_ccdsec(SizeIJ, NewNoOverlap(Isub,:), false);
                Result(Isub) = maskSet(Result(Isub), Flag, Args.Overlap_BitName, true, 'CreateNewObj',false);

            end

        end

        % update the PSF
        if Args.UpdatePSF
            warning('Update PSF is not implenmented');
        end

        % update the Catalog
        if Args.UpdateCat
            for Isub=1:1:Nsub
                cropXY(Result(Isub).CatData, EdgesCCDSEC(Isub,:), 'ColX',Args.ColX,...
                                                                  'ColY',Args.ColY,...
                                                                  'AddX',Args.AddX,...
                                                                  'AddY',Args.AddY,...
                                                                  'UpdateXY',Args.UpdateXY);
            end
        end                
    end
end

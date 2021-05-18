function Result = image2subimages(Obj, BlockSize, Args)
    % Partition an AstroImage image into sub images
    % Input  : - An AstroImage object with a single element.
    %          - BlockSize [X, Y] of sub images. or [X] (will be copied as [X, X]).
    %            If empty, will use imUtil.image.subimage_grid
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
    % Example: AI = AstroImage({rand(1024, 1024)},'Back',{rand(1024, 1024)});
    %          Result = imProc.image.image2subimages(AI,[256 256])

    arguments
        Obj(1,1)
        BlockSize             = [256 256];   % If empty, will use imUtil.image.subimage_grid
        Args.CCDSEC           = [];   % [xmin xmax ymin ymax] If given, override BlockSize
        Args.Nxy              = [];   % If empty then use SubSizeXY. Default is [].
        Args.OverlapXY        = 10;   % Optionally [overlapX overlapY]

        Args.UpdateMask(1,1) logical       = true;
        Args.EdgeDist                      = 1;
        Args.NearEdge_BitName char         = 'NearEdge';
        Args.Overlap_BitName char          = 'Overlap';
        Args.BitDict(1,1) BitDictionary    = BitDictionary('BitMask.Image.Default');

        Args.UpdateCat(1,1) logical        = true;
        Args.ColX                          = {'X','XWIN_IMAGE','XWIN','XPEAK','X_PEAK'};
        Args.ColY                          = {'Y','YWIN_IMAGE','YWIN','YPEAK','Y_PEAK'};
        Args.AddX                          = {};  % additional X-coo to update
        Args.AddY                          = {};
        Args.UpdateXY(1,1) logical         = true;
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
                        imUtil.image.partition_subimage(Obj.(Prop).Image, Args.CCDSEC,...
                               'Output','struct',...
                               'FieldName','Im',...
                               'SubSizeXY',BlockSize,...
                               'Nxy',Args.Nxy,...
                               'OverlapXY',Args.OverlapXY);
                Nsub   = numel(Sub);
                Result = AstroImage([1,Nsub]);
            else
                [Sub] = ...
                        imUtil.image.partition_subimage(Obj.(Prop).Image, Args.CCDSEC,...
                               'Output','struct',...
                               'FieldName','Im',...
                               'SubSizeXY',BlockSize,...
                               'Nxy',Args.Nxy,...
                               'OverlapXY',Args.OverlapXY);
            end

            for Isub=1:1:Nsub
                Result(Isub).(Prop).Data   = Sub.Im;
                Result(Isub).(Prop).Scale  = [];
                Result(Isub).(Prop).CCDSEC = EdgesCCDSEC(Isub,:);
            end
        end
    end

    if ~isnan(Nsub)
        % set the Mask data for edge and overlapping pixels

        % update the header
        KeyNames = {'NAXIS1','NAXIS2','CCDSEC','ORIGSEC','ORIGUSEC','UNIQSEC'};
        KeyVals  = cell(size(KeyNames));
        for Isub=1:1:Nsub
            % 
            KeyVals{1} = size(Result(Isub).ImageData.Image,2);  % NAXIS1
            KeyVals{2} = size(Result(Isub).ImageData.Image,1);  % NAXI2
            KeyVals{3} = imUtil.ccdsec.ccdsec2str([1, KeyVals{1}, 1, KeyVals{2}]); % CCDSEC of current image
            KeyVals{4} = imUtil.ccdsec.ccdsec2str(EdgesCCDSEC(Isub,:));            % ORIGSEC : SEC of subimage in full image
            KeyVals{5} = imUtil.ccdsec.ccdsec2str(NoOverlapCCDSEC(Isub,:));        % ORIGUSEC : SEC of non-overlapping sub image in full image
            KeyVals{6} = imUtil.ccdsec.ccdsec2str(NewNoOverlap(Isub,:));           % UNIQSEC : SEC of non-overlapping sub image in new sub image

            Result(Isub).HeaderData.replaceVal(KeyNames, KeyVals);
        end

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
                Result(Isub) = maskSet(Result(Isub), Flag, Args.NearEdge_BitName, true, 'CreateNewObj',true);

                % ovelaping
                Flag = imUtil.ccdsec.flag_ccdsec(SizeIJ, NewNoOverlap(Isub,:), false);
                Result(Isub) = maskSet(Result(Isub), Flag, Args.Overlap_BitName, true, 'CreateNewObj',true);

            end

        end

        % update the PSF
        warning('Update PSF is not implenmented');

        % update the WCS
        warning('Update WCS is not implenmented');

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

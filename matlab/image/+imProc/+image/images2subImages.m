function [Result] = images2subImages(AI, Args)
    % Partition an AstroImage image into sub images
    %   This function replaces: imProc.image.image2subimages
    % Input  : - An AstroImage object
    %          * ...,key,val,... 
    %
    % Output : - 
    % Author : Eran Ofek (2026 Jan) 
    % Example: SI=imProc.image.images2subImages(AI);

    
    arguments
        AI
        Args.SubSizeXY        = [1728 1728];   % If empty, will use imUtil.cut.subimage_grid
        %Args.CCDSEC           = [];   % [xmin xmax ymin ymax] If given, override BlockSize
        Args.Nxy              = [];   % If empty then use SubSizeXY. Default is [].
        Args.OverlapXY        = 10;   % Optionally [overlapX overlapY]

        Args.EdgesCCDSEC      = [];
        Args.ListCenters      = [];
        Args.NoOverlapCCDSEC  = [];
        Args.NewNoOverlap     = [];
        Args.EdgeDist               = 10;

        Args.PropList               = {'ImageData','BackData','VarData','MaskData'};

        Args.CopyHeader logical     = true;
        Args.KeyCropID              = 'CROPID';
        Args.UpdateMask logical     = true;
        Args.NearEdge_BitName char  = 'NearEdge';
        Args.Overlap_BitName char   = 'Overlap';
        Args.BitDict BitDictionary  = BitDictionary('BitMask.Image.Default');

        Args.UpdateCat logical      = true;
        Args.UpdateXY logical       = true;
        Args.ColX                   = AstroCatalog.DefNamesX;  %{'X','XWIN_IMAGE','XWIN','XPEAK','X_PEAK'};
        Args.ColY                   = AstroCatalog.DefNamesY;  %{'Y','YWIN_IMAGE','YWIN','YPEAK','Y_PEAK'};
        Args.AddX                   = {};  % additional X-coo to update
        Args.AddY                   = {};
        
        Args.UpdateWCS logical      = true;
        Args.UpdatePSF logical      = true;
    end

    % construct partition:
    if isempty(Args.EdgesCCDSEC) && isempty(Args.ListCenters) && isempty(Args.NoOverlapCCDSEC) && isempty(Args.NewNoOverlap)
        SizeXY = fliplr(size(AI(1).ImageData.Image));
        [Args.EdgesCCDSEC, ~, Args.NoOverlapCCDSEC, Args.NewNoOverlap, Args.ListCenters] = imUtil.cut.gridSubImage(SizeXY, Args.SubSizeXY);

        % [Args.EdgesCCDSEC,Args.NoOverlapCCDSEC,Args.ListCenters,Args.Nxy,Args.NewNoOverlap] = imUtil.cut.subimage_grid(SizeXY,...
        %                                                     'SubSizeXY',Args.SubSizeXY,...
        %                                                     'Nxy',Args.Nxy,...
        %                                                     'OverlapXY',Args.OverlapXY);
    end
    
    
    
    Nai      = numel(AI);
    Nprop    = numel(Args.PropList);
    Ind      = 0;
    Nsub     = size(Args.EdgesCCDSEC, 1);

    Result = AstroImage([Nai, Nsub]);
    
    for Iai=1:1:Nai
        for Iprop=1:1:Nprop
            Prop = Args.PropList{Iprop};
            [Sub] = imUtil.cut.partition_subimage(AI(Iai).(Prop).Data, Args.EdgesCCDSEC,...
                                                                       'Output','struct',...
                                                                       'FieldName','Im');
            %

            for Isub=1:1:Nsub
                Result(Iai, Isub).(Prop).Data   = Sub(Isub).Im;
                Result(Iai, Isub).(Prop).Scale  = [];
                Result(Iai, Isub).(Prop).CCDSEC = Args.EdgesCCDSEC(Isub,:);
                % copy full header from original image
                if Iprop==1
                    if Args.CopyHeader 
                        % a new copy of the header
                        Result(Iai, Isub).HeaderData = AI(Iai).HeaderData.copy;
                        
                        if ~isempty(Args.KeyCropID)
                            Result(Iai, Isub).HeaderData.replaceVal(Args.KeyCropID, Isub);
                        end
                    end
                
                    % update the WCS (for all subimages but only at the first
                    %   iteration of Iprop!)
                    if Args.UpdateWCS && Result(Iai, Isub).WCS.Success
                        Result(Iai, Isub).WCS.CRPIX = Result(Iai, Isub).WCS.CRPIX - Args.EdgesCCDSEC(Isub,[1 3]) + [1 1];
                        Result(Iai, Isub).propagateWCS('UpdateCat',false);
                    end
                end %if Iprop==1
            end %for Isub=1:1:Nsub
        end %for Iprop=1:1:Nprop

        % update Mask
        if Args.UpdateMask

            % add edge bit and overlap bit
            for Isub=1:1:Nsub
                % make sure that BitDictionary is populated
                if isempty(Result(Iai, Isub).MaskData.Dict)
                    % populate the BitDictionary
                    Result(Iai, Isub).MaskData.Dict = Args.BitDict;
                end
                SizeIJ = size(Result(Iai, Isub).ImageData.Image);

                % near edge
                Flag   = imUtil.ccdsec.selectNearEdges(SizeIJ, Args.EdgeDist);
                Result(Iai, Isub) = maskSet(Result(Iai, Isub), Flag, Args.NearEdge_BitName, true, 'CreateNewObj',false);

                % ovelaping
                Flag = imUtil.ccdsec.flag_ccdsec(SizeIJ, Args.NewNoOverlap(Isub,:), false);
                Result(Iai, Isub) = maskSet(Result(Iai, Isub), Flag, Args.Overlap_BitName, true, 'CreateNewObj',false);

            end % for Isub=1:1:Nsub

        end % if Args.UpdateMask

        % update the PSF
        if Args.UpdatePSF
            warning('Update PSF is not implenmented');
        end

        % update the Catalog
        if Args.UpdateCat
            for Isub=1:1:Nsub
                cropXY(Result(Iai, Isub).CatData, Args.EdgesCCDSEC(Isub,:), 'ColX',Args.ColX,...
                                                                  'ColY',Args.ColY,...
                                                                  'AddX',Args.AddX,...
                                                                  'AddY',Args.AddY,...
                                                                  'UpdateXY',Args.UpdateXY);
            end
        end    


        % set the Mask data for edge and overlapping pixels
        % update the header CCDSEC info for all sub images in a single AI:
        Result(Iai,:) = imProc.transIm.updateHeaderCCDSEC(Result(Iai,:), 'EdgesCCDSEC',Args.EdgesCCDSEC,...
                                                           'NoOverlapCCDSEC',Args.NoOverlapCCDSEC,...
                                                           'NewNoOverlap',Args.NewNoOverlap);
    end %for Iai=1:1:Nai


end

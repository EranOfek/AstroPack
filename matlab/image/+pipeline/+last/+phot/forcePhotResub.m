function [Result] = forcePhotResub(T, RA, Dec, Args)
    % Forced photometry on LAST subtracted images, when the subtraction image doesn't exist.
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Apr) 
    % Example: RA=40.5229121965; Dec=-16.9563601815;
    %          T=pipeline.last.queryDB.searchVisitsByCoo(RA,Dec,'QueryMethod','upix');
    %          R=pipeline.last.phot.forcePhotResub(T{1}, RA, Dec);

    arguments
        T
        RA
        Dec
        Args.CooUnits          = 'deg';
        Args.RangeJD           = [-Inf Inf];
        Args.UseExistingRef    = false;
        Args.MinNumForRef      = 5;
        Args.MaxNumForRef      = 50;
        Args.MaxIter           = 0;

        Args.ReBack              = true;
        Args.RefIsBackSub        = true;
        Args.Register            = true;
        Args.GenScorr            = true;
        Args.GenTranslient       = true;
        Args.RenormS_ExcludeBits = 'NearEdge';
        Args.RenormS_StdFun      = @tools.math.stat.rstd;

    end

    % select JD in range:
    FlagJD = T.jd_start>Args.RangeJD(1) & T.jd_start<=Args.RangeJD(2);
    T      = T(FlagJD,:);

    % split the first part of the fieldid name:
    FieldID = split(T.fieldid,'.');
    % unique fielid id:
    [UnFieldID,~,UnFInd]   = unique(FieldID, 'rows', 'stable');
    
    % uniuqe: fielid, mount, camera, cropid:
    UnID   = unique([UnFInd, T.mountnum, T.camnum, T.cropid], 'rows');
    NunID  = size(UnID,1);
    K = 0;
    for Iun=1:1:NunID
        Ind = find(UnFInd==UnID(Iun,1) & T.mountnum==UnID(Iun,2) & T.camnum==UnID(Iun,3) & T.cropid==UnID(Iun,4));
        Tun = T(Ind,:);  % Table with unique entries...
        Nim = numel(Ind);

        FieldID = UnFieldID{UnID(Iun,1)};
        Mount   = UnID(Iun,2);
        CamNum  = UnID(Iun,3);
        CropID  = UnID(Iun,4);

        % make ref
        if Args.UseExistingRef
            RefAI = [];
            error('not available yet')
        else
            % select images for reference image
            if Nim<Args.MinNumForRef
                RefAI = [];
            else
                if Nim>Args.MaxNumForRef
                    [~,SI]=sort(Tun.fwhm);

                    IndForRef = SI(1:Args.MaxNumForRef);
                else
                    IndForRef = (1:1:Nim).';
                end
                
                NimRef = numel(IndForRef);
                % make RefAI
                [RefAI] = pipeline.last.coadd.coaddVisits(Tun(IndForRef,:), 'CropID',CropID);
               
            end

        end

        if ~isempty(RefAI)

            
            for Iim=1:1:Nim
                % load image
                NewAI = pipeline.last.queryDB.loadProducts(Tun(Iim,:),'coadd','Image+');

                % check that NewAI contains data
                FlagEmpty = NewAI.isemptyImage;
                if ~FlagEmpty


                    AD = imProc.sub.properSubtraction(NewAI, RefAI, 'ReBack',Args.ReBack,...
                                                            'RefIsBackSub',Args.RefIsBackSub,...
                                                            'Register',Args.Register,...
                                                            'GenScorr',Args.GenScorr,...
                                                            'GenTranslient',Args.GenTranslient,...
                                                            'RenormS_ExcludeBits',Args.RenormS_ExcludeBits,...
                                                            'RenormS_StdFun',Args.RenormS_StdFun);





                  
                    % perform forced photometry
                    % AC=imProc.sub.forcedPhotSub(AD, [RA, Dec]);

                    try
                        [ResultD] = imProc.sources.forcedPhot(AD, 'Coo',[RA Dec], 'CooUnits', Args.CooUnits, 'AddRefStarsDist', 0, 'OutType','table', 'MaxIter',Args.MaxIter);
                        [ResultR] = imProc.sources.forcedPhot(RefAI, 'Coo',[RA Dec], 'CooUnits', Args.CooUnits, 'AddRefStarsDist', 0, 'OutType','table', 'MaxIter',Args.MaxIter);
                        [ResultN] = imProc.sources.forcedPhot(NewAI, 'Coo',[RA Dec], 'CooUnits', Args.CooUnits, 'AddRefStarsDist', 0, 'OutType','table', 'MaxIter',Args.MaxIter);
    
                        % add prefix to Ref and New table columns
                        ResultR.Properties.VariableNames = "Ref_" + ResultR.Properties.VariableNames;
                        ResultN.Properties.VariableNames = "New_" + ResultN.Properties.VariableNames;
    
                        % Read S at position:
                        S_val     = imUtil.image.getValPos(AD.S, ResultD.X, ResultD.Y);
                        Scorr_val = imUtil.image.getValPos(AD.Scorr, ResultD.X, ResultD.Y);
                        Z2        = imUtil.image.getValPos(AD.Z2, ResultD.X, ResultD.Y);

                        % add meta data (JD, Mount, Camera, CropID,...)
                        Tmeta = table(AD.New.julday, string(FieldID), Mount, CamNum, CropID, NimRef, AD.HeaderData.getVal('LIMMAG'), AD.Ref.HeaderData.getVal('LIMMAG'), AD.New.HeaderData.getVal('LIMMAG'), S_val, Scorr_val, Z2);
                        Tmeta.Properties.VariableNames = {'JD','FieldID', 'Mount', 'CamNum', 'CropID', 'NimRef', 'LimMag', 'Ref_LimMag', 'New_LimMag', 'S', 'Scorr', 'Z2'};
                        K = K + 1;
                        if K==1
                            Result = [ResultD, ResultR, ResultN, Tmeta];
                        else
                            Result = [Result; [ResultD, ResultR, ResultN, Tmeta]];
                        end
                        [K, Nim, Iun, NunID]
                    catch ME
                        'a'
                    end
                    % collect data
                end
        
            end
        end

    end     

end

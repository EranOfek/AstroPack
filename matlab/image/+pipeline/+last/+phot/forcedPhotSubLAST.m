function [Result,AD,ADc] = forcedPhotSubLAST(T, RA, Dec, Args)
    % Forced photometry on LAST subtracted images.
    %   Optionally treats the cases in which reference images amd/or subtraction
    %   images does not exist.
    % Input  : - A table of LAST visit information.
    %          - RA for forced photometry position.
    %          - Dec for forced photometry position.
    %          * ...,key,val,... 
    %            'CooUnits' - Coordinates units. Default is 'deg'.
    %            'RangeJD' - [Min, Max] JD range to use.
    %                   Default is [-Inf Inf].
    %            'UseExistingRef' - A logical indicating if tou use
    %                   existing reference images.
    %                   Default is true.
    %            'ReSub' - Re subtract. If false will attempt to upload existing
    %                   zogyD data products.
    %                   Default is false.
    %            'MaxIter' - Max iter for forced photometry. Use 0 if no
    %                   position adjustment. Default is 0.
    %            See code for additional arguments.
    % Output : - An AstroCatalog (or table) output with entry per image.
    %          - The last loaded AstroZOGY object.
    %          - AstroZOGY object of forced phot cutouts. Object per line.
    %            
    % Author : Eran Ofek (2025 Apr) 
    % Example: RA=40.5229121965; Dec=-16.9563601815;
    %          T=pipeline.last.queryDB.searchVisitsByCoo(RA,Dec,'QueryMethod','upix');
    %          R=pipeline.last.phot.forcePhotSubLAST(T{1}, RA, Dec);
    %
    %          RA = 222.71; Dec=51.0
    %          T=DB.query("SELECT * FROM visit_images WHERE jd_start>2460770 AND jd_start<2460780 AND fieldid LIKE '1572'");
    %          R=pipeline.last.phot.forcedPhotSubLAST(T(1,:), RA, Dec, 'UseExistingRef',false);

    arguments
        T
        RA
        Dec
        Args.CooUnits          = 'deg';
        Args.RangeJD           = [-Inf Inf];
        Args.UseExistingRef    = false;
        Args.ReSub             = false;
        Args.MaxIter           = 0;

        Args.MinNumForRef        = 5;
        Args.MaxNumForRef        = 50;
        Args.ReBack              = true;
        Args.RefIsBackSub        = true;
        Args.Register            = true;
        Args.GenScorr            = true;
        Args.GenTranslient       = true;
        Args.RenormS_ExcludeBits = 'NearEdge';
        Args.RenormS_StdFun      = @tools.math.stat.rstd;
        Args.forcedPhotSubArgs   = {};

        Args.OutType             = 'AstroCatalog';  % | 'tableold'
        Args.BasePathRef         = '/marvin/references/v3';
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
    Result = [];

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
            AFN = pipeline.last.queryDB.table2path(Tun(1,:));
            AFN.BasePathRef = Args.BasePathRef;
            % template of ref image file name
            TempRefFileName = AFN.insertWildCards(1,'List',["Time", "Counter", "Level"]);

            PWD = pwd;
            cd(AFN.genRefPath)
            Files = dir(TempRefFileName);
            if numel(Files)==0
                warning('Reference image not found');
                RefAI = [];
            else
                RefAI = AstroImage.readFileNamesObj(Files(1).name);
            end

            cd(PWD);

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

                if Args.ReSub
                    % re-subtract image
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

                    else
                        AD = [];
                    end
                else
                    % use existing subtraction
                    
                    AD = pipeline.last.queryDB.loadProducts(Tun(Iim,:),'coadd.zogyD','Image++');
                end

                FlagEmpty = AD.isemptyImage;
                if ~FlagEmpty

                    if nargout>2
                        if Nim==1
                            ADc = AD.cutoutTransients('XY',[RA, Dec], 'CooUNits',Args.CooUnits, 'CropProp',{}, 'CreateNewObj',true);
                        else
                            ADc(Iim) = AD.cutoutTransients('XY',[RA, Dec], 'CooUNits',Args.CooUnits, 'CropProp',{}, 'CreateNewObj',true);
                        end
                    end


                    % perform forced photometry
                    switch Args.OutType
                        case 'AstroCatalog'

                            if isempty(Result)
                                Result = AstroCatalog;
                            end
                            Result(Iim) = imProc.sub.forcedPhotSub(AD, [RA, Dec], Args.forcedPhotSubArgs{:});

                        case 'tableold'
                            % old table format (used for 2024wpp paper)
                    
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
                        otherwise
                            error('Unknown OutType option');
                    end
                    
                end
        
            end
        end

    end     

end

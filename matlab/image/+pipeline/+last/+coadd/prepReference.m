function [Nvisit] = prepReference(Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Dec) 
    % Example: Nv=pipeline.last.coadd.prepReference

    arguments
        
        Args.MinJD   = celestial.time.julday([1 3 2024]);
        Args.LimMag  = 20;
        Args.MaxFWHM = 5;
        Args.StartPath = '/marvin'
        Args.RefDir  = '/raid/eran/references1';
        Args.Ncam    = 4;
        Args.Nsub    = 24;

        Args.Mode    = 'RegenCoadd';  %'RegenCoadd'|'Missing'
    end
    RAD = 180./pi;

    S = telescope.Scheduler;
    S.generateRegularGrid;
    FieldsList = [str2double(S.List.Table.FieldName), S.List.Table.RA, S.List.Table.Dec];

    D = db.Db;
    D.connect;
    D.useDB('last');
    
    

    cd (Args.StartPath);
    %load LAST_Visits.mat
    %F0 = contains(OT.Visit,'v0');
    %OT = OT(F0,:);
    % data is in OT

    load TargetList.mat
    % data in Tbl
    Ntarget = size(Tbl,1);
    
    PWD = pwd;

    Nvisit = zeros(Ntarget, Args.Ncam, Args.Nsub);
    for Itarget=1:1:Ntarget
        FieldID = Tbl.FieldName(Itarget);
        
        Tmp = split(FieldID,'.');
        FieldID = Tmp{1};
        Mnt     = Tbl.MountNum(Itarget);
        
        
        Itf = find(strcmp(Tbl.FieldName,FieldID));
        CenterRA  = Tbl.RA(Itf);
        CenterDec = Tbl.Dec(Itf);

        [Itarget, CenterRA, CenterDec]
        FieldID

        
        for Icam=1:1:Args.Ncam
            for Isub=1:1:Args.Nsub

                % check if Ref exist
                [Status,StatusPath] = pipeline.last.coadd.checkRefStatus(FieldID, Icam, Isub, 'BaseDir',Args.RefDir);
                if Status.Nref>0 && Status.Ncoadd>0
                    error('Nref>0 & Ncoadd>0 for Field=%s, Icam=%d, Isub=%d',FieldID, Icam, Isub);
                end

                if Status.Nref>0 && Status.Nref<4
                    error('Nref>0 & Nref<4 for Field=%s, Icam=%d, Isub=%d',FieldID, Icam, Isub);
                end

                switch Args.Mode
                    case 'RegenCoadd'
                        % delete existing coadd (but not ref)
                        if Status.Ncoadd>0
                            PWD1=pwd;
                            cd(StatusPath);
                            io.files.delete_cell(Status.FilesCoadd);
                            cd(PWD1);
                            Status.Ncoadd = 0;
                        end
                    case 'Missing'

                end

                if Status.Nref==0 && Status.Ncoadd==0
                
    
                    % look for the field ID in the vists catalog
                    QueryStr = db.Db.genQuery('visit_images','*', {'fieldid',sprintf('%s%%',FieldID); 'camnum',Icam; 'cropid',Isub; 'fwhm',[1 Args.MaxFWHM]; 'jd_start',[Args.MinJD 2500000]; 'limmag',[Args.LimMag 22.5]});
                    T = D.query(QueryStr);
    
                    if ~isempty(T)
                        Dist   = celestial.coo.sphere_dist_fast(CenterRA./RAD, CenterDec./RAD, T.ra./RAD, T.dec./RAD).*RAD;
                        FlagV0 = contains(T.subdir,'v0');
                        Flag   = Dist<3 & FlagV0;
                        T      = T(Flag,:);
                    end
    
                    Ifield = (1:1:size(T,1));
                    %Ifield = find(strcmp(OT.FieldID, FieldID) & OT.Mount==Mnt & OT.Camera==Icam & OT.CropID==Isub & OT.PH_ZP>25 & OT.FWHM<5 & OT.MIDJD>Args.MinJD);
                    % check that all the fields are near the relevant
                    % coordinates
                    %DD = celestial.coo.sphere_dist_fast(Tbl.RA(Itarget)./RAD, Tbl.Dec(Itarget)./RAD, OT.RAU1(Ifield)./RAD, OT.DECU1(Ifield)./RAD).*RAD;
                    %Ifield = Ifield(DD<3);
    
    
                    Nvisit(Itarget,Icam,Isub) = numel(Ifield);
            
                    if Nvisit(Itarget,Icam,Isub)>0
                        if Nvisit(Itarget,Icam,Isub)>=5
                            %CI = pipeline.last.coadd.coaddVisits(OT(Ifield,:),'CropID',Isub);
                            try
                                CI = pipeline.last.coadd.coadd(T, 'MinNim',3);
                                CI.HeaderData.deleteComment;
        
                                Destination = fullfile(Args.RefDir, FieldID);
                                
                                % generate Ref image name
                                [RefAFN]=pipeline.last.queryDB.table2path(T);
        
                                %RefAFN = AstroFileName(OT(Ifield(1),:)); %,'JDCol','MIDJD','JD2Time',true);
                                RefAFN.JD = CI.julday;
                                RefAFN.julday2time;
                                RefAFN.Counter = 0;
                                RefAFN.CCDID   = 1;
                                RefAFN.Level   = 'ref';
                                % ref. image/prodiucts name
                                RefImage       = RefAFN.genFile;
                                RefMask        = RefAFN.genFile('Product','Mask');
                                RefPSF         = RefAFN.genFile('Product','PSF');
                                RefCat         = RefAFN.genFile('Product','Cat');
                                
                                tools.os.cdmkdir(Destination);
    
                            
                                CI.write1(RefImage, 'Image');
                                CI.write1(RefMask, 'Mask');
                                CI.write1(RefPSF, 'PSF');
                                CI.write1(RefCat, 'Cat');
                            catch ME
                                'a'
                            end
    
                            cd(PWD);
    
                            
                        else
                            % select best image based on FWHM and copy it
    
                            %if 1==0
                            [~,Imin] = min(T.fwhm(Ifield));
                            Ifield = Ifield(Imin);
    
                            [AFN]=pipeline.last.queryDB.table2path(T(Ifield,:));
                            %AFN = AstroFileName(T(Ifield,:),'JDCol','MIDJD');
                            RefImagePath = AFN.genPath([],'AddSubDir',true);
    
                            cd(RefImagePath);
                            AFF = AstroFileName.dirLiteral('Level','coadd', 'CropID',Isub, 'Product','*');
                            RefImageName = AFF.genFile;
    
                            Destination = fullfile(Args.RefDir, FieldID);
                            
                            io.files.copyFiles(RefImageName, RefImageName, [], Destination);
    
                            cd(PWD);
                            %end
    
                        end
                    else
                        % no reference for field/cam/sub
                    end
                end
            end
        end
       
    end

end

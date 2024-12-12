function [Nvisit] = prepReference(Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Dec) 
    % Example: pipeline.last.prepReference

    arguments
        
        Args.MinJD   = celestial.time.julday([1 3 2024]);
        Args.StartPath = '/marvin'
        Args.RefDir  = '/raid/eran/references1';
        Args.Ncam    = 4;
        Args.Nsub    = 24;
    end

    RAD = 180./pi;

    cd (Args.StartPath);
    load LAST_Visits.mat
    % data is in OT

    load TargetList.mat
    % data in Tbl
    Ntarget = size(Tbl,1);
    
    PWD = pwd;

    Nvisit = zeros(Ntarget, Args.Ncam, Args.Nsub);
    for Itarget=467:1:Ntarget
        FieldID = Tbl.FieldName(Itarget);
        Tmp = split(FieldID,'.');
        FieldID = Tmp{1};
        Mnt     = Tbl.MountNum(Itarget);
        Itarget
        FieldID

        
        for Icam=1:1:Args.Ncam
            for Isub=1:1:Args.Nsub

                % look for the field ID in the vists catalog
                Ifield = find(strcmp(OT.FieldID, FieldID) & OT.Mount==Mnt & OT.Camera==Icam & OT.CropID==Isub & OT.PH_ZP>25 & OT.FWHM<5 & OT.MIDJD>Args.MinJD);
                % check that all the fields are near the relevant
                % coordinates
                DD = celestial.coo.sphere_dist_fast(Tbl.RA(Itarget)./RAD, Tbl.Dec(Itarget)./RAD, OT.RAU1(Ifield)./RAD, OT.DECU1(Ifield)./RAD).*RAD;
                Ifield = Ifield(DD<3);


                Nvisit(Itarget,Icam,Isub) = numel(Ifield);
        
                if Nvisit(Itarget,Icam,Isub)>0
                    if Nvisit(Itarget,Icam,Isub)>10
                        CI = pipeline.last.coadd.coaddVisits(OT(Ifield,:),'CropID',Isub);
                        CI.HeaderData.deleteComment;

                        Destination = fullfile(Args.RefDir, FieldID);
                        
                        % generate Ref image name
                        RefAFN = AstroFileName(OT(Ifield(1),:)); %,'JDCol','MIDJD','JD2Time',true);
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

                        cd(PWD);

                        
                    else
                        % select best image based on FWHM and copy it

                        %if 1==0
                        [~,Imin] = min(OT.FWHM(Ifield));
                        Ifield = Ifield(Imin);

                        AFN = AstroFileName(OT(Ifield,:),'JDCol','MIDJD');
                        RefImagePath = AFN.genPath([],'AddSubDir',true);

                        cd(RefImagePath);
                        AFF = AstroFileName.dirLiteral('Level','coadd', 'CropID',Isub);
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

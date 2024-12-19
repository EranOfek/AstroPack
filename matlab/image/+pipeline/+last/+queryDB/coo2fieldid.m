function [Result] = coo2fieldid(RA, Dec, Args)
    % Convert RA/Dec to LAST fieldid, CamNum, CropID that contains the coordinate.
    % Input  : - J2000 RA [deg|rad|sexagesimal] or object name.
    %          - J2000 Dec [deg|rad|sexagesimal] if RA is object name this
    %            argument should be empty. Default is [].
    %          * ...,key,val,... 
    %            'InUnits' - Input coordinates units. Default is 'deg'.
    %            'Server' - Name server function.
    %                   Default is @VO.name.server_simbad
    %            
    %            Additional hidden arguments - see code for details.
    % Output : - A structure array with entry per field/cam/crop match.
    %            The following fields are available:
    %            .fieldid
    %            .camnum
    %            .cropid
    % Author : Eran Ofek (2024 Dec) 
    % Example: pipeline.last.queryDB.coo2fieldid(157.78,-27) % field 500

    arguments
        RA
        Dec                     = [];
        Args.InUnits            = 'deg';
        Args.Server             = @VO.name.server_simbad;
        %Args.SortByDist logical = true;

        Args.PixScale          = 1.25;
        Args.SizeNS            = 9600;
        Args.SizeEW            = 6400;
        Args.Offset            = 400;  % pix
        Args.SubNS             = 6;
        Args.SubEW             = 4;

    end
    RAD = 180./pi;
    ARCSEC_DEG = 3600;

    SubSizeNS = Args.SizeNS.*Args.PixScale./(Args.SubNS.*ARCSEC_DEG);
    SubSizeEW = Args.SizeEW.*Args.PixScale./(Args.SubEW.*ARCSEC_DEG);

    % EW / NS
    CamSign   = [1 1; 1 -1; -1 -1; -1 1];
    Ncam      = 4;

    [RA, Dec, ObjectName] = celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits','deg', 'Server',Args.Server);

    S=telescope.Scheduler;
    S.generateRegularGrid;

    Flag = S.cooInField(RA, Dec);
    IndF = find(Flag)
    Nind = numel(IndF);
    K = 0;
    Result = struct('fieldid',[], 'camnum',[], 'cropid',[]);
    for I=1:1:Nind
        S.List.Table.RA(IndF(I))

        [OffsetLong,OffsetLat,Dist,PA] = celestial.coo.sphere_offset(S.List.Table.RA(IndF(I))./RAD, S.List.Table.Dec(IndF(I))./RAD, RA./RAD, Dec./RAD);

        OffsetLong = OffsetLong.*RAD;
        OffsetLat  = OffsetLat.*RAD;

        % % cam1 solutions
        % OffsetSubEW = (OffsetLong + Args.Offset.*Args.PixScale./ARCSEC_DEG )./SubSizeEW;
        % OffsetSubNS = (OffsetLat  + Args.Offset.*Args.PixScale./ARCSEC_DEG )./SubSizeNS;
        % if OffsetSubEW>0 && OffsetSubNS>0
        %     % solution exist:
        %     Iew = fix(abs(OffsetSubEW))+1;
        %     Ins = fix(abs(OffsetSubNS))+1;
        % 
        %     CropID = (Iew-1).*Args.SubNS + Ins;
        %     K = K + 1;
        %     Solution(K).fieldid = IndF(I);
        %     Solution(K).camnum  = 1;
        %     Solution(K).cropid  = CropID;
        % end
        % 
        % % cam2 solutions
        % OffsetSubEW = (OffsetLong + Args.Offset.*Args.PixScale./ARCSEC_DEG )./SubSizeEW;
        % OffsetSubNS = (OffsetLat  - Args.Offset.*Args.PixScale./ARCSEC_DEG )./SubSizeNS;
        % 
        % if OffsetSubEW>0 && OffsetSubNS<0
        %     % solution exist:
        %     Iew = fix(abs(OffsetSubEW))+1; 
        %     Ins = fix(abs(OffsetSubNS))+1; 
        % 
        %     Ins_tag = Args.SubNS + 1 - Ins;
        %     CropID = (Iew-1).*Args.SubNS + Ins_tag;
        %     K = K + 1;
        %     Solution(K).fieldid = IndF(I);
        %     Solution(K).camnum  = 1;
        %     Solution(K).cropid  = CropID;
        % end

        for Icam=1:1:Ncam
            % cam# solutions
            OffsetSubEW = (OffsetLong + CamSign(Icam,1).*Args.Offset.*Args.PixScale./ARCSEC_DEG )./SubSizeEW;
            OffsetSubNS = (OffsetLat  + CamSign(Icam,2).*Args.Offset.*Args.PixScale./ARCSEC_DEG )./SubSizeNS;
            if sign(OffsetSubEW)==CamSign(Icam,1) && sign(OffsetSubNS)==CamSign(Icam,2)
                % solution exist:
                Iew = fix(abs(OffsetSubEW))+1; 
                Ins = fix(abs(OffsetSubNS))+1; 
    
                if OffsetSubEW>0
                    Iew_tag = Iew;
                else
                    Iew_tag = Args.SubEW + 1 - Iew;
                end
                if OffsetSubNS>0
                    Ins_tag = Ins;
                else
                    Ins_tag = Args.SubNS + 1 - Ins;
                end
                
                CropID = (Iew_tag-1).*Args.SubNS + Ins_tag;
                if CropID>0 && CropID<=(Args.SubEW.*Args.SubNS)
                    K = K + 1;
                    Result(K).fieldid = IndF(I);
                    Result(K).camnum  = Icam;
                    Result(K).cropid  = CropID;
                end
            end
        end


    end


end

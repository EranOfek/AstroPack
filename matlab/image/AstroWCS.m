% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef AstroWCS < Component
    % Component should contain:
    % UserData
    % Config
    
    
    % Add comments
    properties (Access = public)
%        Exist(1,1)   logical = false;
        NAXIS(1,1)   uint8  = 2;
        WCSAXES(1,1) uint8  = 2;        
        CTYPE(1,:)   cell   = {'',''};   % e.g., 'RA---TAN', 'SIP', 'TPV', 'ZPN'
        CUNIT        cell   = {'',''};
        RADESYS      char   = 'ICRS';
        LONPOLE      double = 0; 
        LATPOLE      double = 90;
        EQUINOX             = 2000;
        CRPIX(1,:)   double = [0 0];
        CRVAL(1,:)   double = [1 1];
        CD           double = [1 0;0 1];
%        CDELT(1,:)   double = [1 1];   % removed - within AstroWCS we work only with CD. CD can be cosntructed from CDELT and PC
%        PC           double = [];      % removed - within AstroWCS we work only with CD. CD can be cosntructed from CDELT and PC
        PV           cell   = {zeros(0,2),zeros(0,2)};
        SIP          cell   = {zeros(0,2),zeros(0,2)};
    end     
    
    properties (GetAccess = public)
        ProjType     char   = '';
        ProjClass    char   = '';
        CooName(1,:) cell   = {'',''};
        
        AlphaP(1,1)  double = NaN;
        DeltaP(1,1)  double = NaN;
        Alpha0(1,1)  double = NaN;
        Delta0(1,1)  double = NaN;
        Phi0(1,1)    double = NaN;
        Theta0(1,1)  double = NaN;
        PhiP(1,1)    double = NaN;
        
        Tran2D(1,1) Tran2D              %= Tran2D;
        
    end
        
    % Future
%         WCS    % a structure with fields specified in Fields
%         gridLon     % a cube: e.g., Long = [X,Y,Color]
%         gridLat     % a cube: e.g., Lat  = [X,Y,Color]
%         gridCoo     % a cell array of {X,Y,...} - column per axes 
%         gridAxes = {'RA','Dec','Color'}
    
  
    
%======================================================================    
    
    methods
        % Constructor  
        function Obj = AstroWCS(Headers)
            % Construct AstroWCS object and populate it with headers
            % Input  : - Either a vector of the the size of the empty
            %            AstroWCS object (e.g., [2 2]).
            %            OR a cell array of AstrHeaders.
            % Output : - An AstroWCS object with populated fields.
            % Author : Yossi Shvartzvald (June 2021)
            % Example: 
            
            arguments
                Headers      = 1;   % name or array size
            end
            
            if isnumeric(Headers)
                % create an empty AstroWCS object
                List = cell(Headers);
            elseif iscell(Headers)
                List = Headers;
            end

            Nh = numel(List);
            for Ih=1:1:Nh
                Obj(Ih).ProjType = ''; % TODO change
            end            
            Obj = reshape(Obj,size(List));
            
        end

        
        function Obj = read_ctype(Obj)
            % Use Obj.CTYPE to fill the fields ProjType, ProjClass,
            % CooName, and CUNIT (if empty)
            
            ProjTypeDict = Dictionary('DictName','WCS.ProjType');
            CunitDict = Dictionary('DictName','WCS.CUNIT');            
            
      
           ctype = Obj.CTYPE;
            
            projtype = cell(size(ctype));
            cooname  = cell(size(ctype));
            coounit  = cell(size(ctype));
            dist    = cell(size(ctype)); % what is this? Eran - TODO
            
            for I = 1:1:numel(ctype)
                Split    = regexp(ctype{I},'-','split');
                Pair     = Split(~tools.cell.isempty_cell(Split));
                cooname{I}  = Pair{1};
                projtype{I} = Pair{2};
                if (numel(Pair)>2)   % what is this? Eran - TODO
                    dist{I} = Pair{3};
                end                
                coounit{I} = CunitDict.searchAlt(cooname{I});
            end
            
            if all(strcmp(projtype{1},projtype))
                Obj.ProjType = projtype{1};
                Obj.ProjClass = ProjTypeDict.searchAlt(Obj.ProjType);
            else 
                error(' Not all Axis are with the same projection');
            end
            
            Obj.CooName = cooname;
            
            

            Funit = tools.cell.isnan_cell(Obj.CUNIT);
            Obj.CUNIT(Funit) = coounit(Funit);
            
        end
        
    end
    
    
    methods (Static)  % static methods

      
        function Obj = header2wcs(Header)
            % Create and populate an AstroWCS object from an AstroHeader object

            Obj = AstroWCS(1);
            AH = Header;
            
            % Read number of axes
            % if WCSAXES is not available use NAXIS as default
            Obj.NAXIS = AH.getVal('NAXIS');
            Obj.WCSAXES = AH.getVal('WCSAXES');
            if (Obj.WCSAXES==0)
                Obj.WCSAXES = Obj.NAXIS;
            end
            
            Naxis = Obj.WCSAXES;

            % prepare mutli axis keys
            KeyCtype = cell(1,Naxis);
            KeyCunit = cell(1,Naxis);
            KeyCrpix = cell(1,Naxis);
            KeyCrval = cell(1,Naxis);
            for Iaxis1 = 1:1:Naxis
                KeyCtype{Iaxis1} = sprintf('CTYPE%d',Iaxis1);
                KeyCunit{Iaxis1} = sprintf('CUNIT%d',Iaxis1); 
                KeyCrpix{Iaxis1} = sprintf('CRPIX%d',Iaxis1);
                KeyCrval{Iaxis1} = sprintf('CRVAL%d',Iaxis1);
            end

            % Get CTYPE and transalte to projection information (ProjType,
            % ProjClass) and CooName and CUNIT
            Obj.CTYPE = AH.getCellKey(KeyCtype);
            Obj.CUNIT = AH.getCellKey(KeyCunit);                
            Obj = read_ctype(Obj);
            
            % Get base WCS info
            if AH.isKeyExist('RADECSYS')
                Obj.RADECSYS = AH.getVal('RADECSYS');
            end
            if AH.isKeyExist('LONPOLE')
                Obj.LONPOLE = AH.getVal('LONPOLE');
            end
            if AH.isKeyExist('LATPOLE')
                Obj.LATPOLE = AH.getVal('LATPOLE');
            end            
            if AH.isKeyExist('EQUINOX')
                Obj.EQUINOX = AH.getVal('EQUINOX');
            end    
            
            Obj.CRPIX = cell2mat(AH.getCellKey(KeyCrpix));
            Obj.CRVAL = cell2mat(AH.getCellKey(KeyCrval));
            
            Obj.CD = Obj.readCD(AH,Naxis);
            
            
            % Read distortions            
        end
        
        function CD = readCD(Header,Naxis)
            % Read The CD matrix, or PC+CDELT, or CDELT
            
            AH = Header;
            
            KeysCD = cell(1,Naxis.^2);
            KeyCdelt = cell(1,Naxis);
            KeysPC = cell(1,Naxis.^2);
            K = 0;
            for Iaxis1 = 1:1:Naxis
               KeyCdelt{Iaxis1} = sprintf('CDELT%d',Iaxis1);
               for Iaxis2=1:1:Naxis
                   K = K + 1;
                   KeysCD{K} = sprintf('CD%d_%d',Iaxis1,Iaxis2);
                   KeysPC{K} = sprintf('PC%d_%d',Iaxis1,Iaxis2);
               end 
            end
            
            % try to get CD matrix
            ValCD = AH.getCellKey(KeysCD);
            K = 0;
            CD = nan(Naxis,Naxis);
            for Iaxis1=1:1:Naxis
                for Iaxis2=1:1:Naxis
                    K = K + 1;
                    CD(Iaxis1,Iaxis2) = ValCD{K};
                end
            end

            % treat cases in which not all CD keywords are provided -
            % fill with zeros.
            if (any(isnan(CD(:))) && ~all(isnan(CD(:))))
                CD(isnan(CD)) = 0;
            end
            
            % treat cases in which CD matrix is not provided
            if all(isnan(CD(:)))
                ValCdelt = cell2mat(AH.getCellKey(KeyCdelt));
                ValPC = cell2mat(AH.getCellKey(KeysPC));
                
                if ~all(isnan(ValPC))  && ~any(isnan(ValCdelt))   % Both PC and CDELT are provided
                    
                    ValPC(isnan(ValPC)) = 0; % treat cases in which not all PC keywords are provided - fill with zeros  
                    K = 0;
                    for Iaxis1=1:1:Naxis
                        for Iaxis2=1:1:Naxis
                            K = K + 1;
                            CD(Iaxis1,Iaxis2) = ValPC(K) * ValCdelt(Iaxis1);
                        end
                    end           
                                   
                elseif   ~any(isnan(ValCdelt)) % only CDELT is provided 
                    CD = diag(ValCdelt);
                    
                else                      
                    error('no info for CD matrix');
                end
            end
            
        end

        
        
        
        
        
        
        
        
   
        
               function W=Wpopulate(Header) 
            Default.CUNIT = 'deg';
            Default.CTYPE1 = 'RA---TAN';
            Default.CTYPE2 = 'DEC--TAN';
            
            HeaderField = HEAD.HeaderField;
            WCSField    = 'WCS';
            
            if (iscell(Header))
                H = HEAD;
                H.(HeaderField) = Header;
            else
                H = Header;
            end
            Nh = numel(H);
            W  = ClassWCS(Nh,1);
            
            KeysSingle = {'RADECSYS','LONPOLE','LATPOLE','EQUINOX'};
            Nsin       = numel(KeysSingle);
            KeysN      = {'CTYPE','CUNIT','CRPIX','CRVAL','CDELT'};
            Nn         = numel(KeysN);
            
            
            for Ih=1:1:Nh
                % for each header
                % Read number of axes
                % if WCSAXES is not available use NAXES as default
                Naxes = getkey(H(Ih),'WCSAXES');
                if (isempty(Naxes))
                    Naxes = getkey(H(Ih),'NAXIS');
                else
                    if (isnan(Naxes{1}))
                        Naxes = getkey(H(Ih),'NAXIS');
                    end
                end
                Naxes = Naxes{1};
                
                % read keywords from the KeysSingle list
                ValSingle = mgetkey(H(Ih),KeysSingle);
              
                TmpCtype = getkey(H(Ih),'CTYPE1');
                
                % concat
                KeyNames = {'WCSAXES', KeysSingle{:}};
                KeyVal   = {Naxes, ValSingle{:}};
                
                W(Ih).(WCSField) = cell2struct(KeyVal,KeyNames,2);
                
              
%<<<<<<< HEAD
                if isnan(Naxes) || isnan(TmpCtype{1}(1))
                    % deal with missing WCS keywords
                    W(Ih).(WCSField).Status = false;
                    W(Ih).(WCSField).CD = nan(2,2);
                    W(Ih).(WCSField).CRPIX = nan(1,2);
                    W(Ih).(WCSField).CRVAL = nan(1,2);
                    W(Ih).(WCSField).CDELT = nan(1,2);
                    W(Ih).(WCSField).CTYPE = {'RA---TPV','DEC--TPV'};
                    W(Ih).(WCSField).CUNIT = {'deg','deg'};
                else
                    W(Ih).(WCSField).Status = true;
                    
                    % read Keywords from the KeysN list
                    KeyNname = cell(1,Nn.*Naxes);
                    K = 0;
                    for In=1:1:Nn
                        for Iaxis=1:1:Naxes
                            K = K + 1;
                            KeyNname{K} = sprintf('%s%d',KeysN{In},Iaxis);
                        end
%=======
                    end
                %end
                
                    % read Keywords from the KeysN list
                    KeyNname = cell(1,Nn.*Naxes);
                    K = 0;
                    for In=1:1:Nn
                        for Iaxis=1:1:Naxes
                            K = K + 1;
                            KeyNname{K} = sprintf('%s%d',KeysN{In},Iaxis);
                        end
                    end
                    ValN = mgetkey(H(Ih),KeyNname);
                    ValN = reshape(ValN,2,Nn);
                    for In=1:1:Nn
                        % fixing a bug found by Na'ama
                        switch lower(KeysN{In})
                            case 'cunit'
                                if (any(isnan(ValN{1,In})))
                                    % CUNIT is not populated in header
                                    % set to default
                                    ValN{1,In} = Default.CUNIT;
                                    ValN{2,In} = Default.CUNIT;
                                end

                            case 'ctype'
                                if (any(isnan(ValN{1,In})))
                                    % CUNIT is not populated in header
                                    % set to default
                                    ValN{1,In} = Default.CTYPE1;
                                    ValN{2,In} = Default.CTYPE2;
                                end
                        end

                        if (iscellstr(ValN(:,In)))
                            W(Ih).(WCSField).(KeysN{In}) = ValN(:,In).';
                        else
                            W(Ih).(WCSField).(KeysN{In}) = cell2mat(ValN(:,In)).';
    %>>>>>>> d3d1fd3e53a5851582211798c8cdcd679ba36ecd
                        end
                        ValN = mgetkey(H(Ih),KeyNname);
                        ValN = reshape(ValN,2,Nn);
                        for In=1:1:Nn
                            % fixing a bug found by Na'ama
                            switch lower(KeysN{In})
                                case 'cunit'
                                    if (any(isnan(ValN{1,In})))
                                        % CUNIT is not popuklated in header
                                        % set to degault
                                        ValN{1,In} = Default.CUNIT;
                                        ValN{2,In} = Default.CUNIT;
                                    end
                            end

                            if (iscellstr(ValN(:,In)))
                                W(Ih).(WCSField).(KeysN{In}) = ValN(:,In).';
                            else
                                W(Ih).(WCSField).(KeysN{In}) = cell2mat(ValN(:,In)).';
                            end

                        end


                        % Read The CD/PC matrix
                        KeysCD = cell(1,Naxes.^2);
                        KeysPC = cell(1,Naxes.^2);
                        K = 0;
                        for Iaxes1=1:1:Naxes
                            for Iaxes2=1:1:Naxes
                                K = K + 1;
                                KeysCD{K} = sprintf('CD%d_%d',Iaxes1,Iaxes2);
                                KeysPC{K} = sprintf('PC%d_%d',Iaxes1,Iaxes2);
                            end
                        end

                        ValCD = mgetkey(H(Ih),KeysCD);
                        K = 0;
                        CD = nan(Naxes,Naxes);
                        for Iaxes1=1:1:Naxes
                            for Iaxes2=1:1:Naxes
                                K = K + 1;
                                CD(Iaxes1,Iaxes2) = ValCD{K};
                            end
                        end

                        % bug fix - treat cases in whic not all CD keywords are
                        % provided - assume no rotation.
                        if (any(isnan(CD(:))) && ~all(isnan(CD(:))))
                            CD(isnan(CD)) = 0;
                        end
                        
                        % treat cases in which only CDELT is provided
                        if all(isnan(CD(:)))
                            CD = diag(W(Ih).WCS.CDELT);
                            
                        end



                        if (any(isnan(CD(:))) || isempty(CD))
                            % CD is empty try to read PC
                            ValCD = mgetkey(H(Ih),KeysPC);
                            K = 0;
                            ScaleName = sprintf('CDELT');
                            for Iaxes1=1:1:Naxes
                                %ScaleName = sprintf('CDELT%d',Iaxes1);
                                for Iaxes2=1:1:Naxes
                                    K = K + 1;
                                    CD(Iaxes1,1) = ValCD{K}.*W(Ih).(WCSField).(ScaleName)(Iaxes1);
                                end
                            end

                        end
                        W(Ih).(WCSField).CD = CD;


                        % Read distortions

                        % look for PV coeficients
                        FlagMatchPV = ~Util.cell.isempty_cell(regexp(H(Ih).(HeaderField)(:,1),'PV\d+\_\d+','match'));


                        Names  =regexp(H(Ih).(HeaderField)(FlagMatchPV,1), 'PV(?<D1>\d+)\_(?<D2>\d+)','names');
                        Nnames = numel(Names);
                        PV_Ind = zeros(Nnames,2);
                        for Inames=1:1:Nnames
                            PV_Ind(Inames,:) = [str2double(Names{Inames}.D1), str2double(Names{Inames}.D2)];
                        end

                        W(Ih).(WCSField).PV.Ind     = PV_Ind;
                        W(Ih).(WCSField).PV.KeyVal  = H(Ih).(HeaderField)(FlagMatchPV,2);
                        W(Ih).(WCSField).PV.KeyName = H(Ih).(HeaderField)(FlagMatchPV,1);

                        % look for SIP coeficients
                        % TBD
                    end
                    
                end

            end
                
    
            
            
        end
        
    end
    
    methods
        function Obj=fill(Obj,Force)
            % Fill ProjType, ProjClass, Coo, AlphaP, DeltaP in wcsCl object
            % Package: @wcsCl (basic)
            % Input  : - A wcsCl object
            %          - A logical indicating if to re-fill the values even
            %            if exist. Default is true.
            % Output : - A wcsCl object
            % Example: Obj.fill

            if nargin<2
                Force = true;
            end

            Nw = numel(Obj);
            for Iw=1:1:Nw
                if Force || isempty(Obj(Iw).ProjType) || isempty(Obj(Iw).ProjClass) || isnan(Obj(Iw).AlphaP) || isnan(Obj(Iw).DeltaP)
                    % fill missing values

                    N = numel(Obj(Iw).CTYPE);
                    Coo  = cell(1,N);
                    Proj = cell(1,N);
                    for I=1:1:N
                        if isempty(Obj(Iw).CTYPE{I})
                            Proj{I} = 'none';
                            Coo{I}  = 'unknown';
                        else

                            Coo{I} = strrep(Obj(Iw).CTYPE{I}(1:4),'-','');
                            if numel(Obj(Iw).CTYPE{I})<5
                                Proj{I} = 'none';
                            else
                                Proj{I} = strrep(Obj(Iw).CTYPE{I}(5:end),'-','');
                            end

                            if I>1
                                if ~(strcmp(Proj{I},Proj{1}) || strcmp(Proj{I},'none'))
                                    error('Projection of all axes must be the same unless none');
                                end
                            end
                        end
                    end

                    Obj(Iw).ProjType  = Proj{1};
                    Obj(Iw).CooName   = Coo;
                    Obj(Iw).ProjClass = wcsCl.classify_projection(Proj{1});


        %                     if ~(strcmp(Obj(Iw).CTYPE{1}(1:2),'RA') || strcmp(Obj(Iw).CTYPE{1}(3:4),'LON'))
        %                         error('Support only cases in which CTYPE1 contains longitudes');
        %                     end
        %                     if ~(strcmp(Obj(Iw).CTYPE{2}(1:3),'DEC') || strcmp(Obj(Iw).CTYPE{2}(3:4),'LAT'))
        %                         error('Support only cases in which CTYPE2 contains latitude');
        %                     end

                     switch lower(Obj(Iw).ProjClass)
                        case 'none'
                            Obj(Iw).Alpha0 = Obj(Iw).CRVAL(1);
                            Obj(Iw).Delta0 = Obj(Iw).CRVAL(2);
                            Obj(Iw).AlphaP = Obj(Iw).CRVAL(1);
                            Obj(Iw).DeltaP = Obj(Iw).CRVAL(2);

                            Obj(Iw).Phi0   = NaN;
                            Obj(Iw).Theta0 = NaN;
                            Obj(Iw).PhiP   = NaN;

                        case 'zenithal'

                            Obj(Iw).Alpha0 = Obj(Iw).CRVAL(1);
                            Obj(Iw).Delta0 = Obj(Iw).CRVAL(2);
                            Obj(Iw).AlphaP = Obj(Iw).CRVAL(1);
                            Obj(Iw).DeltaP = Obj(Iw).CRVAL(2);

                            Units = strtrim(Obj(Iw).CUNIT{1});
                            ConvFactor = convert.angular('deg',Units);

                            Obj(Iw).Phi0   = 0.*ConvFactor;
                            Obj(Iw).Theta0 = 90.*ConvFactor;

                            if Obj(Iw).Delta0>=Obj(Iw).Theta0
                                Obj(Iw).PhiP = 0.*ConvFactor;
                            else
                                Obj(Iw).PhiP = 180.*ConvFactor;
                            end

                        otherwise
                            error('Unsupported projection class (%s)',Obj(Iw).ProjClass);
                     end

                end

            end

        end

        function Obj=fill_PV(Obj)
            % fill missing values in the PV matrix with zeros
            % Package: @wcsCl (basic)
            % Input  : - A wcsCl object
            % Output : - A wcsCl object
            % Example: Obj=wcsCl.pop_exampl; Obj.fill Obj.fill_PV;

            Nw = numel(Obj);
            for Iw=1:1:Nw
                N  = numel(Obj(Iw).PV);
                for I=1:1:N
                    Ind  = Obj(Iw).PV{I}(:,1);
                    Coef = Obj(Iw).PV{I}(:,2);

                    FullInd = (0:1:max(Ind))';
                    IsM = ismember(FullInd,Ind);
                    FullCoef = zeros(size(FullInd));
                    FullCoef(IsM) = Coef;
                    Obj(Iw).PV{I} = [FullInd, FullCoef];
                end
            end
        end   
        
        
        function Obj=populate_projMeta(Obj)
            % populate projection and pole information in a wcsCl object
            % Package: @wcsCl
            % Description: 
            % Input  : - A wcsCl object
            % Output : - Obj wcsCl object with the projection type and pole data populated.

            Def.LONPOLE = 0;
            Def.LATPOLE = 90;

            N = numel(Obj);
            for I=1:1:N

                ProjAlgo  = Obj(I).CTYPE{1}(6:end);
                ProjClass = wcsCl.classify_projection(ProjAlgo);
                Obj(I).ProjType  = ProjAlgo;
                Obj(I).ProjClass = ProjClass;
                switch lower(ProjClass)
                    case 'zenithal'

                        Alpha0 = Obj(I).CRVAL(1);
                        Delta0 = Obj(I).CRVAL(2);
                        AlphaP = Obj(I).CRVAL(1);
                        DeltaP = Obj(I).CRVAL(2);

                        Phi0   = 0;
                        Theta0 = 90;

                        if Delta0>=Theta0
                            PhiP = 0;
                        else
                            PhiP = 180;
                        end

                        Obj(I).Alpha0 = Alpha0;
                        Obj(I).Delta0 = Delta0;
                        Obj(I).AlphaP = AlphaP;
                        Obj(I).DeltaP = DeltaP;
                        Obj(I).Phi0   = Phi0;
                        Obj(I).Theta0 = Theta0;
                        Obj(I).PhiP   = PhiP;
                    otherwise
                        error('Unsupported projection class (%s)',ProjClass);
                end



                switch lower(ProjAlgo)
                    case {'tan'}

                        % treat LONPOLE
                        if isempty(Obj(I).LONPOLE)
                            % check if LONPOLE is in PV1_3
                            if isempty(Obj(I).PV)
                                % set to default value
                                Obj(I).LONPOLE = Def.LONPOLE;
                            else
                                if numel(Obj(I).PV{1})>=3
                                    Obj(I).LONPOLE = Obj(I).PV{1}{3};
                                else
                                    % set to default value
                                    Obj(I).LONPOLE = Def.LONPOLE;
                                end
                            end
                        end

                        % treat LATPOLE
                        if isempty(Obj(I).LATPOLE)
                            % check if LATPOLE is in PV1_4
                            if isempty(Obj(I).PV)
                                % set to default value
                                Obj(I).LATPOLE = Def.LATPOLE;
                            else
                                if numel(Obj(I).PV{1})>=4
                                    Obj(I).LATPOLE = InPar.PV{1}{4};
                                else
                                    % set to default value
                                    Obj(I).LATPOLE = Def.LATPOLE;
                                end
                            end
                        end

                    case 'tpv'

                    otherwise
                end
            end
        end
        
    end

    %======================================================================
    
    methods (Static)
        
        
        function [Phi,Theta]=alphadelta2phitheta(Alpha,Delta,PhiP,AlphaP,DeltaP,Units)
            % Convert celestial coordinates to native coordinates
            % Package: @wcsCl (Static, transformations)
            % Input  : - Celestial longitude.
            %          - Celestial latitude.
            %          - Native longitude of the celestial pole.
            %          - Celestial longitude of the native pole.
            %          - Celestial latitude of the native pole.
            %          - Units of the input and output coordinates
            %            {'deg'|'rad'}. Default is 'deg'.
            % Output : - Native longitude
            %          - Native latitude
            % Example: [Phi,Theta]=wcsCl.alphadelta2phitheta(Alpha,Delta,PhiP,AlphaP,DeltaP)


            RAD = 180./pi;
            if nargin<6
                Units = 'deg';
            end

            % convert to radians
            switch lower(Units)
                case 'deg'
                    Alpha  = Alpha./RAD;
                    Delta  = Delta./RAD;
                    PhiP   = PhiP./RAD;
                    AlphaP = AlphaP./RAD;
                    DeltaP = DeltaP./RAD;
                case 'rad'
                    % do nothing
                otherwise
                    error('Unknown Units option');
            end

            % note that the arg(x,y) function in Calabretta et al. is
            % equivalent to atan2(y,x)
            Phi = PhiP + atan2(-cos(Delta).*sin(Alpha - AlphaP),...
                               sin(Delta).*cos(DeltaP) - cos(Delta).*sin(DeltaP).*cos(Alpha - AlphaP));
            % Note that this is 90-angular distance...
            Theta = asin(sin(Delta).*sin(DeltaP) + cos(Delta).*cos(DeltaP).*cos(Alpha - AlphaP));

            % convert radians to units
            switch lower(Units)
                case 'deg'
                    Phi   = Phi.*RAD;
                    Theta = Theta.*RAD;
            end

        end

        function [Phi,Theta]=celestial2native(Obj,Alpha,Delta,InUnits,OutUnits)
            % convert celestial coordinates to native coordinates
            % Package: @wcsCl (transformations)
            % Description: Convert sphericall celestial coordinates
            %              (alpha, delta) or cosine directions to native
            %              spherical coordinates.
            % Input  : - A single element wcsCl object.
            %          - A matrix of longiudes (Alpha).
            %            If the next input argument (matrix of latitude)
            %            is empty, then this can be a two column matrix of
            %            [alpha,delta] coordinates
            %            or a three column matrix of cosine directions.
            %          - A matrix of latitudes (Delta).
            %          - Input celestial coordinate units {'rad'|'deg'}
            %            Default is 'deg'.
            %            If input coordinates are in cosine direction, then
            %            this argument is ignored.
            %          - Output celestial coordinate units {'rad'|'deg'}
            %            Default is 'deg'.
            % Output : - A matrix of native Phi coordinate.
            %          - A matrix of native Theta coordinate.
            % Example: [Phi,Theta]=celestial2native(W,[1 1],[2 2])

            if nargin<5
                OutUnits = 'deg';
                if nargin<4
                    InUnits = 'deg';
                    if nargin<3
                        Delta = [];
                    end
                end
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end

            Units = Obj.CUNIT{1}; % units of properties in the wcsCl class
            if isempty(Delta)
                if size(Alpha,2)==3
                    % cosine direction
                    [Alpha, Delta] = celestial.coo.cosined2coo(Alpha(:,1),Alpha(:,2),Alpha(:,3));
                    % output is radians - override InUnits
                    InUnits = 'rad';
                elseif size(Alpha,2)==2
                    % do nothing
                    Delta = Alpha(:,2);
                    Alpha = Alpha(:,1);
                else
                    error('Celestial coordinates matrix must have 2 or 3 columns');
                end
            end

            % input/output units
            if ~strcmp(Obj.CUNIT{1},Obj.CUNIT{2})
                error('CUNIT of longitude and latitude must be the same');
            end

            % convert to radians
            ConvFactorW  = convert.angular(Units,'rad');
            ConvFactorIn = convert.angular(InUnits,'rad');

            [Phi,Theta] = wcsCl.alphadelta2phitheta(Alpha.*ConvFactorIn,Delta.*ConvFactorIn,...
                                    Obj.PhiP.*ConvFactorW, Obj.AlphaP.*ConvFactorW, Obj.DeltaP.*ConvFactorW, 'rad');

            ConvFactor = convert.angular('rad',OutUnits);
            Phi   = Phi.*ConvFactor;
            Theta = Theta.*ConvFactor;

        end



        % change name to: sky2xy
        function varargout=coo2xy(Obj,Lon,Lat,Units)
            % convert celestial coordinates to pixel coordinates
            % Package: @wcsCl (transformation)
            % Description: Convertt celestial coordinates that are in the
            %              reference frmae of CTYPE, RADESYS and EQUNOX,
            %              to pixel [X,Y] coordinates.
            % Input  : - A single element wcsCl object
            %          - Either longitude, or a two column matrix of
            %            [Lognitude, Latitude].
            %            This can also be a sexagesimal string or a cell
            %            array of longitude strings.
            %          - Either latitude, or empty. If empty, then assume
            %            that the previous argument contains [Long,Lat].
            %            This can also be a string or cell arraey of
            %            latitude sexagesimals.
            %          - Input coordinates units {'rad'|'deg'}
            %            Default is 'deg'
            % Output : * Pixel coordinates. If one input argument then this
            %            is a two column matrix of pixel coordinates [X,Y].
            %            If two arguments, then these are X and Y,
            %            respectively.
            % Example: [X,Y] = coo2xy(Obj,100,10,'deg')

            if nargin<4
                Units = 'deg';
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end

            if isempty(Lat)
                Lat = Lon(:,2);
                Lon = Lon(:,1);
            end

            if (iscell(Lon) || ischar(Lon)) && (iscell(Lat) || ischar(Lat))
                Lon = celestial.coo.convertdms(Lon,'gH','r');
                Lat = celestial.coo.convertdms(Lat,'gD','R');
                Units = 'rad';
            end


            % celestial to native
            [Phi,Theta] = celestial2native(Obj,[Lon(:),Lat(:)],[],Units,'deg');
            % native to intermediate
            [X,Y] = native2interm(Obj,[Phi, Theta],[],'deg');
            % inverse distorsion for TPV and SIP
            switch lower(Obj.ProjType)
                case 'tpv'
                    % for TPV, invert numerically, iteratively, hopefully
                    %  converging
                    niter=0;
                    err=Inf;
                    X0=X;
                    Y0=Y;
                    while niter<10 && err > 1e-6 % arbitrary iteration stop
                       [X1,Y1] = interm2TPVdistortedInterm(Obj,X0,Y0);
                       X0=X0-X1+X;
                       Y0=Y0-Y1+Y;
                       err=sqrt(sum((X1(:)-X(:)).^2 + (Y1(:)-Y(:)).^2));
                       niter=niter+1;
                    end
                    X=X0;
                    Y=Y0;
                case 'tan-sip'
                    'not implemented yet'
                otherwise
                    % do nothing
            end

            % Intermediate to pixel
            [PX,PY] = interm2pix(Obj,[X,Y],[]);

            PX = reshape(PX,size(Lon));
            PY = reshape(PY,size(Lat));

            if nargout>1
                varargout{1} = PX;
                varargout{2} = PY;
            else
                varargout{1} = [PX,PY];
            end

        end


    end
    
    methods



        % FFU
        function [X,Y]     = grid_coo2xy(gridCoo,gridLon,gridLat,varargin)

            error('bug')

            % convert XY coo to Long/Lat using a matrix grid
            % Package: @wcsCl (Static, basic mapping functions)
            % Description: given a matrix/cube of Lon/Lat grid coordinates
            %              as a function of X,Y and additional; e.g.,
            %              color) coordinaes, convert X,Y,color to Lon,Lat.
            % Input  : - (gridCoo) Either a cell array in which each
            %            element contains the X/Y/color coordinates of a
            %            grid, or a matrix in which each column represent
            %            the grid coordinates.
            %            Alternatively, this can be a row vector in which
            %            is element is the coordinate of the end of the
            %            grid array and assuming the array start at 1.
            %          - A grid of longitudes. This is a matrix, or a cube
            %            in which the number of dimensions is equal to the
            %            number of columns/elements in gridCoo.
            %            This cube contains the value of longitude at the
            %            specified grid points.
            %          - A grid of latitudes (the same as the previous
            %            argument, but for the latitude).
            %          * Argument per axes (e.g., x,y,color) for which to
            %            interpolate the lon/lat.
            %          * Additional parameters to pass to interpn.
            %            (e.g., 'makima').
            %            Default is 'linear'.
            % Output : - Interpolated longitudes.
            %            Return NaN if out of grid.
            %          - Interpolated latitudes.
            % Example: gridCoo = [1024 1024 3]; gridLon=rand(10,10,3);
            %          [X,Y]=wcsCl.grid_xy2coo(gridCoo,gridLon,gridLon,2,2,1)
            %          % in 2D
            %          [gridLon,gridLat] = meshgrid((1:1:10),(1:1:10));
            %          [X,Y]=wcsCl.grid_coo2xy([1024 1024],gridLon,gridLat,2,2)

            if isnumeric(gridCoo)
                Ncol = size(gridCoo,2);
                if size(gridCoo,1)==1
                    % a row vector
                    SizeGrid = size(gridLon);
                    Step = (gridCoo-1)./(SizeGrid-1);
                    for Icol=1:1:Ncol
                        Tmp{Icol} = (1:Step(Icol):gridCoo(Icol)).';
                    end

                else

                    % convert gridCoo to cell array

                    Tmp  = cell(1,Ncol);
                    for Icol=1:1:Ncol
                        Tmp{Icol} = gridCoo(:,Icol);
                    end
                end
                gridCoo = Tmp;
            end

            Naxes = numel(gridCoo);

            if numel(varargin)<Naxes
                error('Number of requested coordinates is smaller than the dimension of the grid');
            else
                if numel(varargin)==Naxes
                    % use default interpolation
                    varargin{end+1} = 'linear'; %'makima';
                end
            end

            [Mat{1:1:Naxes}] = ngrid(gridCoo{:});

            [X] = interpn(gridLon,gridLat,Mat{1},varargin{1:Naxes},varargin{Naxes+1:end});
            [Y] = interpn(gridLon,gridLat,Mat{2},varargin{1:Naxes},varargin{Naxes+1:end});

        end

        % FFU
        function [Lon,Lat] = grid_xy2coo(gridCoo,gridLon,gridLat,varargin)
            % convert XY coo to Long/Lat using a matrix grid
            % Package: @wcsCl (Static, basic mapping functions)
            % Description: given a matrix/cube of Lon/Lat grid coordinates
            %              as a function of X,Y and additional; e.g.,
            %              color) coordinaes, convert X,Y,color to Lon,Lat.
            % Input  : - (gridCoo) Either a cell array in which each
            %            element contains the X/Y/color coordinates of a
            %            grid, or a matrix in which each column represent
            %            the grid coordinates.
            %            Alternatively, this can be a row vector in which
            %            is element is the coordinate of the end of the
            %            grid array and assuming the array start at 1.
            %          - A grid of longitudes. This is a matrix, or a cube
            %            in which the number of dimensions is equal to the
            %            number of columns/elements in gridCoo.
            %            This cube contains the value of longitude at the
            %            specified grid points.
            %          - A grid of latitudes (the same as the previous
            %            argument, but for the latitude).
            %          * Argument per axes (e.g., x,y,color) for which to
            %            interpolate the lon/lat.
            %          * Additional parameters to pass to interpn.
            %            (e.g., 'makima').
            %            Default is 'linear'.
            % Output : - Interpolated longitudes.
            %            Return NaN if out of grid.
            %          - Interpolated latitudes.
            % Example: gridCoo = [1024 1024 3]; gridLon=rand(10,10,3);
            %          [Lon,Lat]=wcsCl.grid_xy2coo(gridCoo,gridLon,gridLon,2,2,1)
            %          % in 2D
            %          [gridLon,gridLat] = meshgrid((1:1:10),(1:1:10));
            %          [Lon,Lat]=wcsCl.grid_xy2coo([1024 1024],gridLon,gridLat,2,2)

            if isnumeric(gridCoo)
                Ncol = size(gridCoo,2);
                if size(gridCoo,1)==1
                    % a row vector
                    SizeGrid = size(gridLon);
                    Step = (gridCoo-1)./(SizeGrid-1);
                    for Icol=1:1:Ncol
                        Tmp{Icol} = (1:Step(Icol):gridCoo(Icol)).';
                    end

                else

                    % convert gridCoo to cell array

                    Tmp  = cell(1,Ncol);
                    for Icol=1:1:Ncol
                        Tmp{Icol} = gridCoo(:,Icol);
                    end
                end
                gridCoo = Tmp;
            end

            Naxes = numel(gridCoo);

            if numel(varargin)<Naxes
                error('Number of requested coordinates is smaller than the dimension of the grid');
            else
                if numel(varargin)==Naxes
                    % use default interpolation
                    varargin{end+1} = 'linear'; %'makima';
                end
            end
            Lon = interpn(gridCoo{:},gridLon,varargin{1:Naxes},varargin{Naxes+1:end});
            Lat = interpn(gridCoo{:},gridLat,varargin{1:Naxes},varargin{Naxes+1:end});

        end


        function [Phi,Theta]=interm2native(Obj,X,Y,InUnits,OutUnits)
            % project coordinates: intermediate to native
            % Package: @wcsCl (transformations)
            % Input  : - A wcsCl object
            %          - A matrix of intermediate X coordinate.
            %            If next argument (Y) is not orovided then this
            %            is a two column matrix of [X,Y].
            %          - A matrix of intermediate Y coordinate. 
            %          - Input intermediate coordinates units {'rad'|'deg'}.
            %            Default is 'deg'.
            %          - Output native coordinates units. Default is 'deg'.
            % Output : - A matrix of native Phi coordinate.
            %          - A matrix of native Theta coordinate.
            % Example: [Phi,Theta]=interm2native(Obj,100,100)

            if nargin<5
                OutUnits = 'deg';
                if nargin<4
                    InUnits = 'deg';
                    if nargin<3
                        Y = [];
                    end
                end
            end

            if numel(Obj)~=1
                error('Works on a single element wcsCl object');
            end

            if isempty(Y)
                Y = X(:,2);
                X = X(:,1);
            end

            % convert to deg
            ConvFactor = convert.angular(InUnits,'deg');
            X   = X.*ConvFactor;
            Y   = Y.*ConvFactor;


            switch lower(Obj.ProjClass)
                case 'zenithal'

                    switch lower(Obj.ProjType)
                        case 'tan'
                            Rtheta = sqrt(X.^2 + Y.^2);        % deg
                            Theta  = atan(180./(pi.*Rtheta));  % rad
                            Phi    = atan2(X,-Y);              % rad

                        case 'tpv'
                            Rtheta = sqrt(X.^2 + Y.^2);
                            Theta  = atan(180./(pi.*Rtheta));
                            Phi    = atan2(X,-Y);

                        otherwise

                            %Rtheta = 180./pi .* (Mu + 1).*cos(Theta)./(Mu + sin(Theta);

                            error('Unsupported projection option (%s)',ProjAlgo);
                    end

                otherwise
                    error('Unsupported projection class (%s)',Obj.ProjClass);
            end

            ConvFactor = convert.angular('rad',OutUnits);
            Theta = Theta.*ConvFactor;
            Phi   = Phi.*ConvFactor;


        end


        function [PX,PY]=interm2pix(Obj,X,Y)
            % Convert intermediate pixel coordinates to pixel coordinates
            % Package: @wcsCl (transformations)
            % Input  : - A single element wcsCl object.
            %          - A matrix of X intermediate pixel coordinate.
            %            If next argument (Y) is not provided then this is
            %            a two column matrix of [X,Y].
            %          - A matrix of Y intermeditae pixel coordinate.
            % Output : - A matrix of X pixel coordinate.
            %          - A matrix of Y pixel coordinate.
            % Example: [P1,P2]=interm2pix(Obj,1,1)

             if numel(Obj)~=1
                error('The wcsCl object input must contain a single element');
            end

            if Obj.NAXIS~=size(Obj.CD,1) && Obj.NAXIS~=size(Obj.CD,2)
                error('Number of coordinates must be consistent with number of axss and CD matrix');
            end

            if nargin<3
                Y = [];
            end
            if isempty(Y)
                Y = X(:,2);
                X = X(:,1);
            end

            % X = Obj.WCS.CD*[P - Obj.WCS.CRPIX(:)];
            % X = CD*P - CD*CRPIX
            % CD^-1 X = I*P - I*CRPIX
            % P = CRPIX + CD^T X

            %P = (Obj.CRPIX(:) + inv(Obj.CD) * XY.').';

            XY = [X(:), Y(:)];
            P = [inv(Obj.CD) * XY.' + Obj.CRPIX(:)].';

            %[inv(Obj(Iw).(WCSField).CD) * XY.' + Obj(Iw).(WCSField).CRPIX(:)].';
            %X = Obj.WCS.CD.'*XY.'; % - Obj.WCS.CRPIX(:)];

            PX = reshape(P(:,1),size(X));
            PY = reshape(P(:,2),size(Y));

        end



        function [Xi,Yi]=interm2TPVdistortedInterm(Obj,X,Y)
        % Apply TPV distortion to intermediate pixel position
        % Package: @wcsCl (transformations)
        % Input  : - A single element wcsCl object
        %          - A matrix of intermediate pixel position X.
        %          - A matrix of intermediate pixel position Y.
        % Output : - A matrix of the distorted intermediate pixel position X.
        %          - A matrix of the distorted intermediate pixel position Y.
        % Example: [Xi,Yi]=interm2TPVdistortedInterm(Obj,1,1)

            % polynomial mapping: term Xi  Yi r
            PolyPV = wcsCl.polyPVdef;



            if numel(Obj)~=1
                error('Input must be a single element wcsCl object');
            end

            R = sqrt(X.^2 + Y.^2);

            Np = numel(Obj.PV);
            if Np~=2
                error('A TPV distortion should contain two columns');
            end

            Nc     = size(Obj.PV{1},1);

            CoefX  = Obj.PV{1}(:,2);
            CoefY  = Obj.PV{2}(:,2);

            PolyPV = PolyPV(1:Nc,:);

            Xi = CoefX.' * ((X(:).'.^PolyPV(:,2)) .* (Y(:).'.^PolyPV(:,3)) .* (R(:).'.^PolyPV(:,4)));
            Yi = CoefY.' * ((Y(:).'.^PolyPV(:,2)) .* (X(:).'.^PolyPV(:,3)) .* (R(:).'.^PolyPV(:,4)));

            Xi=reshape(Xi,size(X));
            Yi=reshape(Yi,size(Y));
        end


        function Ans=isPopulated(Obj)
            % Check if wcsCl object Exist field is false (i.e., no WCS in object)
            % Package: @wcsCl (basic)
            
            Ans = [Obj.Exist];
        end
 
        
        % delete
        function Ans=iswcsCl(Obj)
        % Check if object is a wcsCl object 
        % Package: @wcsCl (basic)
            Ans = isa(Obj,'wcsCl');
        end


        function [Flag,Obj]=iswcsOk(Obj)
            % check minimal content of wcsCl object and update Exist property
            % Package: @wcsCl (Static)
            % Input  : - A wcsCl object.
            % Output : - An array of logical flags for each wcsCl element indicating if
            %            the wcsCl is likely valid.
            %          - A wcsCl object with the Exist property updated.
            % Example: [Flag,Obj]=wcsCl.iswcsOk(Obj)

            N = numel(Obj);
            Flag = false(size(Obj));
            for I=1:1:N
                Flag(I) = ~isempty(Obj(I).NAXIS) && ~isnan(Obj(I).NAXIS);
                Ntype = numel(Obj(I).CTYPE);
                for Itype=1:1:Ntype
                    if numel(Obj(I).CTYPE{Itype})>=5
                        Flag(I) = Flag(I);
                    else
                        Flag(I) = false(I);
                    end
                end

                Flag(I) = Flag(I) && all(~isempty(Obj(I).CRPIX)) && all(~isnan(Obj(I).CRPIX));
                Flag(I) = Flag(I) && all(~isempty(Obj(I).CRVAL)) && all(~isnan(Obj(I).CRVAL));
                Flag(I) = Flag(I) && all(~isempty(Obj(I).CD(:))) && all(~isnan(Obj(I).CD(:)));

                Obj(I).Exist = Flag(I);
            end
        end
        

        function [Alpha,Delta]=native2celestial(Obj,Phi,Theta,InUnits,OutUnits)
            % convert native coordinates to celestial coordinates
            % Package: @wcsCl (transformations)
            % Description: Convert spherical native coordinates
            %              (phi, theta) to spherical celestial
            %              coordinates (alpha, delta).
            % Input  : - A single element wcsCl object.
            %          - A matrix of phi (native) coordinates.
            %            If the next input argument is empty, then this is
            %            a two column matrix of [phi,theta] coordinates.
            %          - A matrix of Theta (native) coordinates.
            %          - Input native coordinate units {'rad'|'deg'}
            %            Default is 'deg'.
            %          - Output celestial coordinate units {'rad'|'deg'}
            %            Default is 'deg'.
            % Output : - A matrix of celestial (Alpha) coordinates.
            %          - A matrix of celestial (Delta) coordinates.
            % Example: [Alpha,Delta]=native2celestial(Obj,[1 1],[2 2])

            if nargin<5
                OutUnits = 'deg';
                if nargin<4
                    InUnits = 'deg';
                    if nargin<3
                        Theta = [];
                    end
                end
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end

            if isempty(Theta)
                Theta = Phi(:,2);
                Phi   = Phi(:,1);
            end

            Units = Obj.CUNIT{1};

            % input/output units
            if ~strcmp(Obj.CUNIT{1},Obj.CUNIT{2})
                error('CUNIT of longitude and latitude must be the same');
            end

            ConvFactorW  = convert.angular(Units,'rad');
            ConvFactorIn = convert.angular(InUnits,'rad');

            [Alpha,Delta]=wcsCl.phitheta2alphadelta(Phi.*ConvFactorIn,Theta.*ConvFactorIn,...
                                Obj.PhiP.*ConvFactorW,Obj.AlphaP.*ConvFactorW,Obj.DeltaP.*ConvFactorW,'rad');

            ConvFactor = convert.angular('rad',OutUnits);
            Alpha = Alpha.*ConvFactor;
            Delta = Delta.*ConvFactor;


        end


        function [X,Y]=native2interm(Obj,Phi,Theta,InUnits)
            % project coordinates: native to intermediate
            % Package: @wcsCl (transformations)
            % Input  : - A wcsCl object
            %          - A matrix of native Phi coordinate.
            %            If the next input argument (Theta) is not provided
            %            then this is a two column matrix of [Phi,Theta]
            %            native coordinates.
            %          - A matrix of native Theta coordinate.
            %          - Input native coordinates units {'rad'|'deg'}.
            %            Default is 'deg'.
            % Output : - A matrix of X intermediate pixel coordinate.
            %          - A matrix of Y intermediate pixel coordinate.
            % Example: [X,Y]=native2interm(Obj,100,100)

            if nargin<4
                InUnits = 'deg';
                if nargin<3
                    Theta = [];
                end
            end

            if numel(Obj)~=1
                error('Works on a single element wcsCl object');
            end

            if isempty(Theta)
                Theta = Phi(:,2);
                Phi   = Phi(:,1);
            end

            % convert native coordinates to radians
            ConvFactor = convert.angular(InUnits,'rad');
            Phi   = Phi.*ConvFactor;
            Theta = Theta.*ConvFactor;


            % phi,theta -> X,Y

            switch lower(Obj.ProjClass)
                case 'zenithal'

                    switch lower(Obj.ProjType)
                        case 'tan'
                            Rtheta = 180./pi .* cot(Theta);    % deg
                            X      = Rtheta.*sin(Phi);
                            Y      = -Rtheta.*cos(Phi);

                        case 'tpv'
                            Rtheta = 180./pi .* cot(Theta);    % deg
                            X      = Rtheta.*sin(Phi);
                            Y      = -Rtheta.*cos(Phi);

                        otherwise

                            %Rtheta = 180./pi .* (Mu + 1).*cos(Theta)./(Mu + sin(Theta);

                            error('Unsupported projection option (%s)',ProjAlgo);
                    end

                otherwise
                    error('Unsupported projection class (%s)',Obj.ProjClass);
            end

        end


        % static
        function [Alpha,Delta]=phitheta2alphadelta(Phi,Theta,PhiP,AlphaP,DeltaP,Units)
            % convert natve coordinates (Phi,Theta) to celestila (alpha,delta)
            % Package: @wcsCl (Static, transformations)
            % Input  : - Native longitude (phi)
            %          - Native latitude (theta)
            %          - native longitude of celestial pole
            %          - Celestial longitude of native pole
            %          - Celestial latitude of native pole (DeltaP=ThetaP)
            %          - Input and output units {'deg'|'rad'}.
            %            Default is 'deg'
            % Output : - Celestial longitude
            %          - Celestial latitude
            % Example: - [Alpha,Delta]=wcsCl.phitheta2alphadelta(1.1,1.1,0,0,0)


            RAD = 180./pi;
            if nargin<6
                Units = 'deg';
            end

            % convert to radians
            switch lower(Units)
                case 'deg'
                    Phi    = Phi./RAD;
                    Theta  = Theta./RAD;
                    PhiP   = PhiP./RAD;
                    AlphaP = AlphaP./RAD;
                    DeltaP = DeltaP./RAD;
                case 'rad'
                    % do nothing
                otherwise
                    error('Unknown Units option');
            end

            % note that the arg(x,y) function in Calabretta et al. is
            % equivalent to atan2(y,x)
            Alpha = AlphaP + atan2(-cos(Theta).*sin(Phi - PhiP),...
                                   sin(Theta).*cos(DeltaP) - cos(Theta).*sin(DeltaP).*cos(Phi - PhiP));
            Delta = asin(sin(Theta).*sin(DeltaP) + cos(Theta).*cos(DeltaP).*cos(Phi - PhiP));

            % convert radians to units
            switch lower(Units)
                case 'deg'
                    Alpha = Alpha.*RAD;
                    Delta = Delta.*RAD;
            end

        end


        function [X,Y]=pix2interm(Obj,PX,PY)
            % Convert pixel coordinates (P) to intermediate coordinates (X)
            % Package: @wcsCl (transformations)
            % Input  : - A single element wcsCl object
            %          - A matrix of pixel X coordinate.
            %            If next argument is not provided then this is a
            %            two column matrix of [PX,PY].
            %          - A matrix of pixel Y coordinate.
            % Output : - A matrix of X intermediate coordinate.
            %          - A matrix of Y intermediate coordinate.
            %            The intermediate coordinates units are specified in
            %            CUNIT.
            % Example: [X,Y]=pix2interm(Obj,1,1)

            if numel(Obj)~=1
                error('The wcsCl object input must contain a single element');
            end

            if nargin<3
                PY = [];
            end

            if isempty(PY)
                PY = PX(:,2);
                PX = PX(:,1);
            end

            P = [PX(:), PY(:)].';

            if Obj.NAXIS~=size(P,2) && Obj.NAXIS~=size(Obj.CD,1) && Obj.NAXIS~=size(Obj.CD,2)
                error('Number of coordinates must be consistent with number of axes and CD matrix');
            end

            XY = (Obj.CD*(P - Obj.CRPIX(:))).';

            X = reshape(XY(:,1),size(PX));
            Y = reshape(XY(:,2),size(PY));

        end


        function [Ind,PolyPV]=poly2tpvInd(Poly_Xdeg,Poly_Ydeg,Poly_Rdeg)
            %
            % Example: [Ind,PolyPV]=wcsCl.poly2tpvInd(T.PolyRep.PolyX_Xdeg,T.PolyRep.PolyX_Ydeg)

            if nargout<3
                Poly_Rdeg = zeros(size(Poly_Xdeg));
            end


            PolyPV = wcsCl.polyPVdef;

            N = numel(Poly_Xdeg);
            Ind = nan(N,1);
            for I=1:1:N
                Flag   = PolyPV(:,2)==Poly_Xdeg(I) & PolyPV(:,3)==Poly_Ydeg(I) & PolyPV(:,4)==Poly_Rdeg(I);
                Ind(I) = PolyPV(Flag,1);
            end
        end


        function PolyPV=polyPVdef()
            % return the TPV polynomial definition
            % Output : - A matrix of [term, Xi, Yi, r]
            % Example: PolyPV=wcsCl.polyPVdef

            % polynomial mapping: term Xi  Yi r
            PolyPV =   [0     0   0  0;...
                %
                        1     1   0  0;...
                        2     0   1  0;...
                        3     0   0  1;...
                %
                        4     2   0  0;...
                        5     1   1  0;...
                        6     0   2  0;...
                %
                        7     3   0  0;...
                        8     2   1  0;...
                        9     1   2  0;...
                        10    0   3  0;...
                        11    0   0  3;...
                %
                        12    4   0  0;...
                        13    3   1  0;...
                        14    2   2  0;...
                        15    1   3  0;...
                        16    0   4  0;...
                %
                        17    5   0  0;...
                        18    4   1  0;...
                        19    3   2  0;...
                        20    2   3  0;...
                        21    1   4  0;...
                        22    0   5  0;...
                        23    0   0  5;...
                %
                        24    6   0  0;...
                        25    5   1  0;...
                        26    4   2  0;...
                        27    3   3  0;...
                        28    2   4  0;...
                        29    1   5  0;...
                        30    0   6  0;...
                %
                        31    7   0  0;...
                        32    6   1  0;...
                        33    5   2  0;...
                        34    4   3  0;...
                        35    3   4  0;...
                        36    2   5  0;...
                        37    1   6  0;...
                        38    0   7  0;...
                        39    0   0  7];

        end


        function Obj=pop_example()
            % populate an example in a wcsCl object
            % Package: @wcsCl (Static)
            % Example: Obj=wcsCl.pop_example

            % taken from PTF image:
            % PTF_201211213689_i_p_scie_t085110_u014664936_f02_p100037_c02.fits

            Obj = wcsCl;
            Obj.Exist   = true;
            Obj.NAXIS   = 2;
            Obj.WCSAXES = 2;
            Obj.CTYPE   = {'RA--TPV','DEC-TPV'};
            Obj.CUNIT   = {'deg','deg'};
            Obj.RADESYS = 'ICRS';
            Obj.EQUINOX = 2000;
            Obj.CD      = [0.000281213122191427, 6.60586568794139E-06; 6.75167063981288E-06, -0.000281077673400925];
            Obj.CRVAL   = [148.750256715202, 69.4980616019507];
            Obj.CRPIX   = [586.994, 1882.221];
            Obj.LONPOLE = 180;
            Obj.LATPOLE = 0;

            PV1 = [0             0
                   1             1
                   2             0
                   4    0.00078642
                   5   -0.00065487
                   6    0.00012021
                   7   -0.00045983
                   8    0.00017232
                   9    -0.0002152
                   10   -0.00016153
                   12    -0.0011808
                   13   -0.00021634
                   14    0.00036701
                   15    0.00028443
                   16    4.0954e-05];

            PV2 = [0             0
                   1             1
                   2             0
                   4   -0.00078711
                   5    0.00036965
                   6    0.00011267
                   7   -0.00022865
                   8    0.00011075
                   9    0.00012075
                   10    -0.0019171
                   12     0.0001629
                   13    1.7579e-06
                   14    0.00029664
                   15    0.00021697
                   16     0.0028449];

            Obj.PV = {PV1, PV2};

            Obj.fill;
            Obj.fill_PV;

        end




        function Obj=populate(varargin)
            % Package: @wcsCl (basic)
            % Input  : * An arbitrary number of key,val, arguments that will
            %            populate the wcsCl object.
            %            Possible keywords are:
            %            'CD'
            %            'PC'
            %            'PV'
            %            'CDELT'
            %            'NAXIS'
            %            'WCSAXES'
            %            'CUNIT'
            %            'CRVAL'
            %            'PRPIX'
            %            'CTYPE'
            %            'RADESYS'
            %            'EQINOX'
            %            'LONPOLE'
            %            'LATPOLE'
            %            'MJD'
            % Output : - A wcsCl object

            Def.LONPOLE = 0;
            Def.LATPOLE = 90;

            InPar = inputParser;

            addOptional(InPar,'CD',[1 0; 0 1],@(x) isnumeric(x));
            addOptional(InPar,'PC',[1 0; 0 1],@(x) isnumeric(x));
            addOptional(InPar,'PV',{zeros(0,2),zeros(0,2)},@(x) iscell(x));
            addOptional(InPar,'CDELT',[1 1].',@(x) isnumeric(x));
            addOptional(InPar,'NAXIS',2,@(x) isnumeric(x));
            addOptional(InPar,'WCSAXES',2,@(x) isnumeric(x));
            addOptional(InPar,'CUNIT',{'deg','deg'},@(x) iscell(x));
            addOptional(InPar,'CRVAL',[0 0],@(x) isnumeric(x));     % alpha0, delta0
            addOptional(InPar,'CRPIX',[1 1],@(x) isnumeric(x));
            addOptional(InPar,'CTYPE',{'RA--TAN','DEC-TAN'},@(x) iscell(x));
            addOptional(InPar,'RADESYS','ICRS',@(x) ischar(x));  % 'ICRS' | 'FK5' | 'FK4' | 'FK4-NO-E' | 'GAPPT'
            addOptional(InPar,'EQUINOX',2000,@(x) isnumeric(x));
            addOptional(InPar,'LONPOLE',0,@(x) isnumeric(x));      % phiP
            addOptional(InPar,'LATPOLE',90,@(x) isnumeric(x));     % thetaP
            addOptional(InPar,'MJD',NaN,@(x) isnumeric(x));

            parse(InPar,varargin{:});
            InPar = InPar.Results;

            if isempty(InPar.CD)
                % try to populate CD using PC and CDELT
                if isempty(InPar.PC) || isempty(InPar.CDELT)
                    error('If CD is not provided, you must provide PC and CDELT');
                end
                InPar.CD = InPar.PC.*InPar.CDELT(:);
            end

            Obj = wcsCl(1);

            if ~strcmp(InPar.CTYPE{1}(5:7),InPar.CTYPE{2}(5:7))
                error('Projection algorithm must be equal to longitude and latitude');
            end

            if ~(strcmp(InPar.CTYPE{1}(1:2),'RA') || strcmp(InPar.CTYPE{1}(3:4),'LON'))
                error('Support only cases in which CTYPE1 contains longitudes');
            end
            if ~(strcmp(InPar.CTYPE{2}(1:3),'DEC') || strcmp(InPar.CTYPE{2}(3:4),'LAT'))
                error('Support only cases in which CTYPE2 contains latitude');
            end

            ProjAlgo  = InPar.CTYPE{1}(5:7);
            ProjClass = wcsCl.classify_projection(ProjAlgo);
            switch lower(ProjClass)
                case 'zenithal'

                    Alpha0 = InPar.CRVAL(1);
                    Delta0 = InPar.CRVAL(2);
                    AlphaP = InPar.CRVAL(1);
                    DeltaP = InPar.CRVAL(2);

                    Phi0   = 0;
                    Theta0 = 90;

                    if Delta0>=Theta0
                        PhiP = 0;
                    else
                        PhiP = 180;
                    end

                otherwise
                    error('Unsupported projection class (%s)',ProjClass);
            end

            switch lower(ProjAlgo)
                case {'tan'}

                    % treat LONPOLE
                    if isempty(InPar.LONPOLE)
                        % check if LONPOLE is in PV1_3
                        if isempty(InPar.PV)
                            % set to default value
                            InPar.LONPOLE = Def.LONPOLE;
                        else
                            if numel(InPar.PV{1})>=3
                                InPar.LONPOLE = InPar.PV{1}{3};
                            else
                                % set to default value
                                InPar.LONPOLE = Def.LONPOLE;
                            end
                        end
                    end

                    % treat LATPOLE
                    if isempty(InPar.LATPOLE)
                        % check if LATPOLE is in PV1_4
                        if isempty(InPar.PV)
                            % set to default value
                            InPar.LATPOLE = Def.LATPOLE;
                        else
                            if numel(InPar.PV{1})>=4
                                InPar.LATPOLE = InPar.PV{1}{4};
                            else
                                % set to default value
                                InPar.LATPOLE = Def.LATPOLE;
                            end
                        end
                    end

                case 'tpv'

                otherwise
            end

            % check validity
            if ~iscell(InPar.CTYPE)
                error('CTYPE must be a cell array');
            end
            if ~iscell(InPar.CUNIT)
                error('CUNIT must be a cell array');
            end
            if ~iscell(InPar.PV)
                error('PV must be a cell array');
            end

            Obj.WCS       = struct('RADESYS',InPar.RADESYS,...
                                 'EQUINOX',InPar.EQUINOX,...
                                 'NAXIS',  InPar.NAXIS,...
                                 'WCSAXES',InPar.WCSAXES,...
                                 'LONPOLE',InPar.LONPOLE,...
                                 'LATPOLE',InPar.LATPOLE,...
                                 'MJD',    InPar.MJD,...
                                 'CTYPE',  {InPar.CTYPE},...
                                 'CUNIT',  {InPar.CUNIT},...
                                 'CRVAL',  InPar.CRVAL,...
                                 'CRPIX',  InPar.CRPIX,...
                                 'CD',     InPar.CD,...
                                 'PV',     {InPar.PV},...
                                 'alpha0', Alpha0,...
                                 'delta0', Delta0,...
                                 'alphaP', AlphaP,...
                                 'deltaP', DeltaP);

        end

  
        
        
        % static
        function [X,Y]=projection_TAN(Long,Lat,R,CenterVec)
            % Package: @wcsCl (Static, projection)

                Long1 = CenterVec(1);
                Lat1  = CenterVec(2);
                % R is really R.*S (R-radius, S-scale factor)
                CosC = sin(Lat1).*sin(Lat) + cos(Lat1).*cos(Lat).*cos(Long-Long1);
                X = R.*cos(Lat).*sin(Long-Long1)./CosC;
                Y = R.*(cos(Lat1).*sin(Lat) - sin(Lat1).*cos(Lat).*cos(Long-Long1))./CosC;

        end


        function [X,Y]=projectTAN_coo2xy(Lon,Lat,CRVAL,Units)
            % project sky coordinates to interm. coordinates using TAN projection
            % Package: @wcsCl (Static, transformation)
            % Input  : - Longitude (Units).
            %          - Latitude (Units).
            %          - A two element vector of reference point relative
            %            to which the X/Y coordinates are measured.
            %          - Units of both the reference point and X/Y
            %            coordinates.
            %            {'rad'|'deg'}. Default is 'deg'.
            % Output : - X intermediate coordinate in angular units as
            %            measured relative to a reference point.
            %            X is measured toward the North.
            %          - Y intermediate coordinate in angular units as
            %            measured relative to a reference point.
            %            Y is measured toward the North.
            % Example: [Lon,Lat]=wcsCl.projectTAN_xy2coo(1,1,[100 45],'deg')
            %          [X,Y]=wcsCl.projectTAN_coo2xy(Lon,Lat,[100 45],'deg')


            if nargin<4
                Units = 'deg';
            end

            % measure angular distance from CRVAL
            ConvFactor = convert.angular(Units,'rad');
            Lon   = Lon.*ConvFactor;
            Lat   = Lat.*ConvFactor;
            CRVAL = CRVAL.*ConvFactor;

            [Theta,Phi] = celestial.coo.sphere_dist(CRVAL(1),CRVAL(2),Lon,Lat);

            Rtheta = tan(Theta);
            ConvFactor = convert.angular('rad',Units);
            X = Rtheta.*sin(Phi).*ConvFactor;
            Y = Rtheta.*cos(Phi).*ConvFactor;  % no minus sign

        end



        function [Lon,Lat]=projectTAN_xy2coo(X,Y,CRVAL,Units)
            % project interm. coordinates to sky coordinates using TAN projection
            % Package: @wcsCl (Static)
            % Input  : - X intermediate coordinate in angular units as
            %            measured relative to a reference point.
            %            X is measured toward the North.
            %          - Y intermediate coordinate in angular units as
            %            measured relative to a reference point.
            %            Y is measured toward the East.
            %          - A two element vector of reference point relative
            %            to which the X/Y coordinates are measured.
            %          - Units of both the reference point and X/Y
            %            coordinates.
            %            {'rad'|'deg'}. Default is 'deg'.
            % Output : - Longitude [Units].
            %          - Latitude [Units].
            % Example: [Lon,Lat]=wcsCl.projectTAN_xy2coo(1,1,[100 45],'deg')

            if nargin<4
                Units = 'deg';
            end

            ConvFactor = convert.angular(Units,'rad');
            CRVAL  = CRVAL.*ConvFactor;
            X      = X.*ConvFactor;
            Y      = Y.*ConvFactor;
            Rtheta = sqrt(X.^2 + Y.^2);
            Phi    = atan2(X,Y);  % X,-Y
            Theta  = atan(Rtheta);

            [Lat,Lon] = reckon(CRVAL(2),CRVAL(1),Theta,Phi,'radians');

            ConvFactor = convert.angular('rad',Units);
            Lon    = Lon.*ConvFactor;
            Lat    = Lat.*ConvFactor;

        end


        function varargout=xy2coo(Obj,PX,PY,OutUnits)
            % Convert X/Y pixel coordinates to celestial coordinates
            % Package: @wcsCl (transformations)
            % Description: Convert X/Y pixel coordinates to celestial
            %              coordinates.
            % Input  : - A single element wcsCl object
            %          - A matrix of pixel X coordinates.
            %            If next argument is not provided then this is a
            %            two or more column matrix of [PX,PY,...]
            %          - A matrix of pixel Y coordinates.
            % Output : - A two column matrix of [RA,Dec] or a matrix of RA
            %            coordinates.
            %          - A matrix of Dec coordinates.
            %            If not asked for, then the first output will be a
            %            two column matrix.
            % Example: Obj=wcsCl.pop_example;
            %          [RA,Dec]=xy2coo(Obj,1,1)

            if nargin<4
                OutUnits = 'deg';
                if nargin<3
                    PY = [];
                end
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end

            if isempty(PY)
                PY = PX(:,2);
                PX = PX(:,1);
            end

            % pixel to intermediate (in units of CUNIT)
            [X,Y] = pix2interm(Obj,PX,PY);
            % interm pixel coordinates TPV distortion
            switch lower(Obj.ProjType)
                case 'tpv'
                    [X,Y] = interm2TPVdistortedInterm(Obj,X,Y);
                case 'tan-sip'
                    'not implemented yet'
                otherwise
                    % do nothing
            end

            % intermediate to native
            [Phi,Theta] = interm2native(Obj,X,Y,Obj.CUNIT{1},'rad');
            % native to celestial 
            [Alpha, Delta] = native2celestial(Obj,Phi,Theta,'rad',OutUnits);

            if nargout<2
                varargout{1} = [Alpha, Delta];
            else
                varargout{1} = Alpha;
                varargout{2} = Delta;
            end

        end
        
    end          
       
    
    %======================================================================    
    
    methods (Static) % Unit-Test
        function Result = unitTest()

            io.msgStyle(LogLevel.Test, '@start', 'AstroWCS test started')
            
            % Change to data directory
            DataSampleDir = tools.os.getTestDataDir;
            PWD = pwd;
            cd(DataSampleDir);  
            
            % construct an empty AstroWCS
            AW = AstroWCS([2 2]);
            
            % construct a AstroWCS from an AstroHeader with full TAN projection
            AH = AstroHeader('FOCx38i0101t_c0f.fits');
            AW = AstroWCS.header2wcs(AH);
            
            % Test for header without some: CD matrix missing, no CD matrix,  partial PC matrix, no PC matrix
            ValCD = cell2mat(AH.getCellKey({'CD1_1','CD1_2','CD2_1','CD2_2'}));
            
            AH.deleteKey({'CD1_2','CD2_2'});
            AW = AstroWCS.header2wcs(AH); % should fill with zeros
            
            AH.deleteKey({'CD1_1','CD2_1'});
            AH.insertKey({'CDELT1',ValCD(1);'CDELT2',ValCD(4)}); 
            AW = AstroWCS.header2wcs(AH); % should give diagonal CD with CDELT
            
            AH.insertKey({'PC1_1',1;'PC2_2',1});
            AW = AstroWCS.header2wcs(AH); % should give diagonal CD using PC and filling with zeros
            
            AH.insertKey({'PC1_2',ValCD(2)/ValCD(1);'PC2_1',ValCD(3)/ValCD(4)});
            AW = AstroWCS.header2wcs(AH); % using CDELT and PC should give identical CD to original
            cd(PWD);   
            
            % test other things
            
            io.msgStyle(LogLevel.Test, '@passed', 'AstroWCS test passed')
            Result = true;            
        end
end
    
end



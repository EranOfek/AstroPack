% Astronomical World Coordinate System (WCS) container class
%       This class provides a container for WCS data,
%       as well as basic functionality with WCS (xy2sky, sky2xy, create from header/tran2d, convert to header).
% See the follwoing papers for general definitions:
%       Greisen & Calabretta 2002, \aap, 395, 1061. doi:10.1051/0004-6361:20021326
%       Calabretta & Greisen 2002, \aap, 395, 1077. doi:10.1051/0004-6361:20021327
% Currently supporting only Proj types: TAN, TAN-SIP, TPV
% Currently not supporting WCSAXES>2. Support only the option to read first 2 axis for NAXIS>2
%
% TODO: modify tran2wcs to work with arrays. update unittest to check header2wcs with arrays
%
% 
% Note: The ds9 wcs conversions are not precise. 
% We compare the ds9 xy2coo\coo2xy with the AstroWCS xy2sky\sky2xy. 
% The comparison shows weird trails in the difference between the two methods,
% with differences in the order of a few mas.
% The reason for that is the ds9 discontinuity (probably numeric). 
% The same check for AstroWCS methods shows no discontinuity. 
% 
% #functions (autogen)
% AstroWCS - Basic constructor for AstroWCS class. User should usually use AstroWCS.header2wcs or AstroWCS.tran2wcs
% alphadelta2phitheta - Convert celestial coordinates to native coordinates
% backwardDistortion - Apply reverse (i.e. backward) distortion to X,Y coordinates using the PV sturcture
% build_CD - Construct the CD matrix from AstroHeader. Either directly CD matrix, or PC+CDELT, or CDELT
% build_PV_from_Header - Construct a PV (distoration) structure from AstroHeader.
% build_PV_from_Tran2D - Construct a PV (distoration) structure from Tran2D object.
% build_TANSIP_from_Header - Construct a PV or RevPV structure from AstroHeader with TAN-SIP projection.
% build_TPV_from_Header - Construct a PV (distoration) structure from AstroHeader with TPV projection.
% celestial2native - Convert celestial coordinates to native coordinates
% copyElement - Custom copy of object properties Called from copy() of matlab.mixin.Copyable decendents
% fill_TANSIP_KeyNames - Fill TANSIP keynames in a PV (or RevPV) structure.
% fill_TPV_KeyNames - Fill TPV keynames in a PV (distoration) structure.
% forwardDistortion - Apply distortion to X,Y coordinates using the PV sturcture
% header2wcs - Create and populate an AstroWCS object from an AstroHeader object
% interm2native - Project intermediate coordinates to native coordinates
% interm2pix - Convert intermediate pixel coordinates to pixel coordinates, if requested also include distortion
% native2celestial - Convert native coordinates to celestial coordinates
% native2interm - Project native coordinates to intermediate coordinates
% phitheta2alphadelta - Convert naitive coordinates (Phi,Theta) to celestial coordinates (alpha,delta)
% pix2interm - Convert pixel coordinates (P) to intermediate coordinates (X), if requested also include distortion
% polyTPVdef - Return a table of TPV polynomial definition Output : - A table of TPV polinomial power (See ColNames and RowNames) Author : Yossi Shvartzvald (August 2021) Example: PolyTPVtable = AstroWCS.polyTPVdef();
% populateSucess - Populate the sucess flag in the AstroWCS object The success flag indicate if the WCS solution and residuals are reasnoble.
% populate_projMeta - Populate projection metadata (Alpha0,Delta0,AlphaP,DeltaP,Phi0,Theta0,PhiP)
% read_ctype - Read Obj.CTYPE to populate the fields: ProjType, ProjClass, CooName, and CUNIT (if empty or nan)
% read_radesys_equinox - Read from AstroHeader the RADESYS and EQUINOX. If any are missing fill with deafults.
% set.ResFit - setter for ResFit - will automatically populate related properties
% sky2xy - Convert celestial coordinates to pixel coordinates
% tran2wcs - Create and populate an AstroWCS object from a Tran2D object
% wcs2header - Convert AstroWCS object to new AstroHeader object or update an existing AstroHeader object
% wcs2keyCell - Create a cell array of WCS fields from AstroWCS object
% xy2sky - Convert pixel coordinates to celestial coordinates
% #/functions (autogen)
%


classdef AstroWCS < Component
    % Component should contain:
    
    properties (Access = public)
        NAXIS(1,1)   uint8  = 2;            % Number of axes
        WCSAXES(1,1) uint8  = 2;            % WCS dimensionality
        CTYPE(1,:)   cell   = {'',''};      % WCS projection type, e.g., 'RA---TAN', 'RA-TAN-SIP', 'RA---TPV', 'RA---ZPN'
        CUNIT(1,:)   cell   = {'',''};      % Axis unit, e.g., 'deg'
        RADESYS      char   = 'ICRS';       % Astrometric system
        LONPOLE      double = 180;          % Native Longitude of the Celestial Pole. Default for Zenithal projection
        LATPOLE      double = 0;            % Native Latitude of the Celestial Pole
        EQUINOX      double = 2000.0;       % EQUINOX
        CRPIX(1,:)   double = [0 0];        % Reference pixel
        CRVAL(1,:)   double = [1 1];        % World coordinate of reference pixel
        CD           double = [1 0;0 1];    % Linear projection matrix
        
        PV                 = AstroWCS.DefPVstruct;      % Struct of projection distortion
        RevPV              = AstroWCS.DefPVstruct;      % Struct of reverse projection distortion

    end
    
    properties (GetAccess = public)
        ProjType     char   = 'none';       % Projection type, e.g., 'TAN', 'TAN-SIP', 'TPV', 'ZPN'
        ProjClass    char   = 'none';       % Projection class, e.g., 'Zenithal'
        CooName(1,:) cell   = {'',''};      % Corrdinate name, e.g., 'RA', 'Dec'
        
        % See definitions in Calabretta & Greisen 2002, \aap, 395, 1077. doi:10.1051/0004-6361:20021327
        AlphaP(1,1)  double = NaN;          % Celestial longitude of the native pole
        DeltaP(1,1)  double = NaN;          % Celestial latitude of the native pole
        PhiP(1,1)    double = NaN;          % Native Longitude of the Celestial Pole
        
        Alpha0(1,1)  double = NaN;          % Celestial longitude of the fiducial point
        Delta0(1,1)  double = NaN;          % Celestial latitude of the fiducial point
        Phi0(1,1)    double = NaN;          % Native longitude of the fiducial point
        Theta0(1,1)  double = NaN;          % Native latitude of the fiducial point
        
        Tran2D(1,1) Tran2D   = [];          % Objcet of an astrometric transformation class
        
    end
    
    
    properties  % quality of solution
        % why is this here? In principle this can be located in Tran2D.
        % However, Tran2D describes only part of the full transformaion.
        Success(1,1) logical   = false;  % is astrometry solution reasnoable
        ErrorOnMean            = NaN;    % assymptotic-rms/sqrt(Ngood)
        AssymRMS               = NaN;    % assymptotic-rms
        Ngood                  = NaN;    % number of good matches used for the solution
        Resid                  = [];
        RefMag                 = [];
        RefColor               = [];
        SrcX                   = [];
        SrcY                   = [];
        ResFit                 = [];     % structure with additional information. % setter will attempt to populate the other properties
    end
    
    properties (Hidden, Constant)
        DefPVstruct         = struct('KeyNamesX',[],'PolyCoefX',[],'PolyX_Xdeg',[],'PolyX_Ydeg',[],'PolyX_Rdeg',[],...
                                     'KeyNamesY',[],'PolyCoefY',[],'PolyY_Xdeg',[],'PolyY_Ydeg',[],'PolyY_Rdeg',[]);
                                            % Default structure of projection distortion coefficients
    end
        
    methods  % Constructor
        function Obj = AstroWCS(Nobj)
            % Basic constructor for AstroWCS class. User should usually use AstroWCS.header2wcs or AstroWCS.tran2wcs
            % Input  : - A vector of the requested size of the empty
            %            AstroWCS object (e.g., [2 2]).
            % Output : - An AstroWCS object with fields populated with the defaults.
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          AW = AstroWCS(1);
            %          AW = AstroWCS([2 2]);

            arguments
                Nobj      = 1;   % array size
            end
            
            % create an empty AstroWCS object
            List = cell(Nobj);
            Nh = numel(List);
            for Ih=1:1:Nh
                Obj(Ih).Tran2D = [];
            end
            Obj = reshape(Obj,size(List));
            
        end
        
    end

    methods  % setters/getter
        function Obj = set.ResFit(Obj, ResFit)
            % setter for ResFit - will automatically populate related properties

            Obj.ResFit  = ResFit;
            Obj         = tools.struct.copyProp(ResFit, Obj, {'ErrorOnMean','AssymRMS','Ngood','Resid','RefMag','SrcX','SrcY'});
            
            % populate the sucess flag
            
        end
    end

    methods  % General functions
        function Obj = populate_projMeta(Obj)
            % Populate projection metadata (Alpha0,Delta0,AlphaP,DeltaP,Phi0,Theta0,PhiP)
            % Input  : - AstroWCS object.
            % Output : - AstroWCS object with populated metadata fields.
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          AW = AstroWCS(1); AW.populate_projMeta;
                     
             switch lower(Obj.ProjClass)
                case 'none'
                    Obj.Alpha0 = Obj.CRVAL(1);
                    Obj.Delta0 = Obj.CRVAL(2);
                    Obj.AlphaP = Obj.CRVAL(1);
                    Obj.DeltaP = Obj.CRVAL(2);

                    Obj.Phi0   = NaN;
                    Obj.Theta0 = NaN;
                    Obj.PhiP   = NaN;

                case 'zenithal'
                    Obj.Alpha0 = Obj.CRVAL(1);
                    Obj.Delta0 = Obj.CRVAL(2);
                    Obj.AlphaP = Obj.CRVAL(1);
                    Obj.DeltaP = Obj.CRVAL(2);

                    ConvFactor = convert.angular('deg',Obj.CUNIT{1});

                    Obj.Phi0   = 0.*ConvFactor;
                    Obj.Theta0 = 90.*ConvFactor;

                    if Obj.Delta0>=Obj.Theta0
                        Obj.PhiP = 0.*ConvFactor;
                    else
                        Obj.PhiP = 180.*ConvFactor;
                    end

                otherwise
                    error('Unsupported projection class (%s)',Obj.ProjClass);
             end
        end
        
        function Obj = populateSucess(Obj, Args)
            % Populate the sucess flag in the AstroWCS object
            %   The success flag indicate if the WCS solution and residuals
            %   are reasnoble.
            % Input  : - An AstroWCS object (single element)
            %          * ...,key,val,...
            %            'TestNbin' - Number of bins in each dim of the 2D hist
            %                   by which to calculate the regional
            %                   residuals. Default is 3.
            %            'RegionalMaxMedianRMS' - Maximal regional RMS
            %                   [pix/arcsec???]. Default is 1
            %            'RegionalMaxWithNoSrc' - Max number of regions
            %                   with less than 2 matches. Default is 0.
            %            'MaxErrorOnMean' - Max error on th mean.
            %                   [pix/arcsec???]. Default is 0.05.
            % Output : - An AstroWCS object with the Success property
            %            populated.
            % Author : Eran Ofek (Sep 2021)
            
            arguments
                Obj(1,1)
                Args.TestNbin                 = 2;
                Args.RegionalMaxMedianRMS     = 1;     % arcsec OR pix?
                Args.RegionalMaxWithNoSrc     = 0;
                Args.MaxErrorOnMean           = 0.05;  % arcsec OR pix?
                Args.MinStarsForRegional      = 50;
            end
            ARCSEC_DEG = 3600;
            
            % sucess
            ImSize = min(range(Obj.ResFit.SrcX), range(Obj.ResFit.SrcY));
            Step   = ImSize./Args.TestNbin;

            if sum(Obj.ResFit.FlagSrc)<Args.MinStarsForRegional
                % treat images with small number of stars
                if isempty(Obj.ResFit.ErrorOnMean)
                    Obj.Success = false;
                else
                    Obj.Success = Obj.ResFit.ErrorOnMean < (Args.MaxErrorOnMean./ARCSEC_DEG);
                end
            else
                [BinN, ~, BinMedian] = tools.math.stat.bin2dFun(Obj.ResFit.SrcX(Obj.ResFit.FlagSrc),...
                                                                Obj.ResFit.SrcY(Obj.ResFit.FlagSrc),...
                                                                Obj.ResFit.Resid(Obj.ResFit.FlagSrc).*ARCSEC_DEG, 'Step',Step);

                Obj.Success = max(BinMedian,[],'all') < Args.RegionalMaxMedianRMS && ...
                              sum(BinN<1,'all') <= Args.RegionalMaxWithNoSrc && ...
                              Obj.ResFit.ErrorOnMean < (Args.MaxErrorOnMean./ARCSEC_DEG);
            end
        end
        
        function Obj = cropWCS(Obj,Pos,Args)
            % Update AstroWCS for a cropped region
            % Input  : - AstroWCS object or array of AstroWCS with size Nobj.
            %          - Updated position information of either:
            %                - CRPIX(1,1:2): new CRPIX for all AstroWCS array 
            %                - CRPIX(Nobj,1:2): pair of CRPIX for each element in the AstroWCS array
            %                - cropSEC(1,1:4): Same cropped section [xmin,xmax,ymin,ymax] for all AstroWCS array 
            %                - cropSEC(Nobj,1:4): Cropped section [xmin,xmax,ymin,ymax] for each element in the AstroWCS array
            %          * ...,key,val,...
            %            'centerCRPIX'   - Flag for moving CRPIX to within the cropped area using AstroWCS information (including distortions). 
            %                                  - if Pos is CRPIX then move to CRPIX(1,1:2) = [1,1] and update CRVAL,
            %                                  - if Pos is cropSEC then move to CRPIX(1,1:2) to center of cropSEc and update CRVAL,
            %                              *FFU: Note that currently CV and PV are not changed.
            %                              Default is false.
            %            'delDistortion' - Flag for deleting distortions
            %                              (PV, revPV). Default is true.
            % Output : - Updated AstroWCS object or array of AstroWCS for the cropped region
            % Author : Yossi Shvartzvald (December 2021)
            % Example:
            %                
            
            arguments
                Obj
                Pos
                Args.centerCRPIX         = false;
                Args.delDistortion       = true;
            end
            
            Nobj = numel(Obj);
            Npos = size(Pos,1);
            PosType = size(Pos,2);
            
            if (Nobj<1) || (Npos<1) || (Npos>1 && Npos~=Nob) || (PosType~=2 && PosType~=4)
               error('Wrong dimensions of either Obj or Pos');
            end
            
            for Iobj = 1:1:Nobj
                Ipos = min(Iobj,Npos);
                CurrPos = Pos(Ipos,:);
                
                switch PosType
                    case 2 % new CRPIX
                        if ~Args.centerCRPIX
                            Obj(Iobj).CRPIX(1,1:2) = CurrPos;
                        else
                            [newCRVAL(1,1),newCRVAL(1,2)] = Obj(Iobj).xy2sky(CurrPos(1),CurrPos(2));
                            Obj(Iobj).CRVAL(1,1:2) = newCRVAL;
                            Obj(Iobj).CRPIX(1,1:2) = [1,1];
                            Obj(Iobj).populate_projMeta;
                        end
                        
                    case 4 % cropSEC is given
                        if ~Args.centerCRPIX                        
                            Obj(Iobj).CRPIX(1,1:2) = Obj(Iobj).CRPIX(1,1:2) - CurrPos(1,[1,3]) +1;
                        else
                            [newCRVAL(1,1),newCRVAL(1,2)]  = Obj(Iobj).xy2sky(mean(CurrPos(1:2)),mean(CurrPos(3:4)));
                            Obj(Iobj).CRVAL(1,1:2) = newCRVAL;
                            Obj(Iobj).CRPIX(1,1) = mean(CurrPos(1:2))-CurrPos(1)+1;
                            Obj(Iobj).CRPIX(1,2) = mean(CurrPos(3:4))-CurrPos(3)+1;
                            Obj(Iobj).populate_projMeta;
                        end
                        
                end
                
                if Args.delDistortion
                     Obj(Iobj).PV = AstroWCS.DefPVstruct;
                     Obj(Iobj).RevPV = AstroWCS.DefPVstruct;
                end
            end
            
        end
        
        function Scale = getScale(Obj, OutUnits)
            % Get pixel scale from CD matrix
            % Input  : - An AstroWCS object
            %          - Output units: 'arcsec'|'deg' per pixel.
            %            Default is 'arcsec'.
            % Output : - A vector of pixel scales per AstroWCS element.
            % Author : Eran Ofek (Jul 2022)
            
            arguments
                Obj
                OutUnits    = 'arcsec';
            end
            
            Nobj  = numel(Obj);
            Scale = zeros(Nobj,1);
            for Iobj=1:1:Nobj
                Scale(Iobj) = (abs(Obj(Iobj).CD(1,1)) + abs(Obj(Iobj).CD(1,1))).*0.5;
                Scale(Iobj) = convert.angular(Obj(Iobj).CUNIT{1}, OutUnits, Scale(Iobj));
            end
        end
    end

    methods   % Functions to construct AstroWCS from AstroHeader
        
        function Obj = read_ctype(Obj)
            % Read Obj.CTYPE to populate the fields: ProjType, ProjClass, CooName, and CUNIT (if empty or nan)
            % Input  : - AstroWCS object.
            % Output : - AstroWCS object with populated fields.
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          AW = AstroWCS(1); AW.CTYPE = {'RA---TAN' 'DEC--TAN'}; AW.read_ctype;
            
            ProjTypeDict = Dictionary('DictName','WCS.ProjType');
            CunitDict = Dictionary('DictName','WCS.CUNIT');
      
            ctype = Obj.CTYPE;
            
            projtype = cell(size(ctype));
            cooname  = cell(size(ctype));
            coounit  = cell(size(ctype));
            
            for I = 1:1:numel(ctype)
                if ~isnan(ctype{I})
                    Split    = regexp(ctype{I},'-','split');
                    Pair     = Split(~tools.cell.isempty_cell(Split));
                    cooname{I}  = Pair{1};
                    if (numel(Pair)>1)
                        projtype{I} = Pair{2};
                        if (numel(Pair)>2)   % For e.g., TAN-SIP. Combine both
                            projtype{I} = [Pair{2} '-' Pair{3}];
                        end
                    end
                    coounit{I} = CunitDict.searchAlt(cooname{I});
                end
            end
            
            % remove cells with no projtype, e.g. velocity
            projtype = projtype(~tools.cell.isempty_cell(projtype));
            
            if isempty(projtype)                        % No projection given
                Obj.ProjType  = 'none';
                Obj.ProjClass = 'none';
            elseif strcmp(projtype{1},projtype{1})     % verify both RA and DEC with the same projection type
                Obj.ProjType = projtype{1};
                Obj.ProjClass = ProjTypeDict.searchAlt(Obj.ProjType);
            else
                error(' Not all Axis are with the same projection');
            end
            
            Obj.CooName = cooname;
            
            
            % if CUNIT is not given in the header, fill from dictionary
            Funit = tools.cell.isnan_cell(Obj.CUNIT) | tools.cell.isempty_cell(Obj.CUNIT);
            Obj.CUNIT(Funit) = coounit(Funit);
            
        end

    end

    methods    % Functions to construct AstroHeader from AstroWCS
   
        function Header = wcs2header(Obj, Header)
            % Convert AstroWCS object to new AstroHeader object or update an existing AstroHeader object
            % Input  : - AstroWCS object.
            %          - Optional AstroHeader object in which to update key/par
            % Output : - AstroHeader object with the WCS keywords
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          AW = AstroWCS(1); AH = AW.wcs2header;

            arguments
                Obj
                Header      = AstroHeader(1);
            end
            
            % Delete all distortion coefficients from original header (if
            % exist). Required in order not to leave old coefficinets that
            % are not overwritten
            Header = deleteDistortionsWCS(Header);
%             Header.deleteKey('PV\d+_\d+');
%             Header.deleteKey('A_\d+_\d+');
%             Header.deleteKey('B_\d+_\d+');
%             Header.deleteKey('AP_\d+_\d+');
%             Header.deleteKey('BP_\d+_\d+');
            
            
            % Add/create all keywords
            KeyCell = Obj.wcs2keyCell;
            Header.replaceVal(KeyCell(:,1),KeyCell(:,2),'Comment',KeyCell(:,3));
        end

        function [Header,RA,Dec] = addCornersCoo2header(Obj, Header, Args)
            % Add RA/Dec of image corners to header
            % Input  : - A single element AstroWCS object.
            %          - A AstroHeader object.
            %          * ...,key,val,...
            %            'CCDSEC' - Either a CCDSEC [Xmin Xmax Ymin Ymax]
            %                   or a keyword header that contains the
            %                   CCSSEC for which the corners should be
            %                   calculated and added.
            %                   Alternatively, this can be a cell array in
            %                   which each element is a CCDSEC or header
            %                   keword. Im this case, corners will be added
            %                   for each CCDSEC.
            %                   If header keyword doesnt exist, then
            %                   corners will not be written to header.
            %                   Default is {'CCDSEC','UNIQSEC'}
            %            'OutKeysRA' - Header kewords in which the RA
            %                   corners should be added. This is a cell
            %                   array of cell arrys per each CCDSEC.
            %                   Default is {{'RA1','RA2','RA3','RA4'}, {'RAU1','RAU2','RAU3','RAU4'}}
            %            'OutKeysDec' - Like 'OutKeysRA', but for Dec.
            %                   Default is {{'DEC1','DEC2','DEC3','DEC4'}, {'DECU1','DECU2','DECU3','DECU4'}}
            %            'OutUnits' - Output units of coordinates.
            %                   Default is 'deg'.
            % Output : - An updated header object.
            %          - RA of corners of last CCDSEC element.
            %          - Dec of corners of last CCDSEC element.
            % Author : Eran Ofek (Apr 2023)
            % Example: [Header,RA,Dec] = addCornersCoo2header(Obj, Header);

            arguments
                Obj(1,1)
                Header(1,1)
                Args.CCDSEC          = {'CCDSEC','UNIQSEC'};
                Args.OutKeysRA cell  = {{'RA1','RA2','RA3','RA4'}, {'RAU1','RAU2','RAU3','RAU4'}};
                Args.OutKeysDec cell = {{'DEC1','DEC2','DEC3','DEC4'}, {'DECU1','DECU2','DECU3','DECU4'}};
                Args.OutUnits        = 'deg';
            end

            if ~iscell(Args.CCDSEC)
                Args.CCDSEC = {Args.CCDSEC};
            end
            Nccdsec = numel(Args.CCDSEC);
            for Iccdsec=1:1:Nccdsec
                if isnumeric(Args.CCDSEC{Iccdsec})
                    % CCDSEC is already numeric

                else
                    % CCDSEC is an header keyword - get value
                    [Args.CCDSEC{Iccdsec}] = getVal(Header, Args.CCDSEC{Iccdsec}, 'ReadCCDSEC',true);
                end

                % CCDSEC to corners
                if ~isnan(Args.CCDSEC{Iccdsec})
                    % only if keyword value exist
                    Corners   = Args.CCDSEC{Iccdsec}([1 3; 2 3; 2 4 ;1 4]);
                    [RA, Dec] = Obj.xy2sky(Corners(:,1), Corners(:,2), 'OutUnits',Args.OutUnits);

                    Header = replaceVal(Header,[Args.OutKeysRA{Iccdsec}, Args.OutKeysDec{Iccdsec}] ,[RA(:).', Dec(:).']);
                end

            end

       end

   
       function KeyCell = wcs2keyCell(Obj)
            % Create a cell array of WCS fields from AstroWCS object
            % Input  : - AstroWCS object.
            % Output : - Cell array of {keyname, keyval, description}
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          AW = AstroWCS(1); KeyCell = AW.wcs2keyCell;
         
           % Initiate cell array
           KeyCell = cell(0,3);
           
           %Add field by field
           
           if ~isempty(Obj.NAXIS)
               AddCell = {'NAXIS', Obj.NAXIS, 'Number of axes'};
               KeyCell = [KeyCell; AddCell];
           end
           if ~isempty(Obj.WCSAXES)
               AddCell = {'WCSAXES', Obj.WCSAXES, 'WCS dimensionality'};
               KeyCell = [KeyCell; AddCell];
           end
           if ~isempty(Obj.RADESYS)
               AddCell = {'RADESYS', Obj.RADESYS, 'Astrometric system'};
               KeyCell = [KeyCell; AddCell];
           end
           if ~isempty(Obj.EQUINOX)
               AddCell = {'EQUINOX', Obj.EQUINOX, 'Equinox'};
               KeyCell = [KeyCell; AddCell];
           end
           if ~isempty(Obj.LONPOLE)
               AddCell = {'LONPOLE', Obj.LONPOLE, 'Native Longitude of the Celestial Pole'};
               KeyCell = [KeyCell; AddCell];
           end
           if ~isempty(Obj.LATPOLE)
               AddCell = {'LATPOLE', Obj.LATPOLE, 'Native Latitude of the Celestial Pole'};
               KeyCell = [KeyCell; AddCell];
           end
           
           Naxis = Obj.WCSAXES;
           % verify size
           if (numel(Obj.CTYPE)~=Naxis) || (numel(Obj.CUNIT)~=Naxis)
               error('Wrong size of CTYPE or CUNIT');
           end
           
           for Ix = 1:1:Naxis
               AddCell = {sprintf('CTYPE%d',Ix), Obj.CTYPE{Ix}, 'WCS projection type for this axis'};
               KeyCell = [KeyCell; AddCell];
           end
           
           for Ix = 1:1:Naxis
               AddCell = {sprintf('CUNIT%d',Ix), Obj.CUNIT{Ix}, 'Axis unit '};
               KeyCell = [KeyCell; AddCell];
           end

           % verify size
           if (numel(Obj.CRPIX)~=Naxis) || (numel(Obj.CRVAL)~=Naxis) ||...
              (size(Obj.CD,1)~=Naxis) || (size(Obj.CD,2)~=Naxis)
               error('Wrong size of CRPIX or CRVAL or CD');
           end
           
           for Ix = 1:Naxis
               AddCell = {sprintf('CRPIX%d',Ix), Obj.CRPIX(Ix), 'Reference pixel on this axis'};
               KeyCell = [KeyCell; AddCell];
           end
              
           for Ix = 1:Naxis
               AddCell = {sprintf('CRVAL%d',Ix), Obj.CRVAL(Ix), 'World coordinate of reference pixel on this axis'};
               KeyCell = [KeyCell; AddCell];
           end
           
           for Ix1 = 1:Naxis
               for Ix2 = 1:Naxis
                   AddCell = {sprintf('CD%d_%d',Ix1,Ix2), Obj.CD(Ix1,Ix2), 'Linear projection matrix'};
                   KeyCell = [KeyCell; AddCell];
               end
           end
           
           for Ix = 1:numel(Obj.PV.KeyNamesX)
               AddCell = {Obj.PV.KeyNamesX{Ix}, Obj.PV.PolyCoefX(Ix), 'Projection distortion coefficient'};
               KeyCell = [KeyCell; AddCell];
           end
           
           for Ix = 1:numel(Obj.PV.KeyNamesY)
               AddCell = {Obj.PV.KeyNamesY{Ix}, Obj.PV.PolyCoefY(Ix), 'Projection distortion coefficient'};
               KeyCell = [KeyCell; AddCell];
           end
           
           for Ix = 1:numel(Obj.RevPV.KeyNamesX)
               AddCell = {Obj.RevPV.KeyNamesX{Ix}, Obj.RevPV.PolyCoefX(Ix), 'Projection distortion coefficient'};
               KeyCell = [KeyCell; AddCell];
           end
           
           for Ix = 1:numel(Obj.RevPV.KeyNamesY)
               AddCell = {Obj.RevPV.KeyNamesY{Ix}, Obj.RevPV.PolyCoefY(Ix), 'Projection distortion coefficient'};
               KeyCell = [KeyCell; AddCell];
           end
           
           switch lower(Obj.ProjType)
               case 'tan-sip'
                   AddCell = {'A_ORDER', numel(Obj.PV.KeyNamesX), 'Polynomial order, axis 1'};
                   KeyCell = [KeyCell; AddCell];
                   AddCell = {'B_ORDER', numel(Obj.PV.KeyNamesY), 'Polynomial order, axis 2'};
                   KeyCell = [KeyCell; AddCell];
                   AddCell = {'AP_ORDER', numel(Obj.RevPV.KeyNamesX), 'Polynomial order, axis 1'};
                   KeyCell = [KeyCell; AddCell];
                   AddCell = {'BP_ORDER', numel(Obj.RevPV.KeyNamesY), 'Polynomial order, axis 1'};
                   KeyCell = [KeyCell; AddCell];
           end
          
       end
        
    end

    methods   % Functions related to xy2sky
   
        function [Alpha, Delta]  = xy2sky(Obj,PX,PY,Args)
            % Convert pixel coordinates to celestial coordinates
            % Input  : - A single element AstroWCS object
            %          - A matrix of pixel X coordinates.
            %            If next argument is not provided or empty then this is a
            %            two or more column matrix of [PX,PY,...]
            %          - A matrix of pixel Y coordinates.
            %          * ...,key,val,...
            %            'OutUnits'          - Output units. Default is 'deg'.
            %            'IncludeDistortions'- Flag to include distoration.
            %                                  Default is: true.
            %            'useTran2D'         - Flag to use Tran2D object directly.
            %                                  Currently not supported.
            %                                  Default is: false.
            % Output : - A two column matrix of [Alpha, Delta] or a matrix of
            %            Alpha coordinates.
            %          - A matrix of Delta coordinates.
            %            If not asked for, then the first output will be a
            %            two column matrix.
            % Author : Yossi Shvartzvald (August 2021)
            % Example: [Alpha,Delta] = Obj.xy2sky(1,1);
            %
        
            arguments
                Obj
                PX
                PY                       = [];
                Args.OutUnits            = 'deg';
                Args.IncludeDistortions  = true;
                Args.useTran2D           = false;
            end
            
            if Args.useTran2D
                error('Currently do not upport direct use of Tran2D');
            end

            if numel(Obj)~=1
                error('Works only on a single element AstroWCS object');
            end

            if Obj.WCSAXES>2
                error('Currently do not support WCSAXES>2');
            end
            
            if isempty(PY)
                PY = PX(:,2);
                PX = PX(:,1);
            end

            % pixel to intermediate (in units of CUNIT) including distortion
            [Xd,Yd] = Obj.pix2interm(PX,PY,Args.IncludeDistortions);
            
            % intermediate to native
            [Phi,Theta] = Obj.interm2native(Xd,Yd,'InUnits',Obj.CUNIT{1},'OutUnits','rad');
            
            % native to celestial
            [Alpha, Delta] = Obj.native2celestial(Phi,Theta,'InUnits','rad','OutUnits',Args.OutUnits);
    
        end
              
        function [X,Y]=pix2interm(Obj,PX,PY,includeDistortion)
            % Convert pixel coordinates (P) to intermediate coordinates (X), if requested also include distortion
            % Input  : - A single element AstroWCS object
            %          - A matrix of pixel X coordinate.
            %            If next argument is not provided then this is a
            %            two column matrix of [PX,PY].
            %          - A matrix of pixel Y coordinate.
            %            'includeDistortion' - Flag to include distoration.
            %                                  Default is: true.
            % Output : - A matrix of X intermediate coordinate.
            %          - A matrix of Y intermediate coordinate.
            %            The intermediate coordinates units are specified in
            %            CUNIT.
            % Author : Yossi Shvartzvald (August 2021)
            % Example: [X,Y]= Obj.pix2interm(1,1);

        
            arguments
                Obj
                PX
                PY                       = [];
                includeDistortion        = true;
            end

            if numel(Obj)~=1
                error('The wcsCl object input must contain a single element');
            end


            if isempty(PY)
                PY = PX(:,2);
                PX = PX(:,1);
            end
            
            P = [PX(:), PY(:)].';
            relP = (P - Obj.CRPIX(:));
            
            % distorion for TAN-SIP
            if strcmpi(Obj.ProjType,'tan-sip') && includeDistortion
                
                u = relP(1,:);
                v = relP(2,:);
                
                [U,V]  = AstroWCS.forwardDistortion(Obj.PV,u,v,'plusXY_bool',true);  % U = u+f(u,v) and V = v+g(u,v)
                
                relP = [U ; V];
            end

            
            if Obj.NAXIS~=size(P,2) && Obj.NAXIS~=size(Obj.CD,1) && Obj.NAXIS~=size(Obj.CD,2)
                error('Number of coordinates must be consistent with number of axes and CD matrix');
            end

            XY = (Obj.CD*relP).';

            X = reshape(XY(:,1),size(PX));
            Y = reshape(XY(:,2),size(PY));
            
            % distorion for TPV
            if strcmpi(Obj.ProjType,'tpv') && includeDistortion
                
                R = sqrt(X.^2 + Y.^2);

                [X,Y]  = AstroWCS.forwardDistortion(Obj.PV,X,Y,'R',R);

            end

        end
        
        function [Phi,Theta]=interm2native(Obj,X,Y,Args)
            % Project intermediate coordinates to native coordinates
            % Input  : - AstroWCS object
            %          - A matrix of intermediate X coordinate.
            %            If next argument (Y) is not orovided or empty then this
            %            is a two column matrix of [X,Y].
            %          - A matrix of intermediate Y coordinate.
            %          * ...,key,val,...
            %            'InUnits '  - Input intermediate coordinates units.
            %                          Default is 'deg'.
            %            'OutUnits ' - Output native coordinates units.
            %                          Default is 'deg'.
            % Output : - A matrix of native Phi coordinate.
            %          - A matrix of native Theta coordinate.
            % Author : Yossi Shvartzvald (August 2021)
            % Example: [Phi,Theta]=Obj.interm2native(100,100);

            arguments
                Obj
                X
                Y                       = [];
                Args.InUnits            = 'deg';
                Args.OutUnits           = 'deg';
            end
            

            if numel(Obj)~=1
                error('Works on a single element wcsCl object');
            end

            if isempty(Y)
                Y = X(:,2);
                X = X(:,1);
            end

            % convert to deg
            ConvFactor = convert.angular(Args.InUnits,'deg');
            X   = X.*ConvFactor;
            Y   = Y.*ConvFactor;


            switch lower(Obj.ProjClass)
                case 'zenithal'

                    switch lower(Obj.ProjType)
                        case {'tan','tpv','tan-sip'}
                            Rtheta = sqrt(X.^2 + Y.^2);        % deg
                            Theta  = atan(180./(pi.*Rtheta));  % rad
                            Phi    = atan2(X,-Y);              % rad

                        otherwise

                            error('Unsupported projection option (%s)',Obj.ProjType);
                    end

                otherwise
                    error('Unsupported projection class (%s)',Obj.ProjClass);
            end

            ConvFactor = convert.angular('rad',Args.OutUnits);
            Theta = Theta.*ConvFactor;
            Phi   = Phi.*ConvFactor;

        end
        
        function [Alpha,Delta]=native2celestial(Obj,Phi,Theta,Args)
            % Convert native coordinates to celestial coordinates
            % Input  : - A single element AstroWCS object.
            %          - A matrix of phi (native) coordinates.
            %            If the next input argument is empty, then this is
            %            a two column matrix of [phi,theta] coordinates.
            %          - A matrix of Theta (native) coordinates.
            %          * ...,key,val,...
            %            'InUnits '  - Input intermediate coordinates units.
            %                          Default is 'deg'.
            %            'OutUnits ' - Output native coordinates units.
            %                          Default is 'deg'.
            % Output : - A matrix of celestial (Alpha) coordinates.
            %          - A matrix of celestial (Delta) coordinates.
            % Author : Yossi Shvartzvald (August 2021)
            % Example: [Alpha,Delta] = Obj.native2celestial([1 1],[2 2]);

            arguments
                Obj
                Phi
                Theta                   = [];
                Args.InUnits            = 'deg';
                Args.OutUnits           = 'deg';
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
            ConvFactorIn = convert.angular(Args.InUnits,'rad');

            [Alpha,Delta]=Obj.phitheta2alphadelta(Phi.*ConvFactorIn,Theta.*ConvFactorIn,...
                                Obj.PhiP.*ConvFactorW,Obj.AlphaP.*ConvFactorW,Obj.DeltaP.*ConvFactorW,'rad');

            ConvFactor = convert.angular('rad',Args.OutUnits);
            Alpha = Alpha.*ConvFactor;
            Delta = Delta.*ConvFactor;


        end
        
    end
   
    methods  % Functions related to sky2xy
        function [PX,PY]  = sky2xy(Obj,Alpha,Delta,Args)
            % Convert celestial coordinates to pixel coordinates
            % Input  : - A single element AstroWCS object
            %          - A matrix of Alpha coordinates.
            %            If next argument is not provided or empty then this is a
            %            two or more column matrix of [Alpha,...]
            %          - A matrix of pixel Delta coordinates.
            %          * ...,key,val,...
            %            'InUnits'          - Output units. Default is 'deg'.
            %            'includeDistortion' - Flag to include distoration.
            %                                  Default is: true.
            %            'useTran2D'         - Flag to use Tran2D object directly.
            %                                  Currently not supported.
            %                                  Default is: false.
            % Output : - A matrix of PX coordinates.
            %          - A matrix of PY coordinates.
            % Author : Yossi Shvartzvald (August 2021)
            % Example: [PX,PY] = Obj.coo2xy(Obj,100,10);
            
            arguments
                Obj
                Alpha
                Delta                    = [];
                Args.InUnits             = 'deg';
                Args.includeDistortion   = true;
                Args.useTran2D           = false;
            end
            
            if Args.useTran2D
                error('Currently do not upport direct use of Tran2D');
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end
            
            if Obj.WCSAXES>2
                error('Currently do not support WCSAXES>2');
            end

            if isempty(Delta)
                Delta = Alpha(:,2);
                Alpha = Alpha(:,1);
            end

            if (iscell(Delta) || ischar(Delta)) && (iscell(Alpha) || ischar(Alpha))
                Delta = celestial.coo.convertdms(Delta,'gH','r');
                Alpha = celestial.coo.convertdms(Alpha,'gD','R');
                Args.InUnits = 'rad';
            end

            % celestial to native
            [Phi,Theta] = Obj.celestial2native(Alpha(:),Delta(:),'InUnits',Args.InUnits,'OutUnits','rad');
            
            % native to intermediate
            [X,Y] = Obj.native2interm(Phi, Theta,'InUnits','rad','OutUnits',Obj.CUNIT{1});
            
            % Intermediate to pixel
            [PX,PY] = Obj.interm2pix(X,Y,Args.includeDistortion);

            PX = reshape(PX,size(Delta));
            PY = reshape(PY,size(Alpha));
        end
        
        function [Phi,Theta]=celestial2native(Obj,Alpha,Delta,Args)
            % Convert celestial coordinates to native coordinates
            % Input  : - A single element AstroWCS object.
            %          - A matrix of longiudes (Alpha).
            %            If the next input argument (matrix of latitude)
            %            is empty, then this can be a two column matrix of
            %            [alpha,delta] coordinates
            %            or a three column matrix of cosine directions.
            %          - A matrix of latitudes (Delta).
            %          * ...,key,val,...
            %            'InUnits '  - Input intermediate coordinates units.
            %                          Default is 'deg'.
            %            'OutUnits ' - Output native coordinates units.
            %                          Default is 'deg'.
            % Output : - A matrix of native Phi coordinate.
            %          - A matrix of native Theta coordinate.
            % Author : Yossi Shvartzvald (August 2021)
            % Example: [Phi,Theta]=Obj.celestial2native([1 1],[2 2]);
            
            arguments
                Obj
                Alpha
                Delta                   = [];
                Args.InUnits            = 'deg';
                Args.OutUnits           = 'deg';
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
                    Args.InUnits = 'rad';
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
            ConvFactorIn = convert.angular(Args.InUnits,'rad');

            [Phi,Theta] = AstroWCS.alphadelta2phitheta(Alpha.*ConvFactorIn,Delta.*ConvFactorIn,...
                                    Obj.PhiP.*ConvFactorW, Obj.AlphaP.*ConvFactorW, Obj.DeltaP.*ConvFactorW, 'rad');

            ConvFactor = convert.angular('rad',Args.OutUnits);
            Phi   = Phi.*ConvFactor;
            Theta = Theta.*ConvFactor;

        end
   
        function [X,Y]=native2interm(Obj,Phi,Theta,Args)
            % Project native coordinates to intermediate coordinates
            % Input  : - A AstroWCS object
            %          - A matrix of native Phi coordinate.
            %            If the next input argument (Theta) is not provided
            %            then this is a two column matrix of [Phi,Theta]
            %            native coordinates.
            %          - A matrix of native Theta coordinate.
            %          * ...,key,val,...
            %            'InUnits '  - Input intermediate coordinates units.
            %                          Default is 'deg'.
            %            'OutUnits ' - Output native coordinates units.
            %                          Default is 'deg'.
            % Output : - A matrix of X intermediate pixel coordinate.
            %          - A matrix of Y intermediate pixel coordinate.
            % Author : Yossi Shvartzvald (August 2021)
            % Example: [X,Y]=Obj.native2interm(100,100);
         
            arguments
                Obj
                Phi
                Theta                   = [];
                Args.InUnits            = 'deg';
                Args.OutUnits           = 'deg';
            end

            if numel(Obj)~=1
                error('Works on a single element wcsCl object');
            end

            if isempty(Theta)
                Theta = Phi(:,2);
                Phi   = Phi(:,1);
            end

            % convert native coordinates to radians
            ConvFactor = convert.angular(Args.InUnits,'rad');
            Phi   = Phi.*ConvFactor;
            Theta = Theta.*ConvFactor;


            % phi,theta -> X,Y

            switch lower(Obj.ProjClass)
                case 'zenithal'

                    switch lower(Obj.ProjType)
                        case {'tan','tpv','tan-sip'}
                            Rtheta = 180./pi .* cot(Theta);    % deg
                            X      = Rtheta.*sin(Phi);
                            Y      = -Rtheta.*cos(Phi);

                        otherwise

                            %Rtheta = 180./pi .* (Mu + 1).*cos(Theta)./(Mu + sin(Theta);

                            error('Unsupported projection option (%s)',Obj.ProjType);
                    end

                otherwise
                    error('Unsupported projection class (%s)',Obj.ProjClass);
            end
            
            
            ConvFactor = convert.angular('deg',Args.OutUnits);
            X   = X.*ConvFactor;
            Y   = Y.*ConvFactor;
            

        end
        
        function [PX,PY]=interm2pix(Obj,X,Y,includeDistortion)
            % Convert intermediate pixel coordinates to pixel coordinates, if requested also include distortion
            % Input  : - A single element AstroWCS object.
            %          - A matrix of X intermediate pixel coordinate.
            %            If next argument (Y) is not provided then this is
            %            a two column matrix of [X,Y].
            %          - A matrix of Y intermeditae pixel coordinate.
            %          - Flag to include distoration. Default is: true.
            % Output : - A matrix of X pixel coordinate.
            %          - A matrix of Y pixel coordinate.
            % Author : Yossi Shvartzvald (August 2021)
            % Example: [PX,PY]=Obj.interm2pix(1,1)
            
            arguments
                Obj
                X
                Y                       = [];
                includeDistortion        = true;
            end
            
             if numel(Obj)~=1
                error('The wcsCl object input must contain a single element');
            end

            if Obj.NAXIS~=size(Obj.CD,1) && Obj.NAXIS~=size(Obj.CD,2)
                error('Number of coordinates must be consistent with number of axss and CD matrix');
            end
                
            if isempty(Y)
                Y = X(:,2);
                X = X(:,1);
            end
            
            % inverse distorion for TPV
            if strcmpi(Obj.ProjType,'tpv') && includeDistortion
                
                err_thresh = 1e-7;
                max_iters = 20;
                    
                [X,Y]  = AstroWCS.backwardDistortion(Obj.PV,X,Y,'plusXY_bool',false,'Threshold',err_thresh,'MaxIter',max_iters);
            end
                 
            XY = [X(:), Y(:)];
            
            relP = (Obj.CD) \ XY.';
            
            % inverse distorion for TAN-SIP
            if strcmpi(Obj.ProjType,'tan-sip')  && includeDistortion
                
                U = relP(1,:);
                V = relP(2,:);
                    
                if ~isempty(Obj.RevPV.PolyCoefX)
                    [u,v]  = AstroWCS.forwardDistortion(Obj.RevPV,U,V,'plusXY_bool',true); % u = U + F(U,V), v = V+G(U,V)
                else

                    err_thresh = 1e-6;
                    max_iters = 20;

                    [u,v]  = AstroWCS.backwardDistortion(Obj.PV,U,V,'plusXY_bool',true,'Threshold',err_thresh,'MaxIter',max_iters);

                end
                
                relP = [u ; v];
            end
                      
            P = (relP + Obj.CRPIX(:)).';
            
            PX = reshape(P(:,1),size(X));
            PY = reshape(P(:,2),size(Y));

        end
        
        function [InImage, MinDist] = isSkyCooInImage(Obj, Alpha, Delta, CCDSEC, Units)
            % Check if RA/Dec are within image footprint (CCDSEC)
            % Input  : - A single element AstroWCS object.
            %          - J2000.0 R.A.
            %          - J2000.0 Dec.
            %          - CCDSEC. Either [xmin xmax ymin ymax] or
            %            [xmax, ymax].
            %          - Input RA/Dec units. Default is 'deg'.
            % Output : - A vector of logicals indicating, for each
            %            coordinate, if it is inside CCDSEC footprint.
            %          - Vector of minimum distance of each position from
            %            image boundries.
            % Author : Eran Ofek (Jan 2023)

            arguments
                Obj(1,1)
                Alpha
                Delta
                CCDSEC
                Units    = 'deg';
            end

            if numel(CCDSEC)==4
                % do nothing
            elseif numel(CCDSEC)==2
                CCDSEC = [1 CCDSEC(1) 1 CCDSEC(2)];
            else
                error('CCDSEC must contains 2 or 4 elements');
            end

            [PX, PY] = sky2xy(Obj, Alpha, Delta, 'InUnits',Units);

            InImage = PX>CCDSEC(1) & PX<CCDSEC(2) & PY>CCDSEC(3) & PY<CCDSEC(4);

            if nargout>1
                MinDist=tools.math.geometry.dist_box_edge(PX, PY, CCDSEC(1:2), CCDSEC(3:4));
            end
        end
    
        function Result = cooImage(Obj, CCDSEC, Args)
            % Return the image center and corners coordinates
            % Input  : - A single element AstroWCS object.
            %          - CCDSEC [Xmin, Xmax, Ymin, Ymax]
            %          * ...,key,val,...
            %            'OutUnits' - Output units. Default is 'deg'.
            % Output : - A structure containing:
            %            .Center - [RA, Dec] of center (of CCDSEC).
            %            .Corners - [RA, Dec] of 4 image corners.
            % Author : Eran Ofek (Jan 2023)
            % Example: RR=AI.WCS.cooImage([1 1000 1 1000])

            arguments
                Obj(1,1)
                CCDSEC(1,4)      = [];
                Args.OutUnits    = 'deg';
            end

            % X/Y contains [center + 4 corners]
            X = [(CCDSEC(1) + CCDSEC(2)).*0.5; CCDSEC(1); CCDSEC(1); CCDSEC(2); CCDSEC(2)];
            Y = [(CCDSEC(3) + CCDSEC(4)).*0.5; CCDSEC(3); CCDSEC(4); CCDSEC(4); CCDSEC(3)];
            [RA, Dec] = Obj.xy2sky(X, Y, 'OutUnits',Args.OutUnits);

            Result.Center  = [RA(1), Dec(1)];
            Result.Corners = [RA(2:end), Dec(2:end)];
        end
    end
    
    methods  % Functions related to xy2refxy
         function [RefPX,RefPY,PX,PY]  = xy2refxy(Obj,XY,RefWCS,Args)
            % Given X, Y coordinates in an image, convert to X,Y in reference coordinate system.
            %   by using WCS info of both images to tranlstae XY to refXY.
            % Input  : - A single element AstroWCS object.
            %          - Either a four element region (i.e., CCDSEC)
            %            [xmin,xmax,ymin,ymax].
            %            This CCDSEC represents the image of the input
            %            AstroWCS object.
            %            Alternatively a two column matrix of [X Y] image size.
            %          - A single refence AstroWCS object
            %          * ...,key,val,...
            %            'Sampling' - step size for sampling CCDSEC region.
            %                   Default is 1
            % Output : - Translated X pixel coordinates in reference image
            %          - Translated Y pixel coordinates in reference image
            %          - X pixel coordinates used
            %          - Y pixel coordinates used
            % Author : Yossi Shvartzvald (Dec 2021)
            % Example:
            %      [D,refPX,refPY,PX,PY]=Obj.xy2refxy([1,100,1,100],refWCS);
            
            arguments
                Obj
                XY
                RefWCS(1,1) AstroWCS
                Args.Sampling       = 1;
            end
            
            switch size(XY,2)
                case 4                              % i.e., CCDSEC
                    if size(XY,1)>1
                        error('wrong XY dimensions');
                    else
                        VecX = (XY(1):Args.Sampling:XY(2));
                        if VecX(end)~=XY(2)
                            VecX = [VecX, XY(2)];
                        end
                        VecY = (XY(3):Args.Sampling:XY(4));
                        if VecY(end)~=XY(4)
                            VecY = [VecY, XY(4)];
                        end
                        %[PX,PY] = meshgrid(XY(1):Args.Sampling:XY(2),XY(3):Args.Sampling:XY(4));
                        [PX,PY] = meshgrid(VecX, VecY);
                    end
                case 2                              % matrix of xy
                   PX = XY(:,1);
                   PY = XY(:,2);
                otherwise
                    error('wrong XY dimensions');
            end
                    
            [Alpha, Delta] = Obj.xy2sky(PX,PY);
            [RefPX,RefPY]  = RefWCS.sky2xy(Alpha,Delta);
           

         end

        function [D,refPX,refPY,PX,PY]  = xy2refxyDisp(Obj,XY,RefWCS,Args)
            % Calculate the displacement field D between current image to refernce image,
            %   by using WCS info of both images to tranlstae XY to refXY.
            % Input  : - A single element AstroWCS object.
            %          - Either a four element region (i.e., CCDSEC)
            %            [xmin,xmax,ymin,ymax].
            %            This CCDSEC represents the image of the input
            %            AstroWCS object.
            %            Alternatively a two column matrix of [X Y] image size.
            %          - A single refence AstroWCS object
            %          * ...,key,val,...
            %            'Sampling' - step size for sampling CCDSEC region.
            %                   Default is 1
            % Output : - Displacement field matrix.
            %          - Translated X pixel coordinates in reference image
            %          - Translated Y pixel coordinates in reference image
            %          - X pixel coordinates used
            %          - Y pixel coordinates used
            % Author : Yossi Shvartzvald (Dec 2021)
            % Example:
            %      [D,refPX,refPY,PX,PY]=Obj.xy2refxy([1,100,1,100],refWCS);
            
            arguments
                Obj
                XY
                RefWCS(1,1) AstroWCS
                Args.Sampling       = 1;
            end
            
            switch size(XY,2)
                case 4                              % i.e., CCDSEC
                    if size(XY,1)>1
                        error('wrong XY dimensions');
                    else
                        [PX,PY] = meshgrid(XY(1):Args.Sampling:XY(2),XY(3):Args.Sampling:XY(4));
                    end
                case 2                              % matrix of xy
                   PX = XY(:,1);
                   PY = XY(:,2);
                otherwise
                    error('wrong XY dimensions');
            end
                    
            [Alpha, Delta] = Obj.xy2sky(PX,PY);
            [refPX,refPY]  = RefWCS.sky2xy(Alpha,Delta);
            DX = PX-refPX;
            DY = PY-refPY;
            
            if Args.Sampling>1
                D(:,:,1) = imresize(DX, [XY(4),XY(2)]);
                D(:,:,2) = imresize(DY, [XY(4),XY(2)]);
            else
                D(:,:,1) = DX;
                D(:,:,2) = DY;
            end
        end
        
    end

    methods % plots
        function varargout=plotRMS(Obj, Args)
            % plot RMS vs. magnitude plot for astrometric solution
            % Input  : - A single element AstroWCS object
            % Output : - Handle for plot.
            % Author : Eran Ofek (Jan 2023)
            % Example: AllSI(1).WCS.plotRMS

            arguments
                Obj(1,1)
                Args.OnlyGood logical   = false;
            end

            Flag = Obj.ResFit.FlagSrc;
            Hp = semilogy(Obj.RefMag(Flag), Obj.Resid(Flag).*3600,'k.');
            if ~Args.OnlyGood
                hold on;
                semilogy(Obj.RefMag(~Flag), Obj.Resid(~Flag).*3600,'r.');
                hold off;
            end
            
            H = xlabel('mag');
            H.Interpreter = 'latex';
            H.FontSize    = 18;

            H = ylabel('Residuals [arcsec]');
            H.Interpreter = 'latex';
            H.FontSize    = 18;

            if nargout>0
                varargout{1} = Hp;
            end
        end
    end
    
    methods (Static)  % static methods %======== Functions to construct AstroWCS from AstroHeader =========
        
        function Result = header2wcs(AH,Args)
            % Create and populate an AstroWCS object from an AstroHeader object
            % Input  : - AstroHeader object.
            %          * ...,key,val,...
            %            'read2axes' - Flag to read ONLY first 2 axis. Can
            %                          be used to ignore 3rd and up axis.
            %                          Default is false.
            % Output : - AstroWCS object.
            % Author : Yossi Shvartzvald (December 2021)
            % Example:
            %           AH = AstroHeader(Im_name); AW = AstroWCS.header2wcs(AH);
            
            arguments
                AH
                Args.read2axes     =  false;
            end
            
            Nobj   = numel(AH);
            Result = AstroWCS(size(AH));
            for Iobj=1:1:Nobj
                
                if AH(Iobj).numKeys==0
                    warning('Can not generate WCS because header is empty');
                else
                    % Read all single val parmeters
                    KeyValStruct = AH(Iobj).getStructKey({'NAXIS','WCSAXES','LONPOLE','LATPOLE'});
                    
                    % if WCSAXES is not available use NAXIS as default
                    Result(Iobj).NAXIS = KeyValStruct.NAXIS;
                    Result(Iobj).WCSAXES = KeyValStruct.WCSAXES;
                    if (Result(Iobj).WCSAXES==0)
                        Result(Iobj).WCSAXES = Result(Iobj).NAXIS;
                    end
                    
                    if Args.read2axes
                        Result(Iobj).WCSAXES = 2;
                        Result(Iobj).NAXIS   = 2;
                    end
                
                    Naxis = Result(Iobj).WCSAXES;
    
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
                    Result(Iobj).CTYPE = AH(Iobj).getCellKey(KeyCtype);
                    Result(Iobj).CUNIT = AH(Iobj).getCellKey(KeyCunit);
                    Result(Iobj).read_ctype;
                
                    if isempty(Result(Iobj).CTYPE) || (isnumeric(Result(Iobj).CTYPE{1}) && any(isnan(Result(Iobj).CTYPE{1})))
                        Result(Iobj).Success = false;
                    else
                        [Result(Iobj).RADESYS, Result(Iobj).EQUINOX] = Result(Iobj).read_radesys_equinox(AH(Iobj));
    
                        % Get base WCS info
                        if ~isnan(KeyValStruct.LONPOLE)
                            Result(Iobj).LONPOLE = KeyValStruct.LONPOLE;
                        end
                        if ~isnan(KeyValStruct.LATPOLE)
                            Result(Iobj).LATPOLE = KeyValStruct.LATPOLE;
                        end
    
                        Result(Iobj).CRPIX = cell2mat(AH(Iobj).getCellKey(KeyCrpix));
                        Result(Iobj).CRVAL = cell2mat(AH(Iobj).getCellKey(KeyCrval));
                        Result(Iobj).CD = Result(Iobj).build_CD(AH(Iobj),Naxis);
    
                        % Read distortions
                        % look for PV coeficients
                        Result(Iobj).PV = Result(Iobj).build_PV_from_Header(AH(Iobj), Result(Iobj).ProjType);
    
                        % For TAN-SIP try to get RevPV (TODO generlize)
                        if strcmpi(Result(Iobj).ProjType,'tan-sip')
                            Result(Iobj).RevPV = AstroWCS.build_TANSIP_from_Header(AH(Iobj),true);
                        end
    
                        % populate proj Meta
                        Result(Iobj).populate_projMeta;
    
                        % assume header solution is good
                        Result(Iobj).Success = true;
                    end
                end
            end
        end
                
        function Result = xrayHeader2wcs(AH, Args)
            % Read X-Ray telescope/mission header into WCS object
            %   Note that the sky2xy/xy2sky are relevant for the "Detector"
            %   coordinates in ds9.
            % Input  : - An AstroHeader object, or a cell array of header.
            %          * ...,key,val,...
            %            'Mission' - X-Ray mission. Default is 'chandra'.
            %            'Num1' - Number suffix in X coordinate keywords.
            %                   Default is 11.
            %            'Num2' - Number suffix in Y coordinate keywords.
            %                   Default is 12.
            % Output : - An AstroWCS object
            % Author : Eran Ofek (Feb 2022)
            
            arguments
                AH
                Args.Mission        = 'chandra';
                Args.Num1           = 11;
                Args.Num2           = 12;
                
            end
            
            Keys.CTYPE{2} = sprintf('%s%d','TCTYP',Args.Num2);
            Keys.CTYPE{1} = sprintf('%s%d','TCTYP',Args.Num1);
            
            Keys.CRVAL{2} = sprintf('%s%d','TCRVL',Args.Num2);
            Keys.CRVAL{1} = sprintf('%s%d','TCRVL',Args.Num1);
            
            Keys.CRPIX{2} = sprintf('%s%d','TCRPX',Args.Num2);
            Keys.CRPIX{1} = sprintf('%s%d','TCRPX',Args.Num1);
            
            Keys.CDELT{2} = sprintf('%s%d','TCDLT',Args.Num2);
            Keys.CDELT{1} = sprintf('%s%d','TCDLT',Args.Num1);
            
            Keys.CUNIT{2} = sprintf('%s%d','TCUNI',Args.Num2);
            Keys.CUNIT{1} = sprintf('%s%d','TCUNI',Args.Num1);
            
            if isa(AH, 'AstroHeader')
                Nobj = numel(AH);
            else
                % assume a single header in a cell array
                Nobj = 1;
            end
            
            Result = AstroWCS;
            for Iobj=1:1:Nobj
            
                Result(Iobj).NAXIS      = 2;
                Result(Iobj).WCSAXES    = 2;

                if isa(AH, 'AstroHeader')
                    CellHeader = AH(Iobj).Data;
                else
                    % assume input is a cell array
                    CellHeader = AH;
                end
            
                FieldNames = fieldnames(Keys);
                Nfn        = numel(FieldNames);
                for Ifn=1:1:Nfn
                    [Val1] = imUtil.headerCell.getValBySynonym(CellHeader, Keys.(FieldNames{Ifn}){1});
                    [Val2] = imUtil.headerCell.getValBySynonym(CellHeader, Keys.(FieldNames{Ifn}){2});
                    switch FieldNames{Ifn}
                        case 'CDELT'
                            Result(Iobj).CD      = zeros(2,2);
                            Result(Iobj).CD(1,1) = Val1;
                            Result(Iobj).CD(2,2) = Val2;
                        case {'CTYPE','CUNIT'}
                            % cell arrays
                            Result(Iobj).(FieldNames{Ifn}) = {Val1, Val2};
                        otherwise
                            % vectors
                            Result(Iobj).(FieldNames{Ifn}) = [Val1, Val2];
                    end
                    % populate proj Meta
                    Result(Iobj).read_ctype;
                    Result(Iobj).populate_projMeta;
                    
                end
            end
            
            
        end
            
        function [radesys,equinox] =read_radesys_equinox(Header)
            % Read from AstroHeader the RADESYS and EQUINOX. If any are missing fill with deafults.
            % Input  : - AstroHeader object.
            % Output : - Astrometric system, e.g., 'ICRS', 'FK5', 'FK4'
            %          - EQUINOX
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          AH = AstroHeader(Im_name); [radesys,equinox] = AstroWCS.read_radesys_equinox(AH);
            
            AH = Header;
            
            radesys = AH.getVal({'RADESYS','RADECSYS'});
            equinox = AH.getVal('EQUINOX');
            
            % cases where radesys or equinox are not given
            if any(isnan(radesys)) && ~isnan(equinox) % no radesys
                
                if equinox < 1984.0
                    radesys ='FK4';
                else
                    radesys ='FK5';
                end
                
            elseif ~any(isnan(radesys)) && isnan(equinox) % no equinox
                
                switch radesys
                    case{'FK4','FK4-NO-E'}
                        equinox = 1950.0;
                    case 'FK5'
                        equinox = 2000.0;
                    otherwise
                        equinox = [];
                end
                             
            elseif any(isnan(radesys)) && isnan(equinox) % no radesys nor equinox
                radesys = 'ICRS';
                equinox = 2000.0;
                
            end
            
        end
                
        function CD = build_CD(Header,Naxis)
            % Construct the CD matrix from AstroHeader. Either directly CD matrix, or PC+CDELT, or CDELT
            % Input  : - AstroHeader object.
            %          - Number of WCS axes.
            % Output : - CD matrix
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          AH = AstroHeader(Im_name); CD = AstroWCS.build_CD(AH,2);
            
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
       
        function PV = build_PV_from_Header(Header,ProjType)
            % Construct a PV (distoration) structure from AstroHeader.
            % Input  : - AstroHeader object.
            %          - Projection type, e.g., 'tpv, 'tan-sip'
            % Output : - PV structure (following AstroWCS.DefPVstruct)
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          AH = AstroHeader(Im_name); PV = AstroWCS.build_PV_from_Header(AH,'tpv');
            
             switch lower(ProjType)
                case 'none'
                    PV = AstroWCS.DefPVstruct;
                case 'tan'
                    PV = AstroWCS.DefPVstruct;
                case 'tpv'
                    PV = AstroWCS.build_TPV_from_Header(Header);
                case 'tan-sip'
                    PV = AstroWCS.build_TANSIP_from_Header(Header);
                case 'zpn'
                    error('Need to add ZPN - TODO');
                otherwise
                    error('Unsupported projection type (%s)',ProjType);
             end
        end
        
        function PV = build_TPV_from_Header(Header)
            % Construct a PV (distoration) structure from AstroHeader with TPV projection.
            % Input  : - AstroHeader object.
            % Output : - PV structure (following AstroWCS.DefPVstruct)
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          AH = AstroHeader(Im_name); PV = AstroWCS.build_TPV_from_Header(AH);
            
            PV = AstroWCS.DefPVstruct;
            
            AH = Header;
           
            %PolyTPVtable = AstroWCS.polyTPVdef(); % remove
            PolyTPVstruct = AstroWCS.polyTPVstruct(); % New
            
            FlagMatchPV1 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),'PV1_\d+','match'));
            FlagMatchPV2 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),'PV2_\d+','match'));
            
            NPV1 = sum(FlagMatchPV1);
            NPV2 = sum(FlagMatchPV2);
            
            if NPV1
                PV.KeyNamesX = AH.Data(FlagMatchPV1,1)';
                PV.PolyCoefX = cell2mat(AH.Data(FlagMatchPV1,2))';
                PV.PolyX_Xdeg = zeros(1,NPV1);
                PV.PolyX_Ydeg = zeros(1,NPV1);
                PV.PolyX_Rdeg = zeros(1,NPV1);
                
                for I1 = 1:1:NPV1
                   %currPV = PolyTPVtable(PV.KeyNamesX(I1),:); % remove
                   currPV_ind = find(strcmp(PolyTPVstruct.PolyNames,PV.KeyNamesX(I1))); % New
                   %if ~currPV.Axis==1 % remove
                   if ~PolyTPVstruct.Axis(currPV_ind)==1    % New 
                       error('wrong axis');
                   end
                   %PV.PolyX_Xdeg(I1) = currPV.xi_power; % remove
                   %PV.PolyX_Ydeg(I1) = currPV.eta_power; % remove
                   %PV.PolyX_Rdeg(I1) = currPV.r_power; % remove
                   
                   PV.PolyX_Xdeg(I1) = PolyTPVstruct.xi_power(currPV_ind); % New
                   PV.PolyX_Ydeg(I1) = PolyTPVstruct.eta_power(currPV_ind); % New
                   PV.PolyX_Rdeg(I1) = PolyTPVstruct.r_power(currPV_ind); % New
                   
                end
                
                if all(PV.PolyX_Rdeg==0)
                    PV.PolyX_Rdeg=[];
                end
            end
            
            if NPV2
                PV.KeyNamesY = AH.Data(FlagMatchPV2,1)';
                PV.PolyCoefY = cell2mat(AH.Data(FlagMatchPV2,2))';
                PV.PolyY_Xdeg = zeros(1,NPV2);
                PV.PolyY_Ydeg = zeros(1,NPV2);
                PV.PolyY_Rdeg = zeros(1,NPV2);
                
                for I2 = 1:1:NPV2
                   % currPV = PolyTPVtable(PV.KeyNamesY(I2),:); % remove
                   currPV_ind = find(strcmp(PolyTPVstruct.PolyNames,PV.KeyNamesY(I2))); % New
                   % if ~currPV.Axis==2 % remove
                   if ~PolyTPVstruct.Axis(currPV_ind)==2    % New                 
                       error('wrong axis');
                   end
                   %PV.PolyY_Xdeg(I2) = currPV.xi_power; % remove
                   %PV.PolyY_Ydeg(I2) = currPV.eta_power; % remove
                   %PV.PolyY_Rdeg(I2) = currPV.r_power; % remove
                   
                   PV.PolyY_Xdeg(I2) = PolyTPVstruct.xi_power(currPV_ind); % New
                   PV.PolyY_Ydeg(I2) = PolyTPVstruct.eta_power(currPV_ind); % New
                   PV.PolyY_Rdeg(I2) = PolyTPVstruct.r_power(currPV_ind); % New                   
                   
                end
                
                if all(PV.PolyY_Rdeg==0)
                    PV.PolyY_Rdeg=[];
                end
            end
            
        end
        
        function PolyTPVtable=polyTPVdef()
            % Return a table of TPV polynomial definition
            % Output : - A table of TPV polinomial power (See ColNames and RowNames)
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          PolyTPVtable = AstroWCS.polyTPVdef();

            ColNames = {'Axis' 'Term' 'xi_power' 'eta_power' 'r_power'};
            PolyNames ={'PV1_0' 'PV1_1' 'PV1_2' 'PV1_3' 'PV1_4' 'PV1_5' ...
                        'PV1_6' 'PV1_7' 'PV1_8' 'PV1_9' 'PV1_10' 'PV1_11'...
                        'PV1_12' 'PV1_13' 'PV1_14' 'PV1_15' 'PV1_16'...
                        'PV1_17' 'PV1_18' 'PV1_19' 'PV1_20' 'PV1_21'...
                        'PV1_22' 'PV1_23' 'PV1_24' 'PV1_25' 'PV1_26'...
                        'PV1_27' 'PV1_28' 'PV1_29' 'PV1_30' 'PV1_31'...
                        'PV1_32' 'PV1_33' 'PV1_34' 'PV1_35' 'PV1_36'...
                        'PV1_37' 'PV1_38' 'PV1_39'...
                        'PV2_0' 'PV2_1' 'PV2_2' 'PV2_3' 'PV2_4' 'PV2_5' ...
                        'PV2_6' 'PV2_7' 'PV2_8' 'PV2_9' 'PV2_10' 'PV2_11'...
                        'PV2_12' 'PV2_13' 'PV2_14' 'PV2_15' 'PV2_16'...
                        'PV2_17' 'PV2_18' 'PV2_19' 'PV2_20' 'PV2_21'...
                        'PV2_22' 'PV2_23' 'PV2_24' 'PV2_25' 'PV2_26'...
                        'PV2_27' 'PV2_28' 'PV2_29' 'PV2_30' 'PV2_31'...
                        'PV2_32' 'PV2_33' 'PV2_34' 'PV2_35' 'PV2_36'...
                        'PV2_37' 'PV2_38' 'PV2_39'};
            
            % polynomial mapping: [Axis, Term, xi_power, eta_power, r_power]
            PolyTPV =  [1   0     0   0  0;...
                %
                        1   1     1   0  0;...
                        1   2     0   1  0;...
                        1   3     0   0  1;...
                %
                        1   4     2   0  0;...
                        1   5     1   1  0;...
                        1   6     0   2  0;...
                %
                        1   7     3   0  0;...
                        1   8     2   1  0;...
                        1   9     1   2  0;...
                        1   10    0   3  0;...
                        1   11    0   0  3;...
                %
                        1   12    4   0  0;...
                        1   13    3   1  0;...
                        1   14    2   2  0;...
                        1   15    1   3  0;...
                        1   16    0   4  0;...
                %
                        1   17    5   0  0;...
                        1   18    4   1  0;...
                        1   19    3   2  0;...
                        1   20    2   3  0;...
                        1   21    1   4  0;...
                        1   22    0   5  0;...
                        1   23    0   0  5;...
                %
                        1   24    6   0  0;...
                        1   25    5   1  0;...
                        1   26    4   2  0;...
                        1   27    3   3  0;...
                        1   28    2   4  0;...
                        1   29    1   5  0;...
                        1   30    0   6  0;...
                %
                        1   31    7   0  0;...
                        1   32    6   1  0;...
                        1   33    5   2  0;...
                        1   34    4   3  0;...
                        1   35    3   4  0;...
                        1   36    2   5  0;...
                        1   37    1   6  0;...
                        1   38    0   7  0;...
                        1   39    0   0  7;...
                    
                        2   0     0   0  0;...
                %
                        2   1     0   1  0;...
                        2   2     1   0  0;...
                        2   3     0   0  1;...
                %
                        2   4     0   2  0;...
                        2   5     1   1  0;...
                        2   6     2   0  0;...
                %
                        2   7     0   3  0;...
                        2   8     1   2  0;...
                        2   9     2   1  0;...
                        2   10    3   0  0;...
                        2   11    0   0  3;...
                %
                        2   12    0   4  0;...
                        2   13    1   3  0;...
                        2   14    2   2  0;...
                        2   15    3   1  0;...
                        2   16    4   0  0;...
                %
                        2   17    0   5  0;...
                        2   18    1   4  0;...
                        2   19    2   3  0;...
                        2   20    3   2  0;...
                        2   21    4   1  0;...
                        2   22    5   0  0;...
                        2   23    0   0  5;...
                %
                        2   24    0   6  0;...
                        2   25    1   5  0;...
                        2   26    2   4  0;...
                        2   27    3   3  0;...
                        2   28    4   2  0;...
                        2   29    5   1  0;...
                        2   30    6   0  0;...
                %
                        2   31    0   7  0;...
                        2   32    1   6  0;...
                        2   33    2   5  0;...
                        2   34    3   4  0;...
                        2   35    4   3  0;...
                        2   36    5   2  0;...
                        2   37    6   1  0;...
                        2   38    7   0  0;...
                        2   39    0   0  7];
                    
                 PolyTPVtable = array2table(PolyTPV,'VariableNames',ColNames,'RowNames',PolyNames);
                 
        end
        
        function PolyTPVstruct=polyTPVstruct()
            % Return a struct of TPV polynomial definition
            % Output : - A struct of TPV polinomial power (See Variables below)
            % Author : Yossi Shvartzvald (November 2021)
            % Example:
            %          PolyTPVstruct = AstroWCS.polyTPVstruct();

            %ColNames = {'Axis' 'Term' 'xi_power' 'eta_power' 'r_power'};
            PolyNames ={'PV1_0' 'PV1_1' 'PV1_2' 'PV1_3' 'PV1_4' 'PV1_5' ...
                        'PV1_6' 'PV1_7' 'PV1_8' 'PV1_9' 'PV1_10' 'PV1_11'...
                        'PV1_12' 'PV1_13' 'PV1_14' 'PV1_15' 'PV1_16'...
                        'PV1_17' 'PV1_18' 'PV1_19' 'PV1_20' 'PV1_21'...
                        'PV1_22' 'PV1_23' 'PV1_24' 'PV1_25' 'PV1_26'...
                        'PV1_27' 'PV1_28' 'PV1_29' 'PV1_30' 'PV1_31'...
                        'PV1_32' 'PV1_33' 'PV1_34' 'PV1_35' 'PV1_36'...
                        'PV1_37' 'PV1_38' 'PV1_39'...
                        'PV2_0' 'PV2_1' 'PV2_2' 'PV2_3' 'PV2_4' 'PV2_5' ...
                        'PV2_6' 'PV2_7' 'PV2_8' 'PV2_9' 'PV2_10' 'PV2_11'...
                        'PV2_12' 'PV2_13' 'PV2_14' 'PV2_15' 'PV2_16'...
                        'PV2_17' 'PV2_18' 'PV2_19' 'PV2_20' 'PV2_21'...
                        'PV2_22' 'PV2_23' 'PV2_24' 'PV2_25' 'PV2_26'...
                        'PV2_27' 'PV2_28' 'PV2_29' 'PV2_30' 'PV2_31'...
                        'PV2_32' 'PV2_33' 'PV2_34' 'PV2_35' 'PV2_36'...
                        'PV2_37' 'PV2_38' 'PV2_39'}';
            
            % polynomial mapping: [Axis, Term, xi_power, eta_power, r_power]
            PolyTPV =  [1   0     0   0  0;...
                %
                        1   1     1   0  0;...
                        1   2     0   1  0;...
                        1   3     0   0  1;...
                %
                        1   4     2   0  0;...
                        1   5     1   1  0;...
                        1   6     0   2  0;...
                %
                        1   7     3   0  0;...
                        1   8     2   1  0;...
                        1   9     1   2  0;...
                        1   10    0   3  0;...
                        1   11    0   0  3;...
                %
                        1   12    4   0  0;...
                        1   13    3   1  0;...
                        1   14    2   2  0;...
                        1   15    1   3  0;...
                        1   16    0   4  0;...
                %
                        1   17    5   0  0;...
                        1   18    4   1  0;...
                        1   19    3   2  0;...
                        1   20    2   3  0;...
                        1   21    1   4  0;...
                        1   22    0   5  0;...
                        1   23    0   0  5;...
                %
                        1   24    6   0  0;...
                        1   25    5   1  0;...
                        1   26    4   2  0;...
                        1   27    3   3  0;...
                        1   28    2   4  0;...
                        1   29    1   5  0;...
                        1   30    0   6  0;...
                %
                        1   31    7   0  0;...
                        1   32    6   1  0;...
                        1   33    5   2  0;...
                        1   34    4   3  0;...
                        1   35    3   4  0;...
                        1   36    2   5  0;...
                        1   37    1   6  0;...
                        1   38    0   7  0;...
                        1   39    0   0  7;...
                    
                        2   0     0   0  0;...
                %
                        2   1     0   1  0;...
                        2   2     1   0  0;...
                        2   3     0   0  1;...
                %
                        2   4     0   2  0;...
                        2   5     1   1  0;...
                        2   6     2   0  0;...
                %
                        2   7     0   3  0;...
                        2   8     1   2  0;...
                        2   9     2   1  0;...
                        2   10    3   0  0;...
                        2   11    0   0  3;...
                %
                        2   12    0   4  0;...
                        2   13    1   3  0;...
                        2   14    2   2  0;...
                        2   15    3   1  0;...
                        2   16    4   0  0;...
                %
                        2   17    0   5  0;...
                        2   18    1   4  0;...
                        2   19    2   3  0;...
                        2   20    3   2  0;...
                        2   21    4   1  0;...
                        2   22    5   0  0;...
                        2   23    0   0  5;...
                %
                        2   24    0   6  0;...
                        2   25    1   5  0;...
                        2   26    2   4  0;...
                        2   27    3   3  0;...
                        2   28    4   2  0;...
                        2   29    5   1  0;...
                        2   30    6   0  0;...
                %
                        2   31    0   7  0;...
                        2   32    1   6  0;...
                        2   33    2   5  0;...
                        2   34    3   4  0;...
                        2   35    4   3  0;...
                        2   36    5   2  0;...
                        2   37    6   1  0;...
                        2   38    7   0  0;...
                        2   39    0   0  7];
                 
                 PolyTPVstruct.PolyNames = PolyNames;
                 PolyTPVstruct.Axis = PolyTPV(:,1);
                 PolyTPVstruct.Term = PolyTPV(:,2);
                 PolyTPVstruct.xi_power = PolyTPV(:,3);
                 PolyTPVstruct.eta_power = PolyTPV(:,4);
                 PolyTPVstruct.r_power = PolyTPV(:,5);
                 
        end
        
        function PV = build_TANSIP_from_Header(Header,get_inv)
            % Construct a PV or RevPV structure from AstroHeader with TAN-SIP projection.
            % Input  : - AstroHeader object.
            %          - Flag to constuct PV (false) or RevPV (true).
            %            Default is false.
            % Output : - PV structure (following AstroWCS.DefPVstruct)
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          AH = AstroHeader(Im_name); PV = AstroWCS.build_TANSIP_from_Header(AH);
            
            arguments
                Header
                get_inv        = false;
            end
            
            PV = AstroWCS.DefPVstruct;
            
            AH = Header;
            
            BaseX = 'A';
            BaseY = 'B';
            
            if get_inv
                BaseX = 'AP';
                BaseY = 'BP';
            end
                
            
            FlagMatchPV1 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),[BaseX '_\d+_\d+'],'match'));
            FlagMatchPV2 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),[BaseY '_\d+_\d+'],'match'));
            
            NPV1 = sum(FlagMatchPV1);
            NPV2 = sum(FlagMatchPV2);
            
            if NPV1
                
                PV.KeyNamesX = AH.Data(FlagMatchPV1,1)';
                PV.PolyCoefX = cell2mat(AH.Data(FlagMatchPV1,2))';
                PV.PolyX_Xdeg = zeros(1,NPV1);
                PV.PolyX_Ydeg = zeros(1,NPV1);
                
                PV1_Powers  =regexp(AH.Data(FlagMatchPV1,1), [BaseX '_(?<u_power>\d+)\_(?<v_power>\d+)'],'names');
                for I1 = 1:1:NPV1
                   %PV.PolyX_Xdeg(I1) = str2double(PV1_Powers{I1}.u_power);
                   PV.PolyX_Xdeg(I1) = real(str2doubleq(PV1_Powers{I1}.u_power));
                   %PV.PolyX_Ydeg(I1) = str2double(PV1_Powers{I1}.v_power);
                   PV.PolyX_Ydeg(I1) = real(str2doubleq(PV1_Powers{I1}.v_power));
                end
            end
            
            if NPV2
                PV.KeyNamesY = AH.Data(FlagMatchPV2,1)';
                PV.PolyCoefY = cell2mat(AH.Data(FlagMatchPV2,2))';
                PV.PolyY_Xdeg = zeros(1,NPV2);
                PV.PolyY_Ydeg = zeros(1,NPV2);
                
                PV2_Powers  =regexp(AH.Data(FlagMatchPV2,1), [BaseY '_(?<u_power>\d+)\_(?<v_power>\d+)'],'names');
                
                for I2 = 1:1:NPV2
                   %PV.PolyY_Xdeg(I2) = str2double(PV2_Powers{I2}.u_power);
                   PV.PolyY_Xdeg(I2) = real(str2doubleq(PV2_Powers{I2}.u_power));
                   %PV.PolyY_Ydeg(I2) = str2double(PV2_Powers{I2}.v_power);
                   PV.PolyY_Ydeg(I2) = real(str2doubleq(PV2_Powers{I2}.v_power));
                end
            end
            
       
            
        end
        
        
   %======== Functions to construct AstroWCS from Tran2D =========
   
        function Obj = tran2wcs(Tran2D, Args)
            % Create and populate an AstroWCS object from a Tran2D object
            % Input  : - Tran2D object.
            %          * ...,key,val,...
            %            'NAXIS' (mandatory) - Number of axes
            %            'CRPIX' (mandatory) - Reference pixel
            %            'CRVAL' (mandatory) - World coordinate of reference pixel
            %            'CD'    (mandatory) - Linear projection matrix
            %            'CTYPE' (mandatory) - WCS projection type, e.g., 'RA---TAN', 'RA-TAN-SIP', 'RA---TPV', 'RA---ZPN'
            %            'CUNIT' (mandatory) - Axis unit, e.g., 'deg'
            %            'RADESYS'           - Astrometric system
            %            'EQUINOX'           - EQUINOX
            %            'LONPOLE'           - Native Longitude of the Celestial Pole
            %            'LATPOLE'           - Native Latitude of the Celestial Pole
            %            'WCSAXES'           - WCS dimensionality
            % Output : - AstroWCS object.
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          TC=Tran2D;
            %          TC.symPoly; TC.ParX = ones(1,13);TC.ParY = ones(1,13);
            %          TC.polyCoef;
            %          NAXIS = 2; CRPIX(1,:) = [1.0 1.0]; CRVAL(1,:) = [0.0 0.0];
            %          CD = eye(2); CTYPE(1,:) = {'RA---TPV' 'DEC--TPV'}; CUNIT(1,:) = {'deg' 'deg'};
            %          AW = AstroWCS.tran2wcs(TC,'NAXIS',NAXIS,'CRPIX',CRPIX,'CRVAL',CRVAL,'CD',CD,'CTYPE',CTYPE,'CUNIT',CUNIT);
            
            arguments
                Tran2D
                Args.NAXIS(1,1)
                Args.CRPIX
                Args.CRVAL
                Args.CD
                Args.CTYPE
                Args.CUNIT
                Args.RADESYS   = [];
                Args.EQUINOX   = [];
                Args.LONPOLE   = [];
                Args.LATPOLE   = [];
                Args.WCSAXES   = [];
            end

            % if WCSAXES is not given use NAXIS as default
            if isempty(Args.WCSAXES)
                Args.WCSAXES = Args.NAXIS;
            end

            Obj = AstroWCS(1);
            Obj.Tran2D = Tran2D;   % @Todo: do we need copyObject here?
            
            % Paste number of axes

            Obj.NAXIS = Args.NAXIS;
            Obj.WCSAXES = Args.WCSAXES;

            
            % paste CTYPE and transalte to projection information (ProjType,
            % ProjClass) and CooName and CUNIT
            Obj.CTYPE = Args.CTYPE;
            Obj.CUNIT = Args.CUNIT;
            Obj.read_ctype;
            
            % paste base WCS info
            if ~isempty(Args.RADESYS)
                Obj.RADESYS = Args.RADESYS;
            end
            if ~isempty(Args.EQUINOX)
                Obj.EQUINOX = Args.EQUINOX;
            end
            if ~isempty(Args.LONPOLE)
                Obj.LONPOLE = Args.LONPOLE;
            end
            if ~isempty(Args.LATPOLE)
                Obj.LATPOLE = Args.LATPOLE;
            end
         
            Obj.CRPIX = Args.CRPIX;
            Obj.CRVAL = Args.CRVAL;
                       
            Obj.CD = Args.CD;

            Obj.PV = Obj.build_PV_from_Tran2D(Obj.Tran2D, Obj.ProjType);
            
            %consider adding RevPV for TAN-SIP
            
            % populate proj Meta
            Obj.populate_projMeta;
        end
        
        function PV = build_PV_from_Tran2D(Tran2D,ProjType,set_rev)
            % Construct a PV (distoration) structure from Tran2D object.
            % Input  : - Tran2D object.
            %          - Projection type, e.g., 'tpv, 'tan-sip'
            %          - Set the RevPV distortion names. Option for
            %            TAN-SIP. Default is false;
            % Output : - PV structure (following AstroWCS.DefPVstruct)
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          PV = AstroWCS.build_PV_from_Tran2D(Tran2D, 'TPV');
            
            arguments
                Tran2D
                ProjType
                set_rev        = false;
            end
            
            if isempty(Tran2D.PolyRep.PolyParX) || isempty(Tran2D.PolyRep.PolyX_Xdeg) || isempty(Tran2D.PolyRep.PolyX_Ydeg) || ...
                    isempty(Tran2D.PolyRep.PolyParY) || isempty(Tran2D.PolyRep.PolyY_Xdeg) || isempty(Tran2D.PolyRep.PolyY_Ydeg)
                Tran2D.polyRep;
            end
            
            PV = AstroWCS.DefPVstruct;
            
            PV.PolyCoefX = Tran2D.PolyRep.PolyParX;
            PV.PolyX_Xdeg = Tran2D.PolyRep.PolyX_Xdeg;
            PV.PolyX_Ydeg = Tran2D.PolyRep.PolyX_Ydeg;
            PV.PolyCoefY = Tran2D.PolyRep.PolyParY;
            PV.PolyY_Xdeg = Tran2D.PolyRep.PolyY_Xdeg;
            PV.PolyY_Ydeg = Tran2D.PolyRep.PolyY_Ydeg;

             switch lower(ProjType)
                case 'none'
                    % do nothing
                case 'tan'
                    % do nothing
                case 'tpv'
                    PV = AstroWCS.fill_TPV_KeyNames(PV);
                case 'tan-sip'
                    PV = AstroWCS.fill_TANSIP_KeyNames(PV,set_rev);
                case 'zpn'
                    error('Need to add ZPN - TODO');
                otherwise
                    error('Unsupported projection type (%s)',ProjType);
             end
             
        end
        
        function PV = fill_TPV_KeyNames(PV)
            % Fill TPV keynames in a PV (distoration) structure.
            % Input  : - PV structure (following AstroWCS.DefPVstruct)
            % Output : - PV structure (following AstroWCS.DefPVstruct)
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          PV = AstroWCS.fill_TPV_KeyNames(PV);
            
            %PolyTPVtable = AstroWCS.polyTPVdef(); % remove
            PolyTPVstruct = AstroWCS.polyTPVstruct(); % New            
            
            NPV1 = length(PV.PolyCoefX);
            NPV2 = length(PV.PolyCoefY);
            
            if NPV1
                PV.KeyNamesX = cell(size(PV.PolyCoefX));
                
                for I1 = 1:1:NPV1
                    Axis = 1;
                    xi_power = PV.PolyX_Xdeg(I1);
                    eta_power = PV.PolyX_Ydeg(I1);
                    if isempty(PV.PolyX_Rdeg)
                        r_power = 0;
                    else
                        r_power = PV.PolyX_Rdeg(I1);
                    end
                    %PV.KeyNamesX(I1) = PolyTPVtable.Row(PolyTPVtable.Axis==Axis & PolyTPVtable.xi_power==xi_power & PolyTPVtable.eta_power==eta_power & PolyTPVtable.r_power==r_power); % remove
                    PV.KeyNamesX(I1) = PolyTPVstruct.PolyNames(PolyTPVstruct.Axis==Axis & PolyTPVstruct.xi_power==xi_power & PolyTPVstruct.eta_power==eta_power & PolyTPVstruct.r_power==r_power); % New
                end
            end
            
            if NPV2
                PV.KeyNamesY = cell(size(PV.PolyCoefY));
                
                for I2 = 1:1:NPV2
                    Axis = 2;
                    xi_power = PV.PolyY_Xdeg(I2);
                    eta_power = PV.PolyY_Ydeg(I2);
                    if isempty(PV.PolyY_Rdeg)
                        r_power = 0;
                    else
                        r_power = PV.PolyY_Rdeg(I2);
                    end
                    % PV.KeyNamesY(I2) = PolyTPVtable.Row(PolyTPVtable.Axis==Axis & PolyTPVtable.xi_power==xi_power & PolyTPVtable.eta_power==eta_power & PolyTPVtable.r_power==r_power); % remove
                    PV.KeyNamesY(I2) = PolyTPVstruct.PolyNames(PolyTPVstruct.Axis==Axis & PolyTPVstruct.xi_power==xi_power & PolyTPVstruct.eta_power==eta_power & PolyTPVstruct.r_power==r_power); % New
                end
            end
        
        end
        
        function PV = fill_TANSIP_KeyNames(PV,set_rev)
            % Fill TANSIP keynames in a PV (or RevPV) structure.
            % Input  : - PV structure (following AstroWCS.DefPVstruct)
            %          - Set the RevPV distortion names. Option for
            %            TAN-SIP. Default is false;
            % Output : - PV structure (following AstroWCS.DefPVstruct)
            % Author : Yossi Shvartzvald (August 2021)
            % Example:
            %          PV = AstroWCS.fill_TANSIP_KeyNames(PV);
            arguments
                PV
                set_rev        = false;
            end
            
            BaseX = 'A';
            BaseY = 'B';
            
            if set_rev
                BaseX = 'AP';
                BaseY = 'BP';
            end

            NPV1 = length(PV.PolyCoefX);
            NPV2 = length(PV.PolyCoefY);
            
            if ~isempty(PV.PolyX_Rdeg) || ~isempty(PV.PolyY_Rdeg)
                error('TANSIP do not support poly for R');
            end
            
            if NPV1
                 PV.KeyNamesX = cell(size(PV.PolyCoefX));
                
                for I1 = 1:1:NPV1
                    u_power = PV.PolyX_Xdeg(I1);
                    v_power = PV.PolyX_Ydeg(I1);
                    PV.KeyNamesX{I1} = sprintf('%s_%d_%d',BaseX,u_power, v_power);
                end
                          
            end
            
            if NPV2
                 PV.KeyNamesY = cell(size(PV.PolyCoefY));
                
                for I2 = 1:1:NPV2
                    u_power = PV.PolyY_Xdeg(I2);
                    v_power = PV.PolyY_Ydeg(I2);
                    PV.KeyNamesY{I2} = sprintf('%s_%d_%d',BaseY,u_power, v_power);
                end
                          
            end
            
        end
        
   %======== Functions for related to xy2sky =========
        
        function [Alpha,Delta]=phitheta2alphadelta(Phi,Theta,PhiP,AlphaP,DeltaP,Units)
            % Convert naitive coordinates (Phi,Theta) to celestial coordinates (alpha,delta)
            % Input  : - Native longitude (phi)
            %          - Native latitude (theta)
            %          - native longitude of celestial pole
            %          - Celestial longitude of native pole
            %          - Celestial latitude of native pole (DeltaP=ThetaP)
            %          - Input and output units.
            %            Default is 'deg'
            % Output : - Celestial longitude
            %          - Celestial latitude
            % Author : Yossi Shvartzvald (August 2021)
            % Example: - [Alpha,Delta]=AstroWCS.phitheta2alphadelta(1.1,1.1,0,0,0)

            arguments
                Phi
                Theta
                PhiP
                AlphaP
                DeltaP
                Units        = 'deg';
            end
            
            RAD = 180./pi;

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

        function [Xd,Yd]  = forwardDistortion(PV,X,Y,Args)
            % Apply distortion to X,Y coordinates using the PV sturcture
            % Input  : - PV structure (following AstroWCS.DefPVstruct)
            %          - X coordinate vector
            %          - Y coordinate vector
            %          * ...,key,val,...
            %            'R'           - Radial vector (i.e., sqrt(X.^2+Y.^2))
            %                            May be relevant for TPV/ZPN distorations
            %                            Default is 1.
            %            'plusXY_bool' - Add X,Y to the poliniomial. (e.g. in TAN-SIP)
            %                            Default is false.
            % Output : - Distorted X coordinate vector
            %          - Distorted Y coordinate vector
            % Author : Yossi Shvartzvald (December 2021)
            % Example: [Xd,Yd]  = AstroWCS.forwardDistortion(PV,1,1);

            arguments
                PV
                X
                Y
                Args.R            = 1;
                Args.plusXY_bool  = false;
            end
            
            if ~isempty(PV.PolyCoefX)
                CoefX    = PV.PolyCoefX;
                X_Xpower = PV.PolyX_Xdeg;
                X_Ypower = PV.PolyX_Ydeg;
                if ~isempty(PV.PolyX_Rdeg)
                    X_Rpower = PV.PolyX_Rdeg;
                else
                    X_Rpower = 0;
                end
            else                % if no CoefX, return with no distortion
                X_Xpower = 1;
                X_Ypower = 0;
                X_Rpower = 0;
                if Args.plusXY_bool
                    CoefX = 0;
                else
                    CoefX =1;
                end
            end

            if ~isempty(PV.PolyCoefY)
                CoefY    = PV.PolyCoefY;
                Y_Xpower = PV.PolyY_Xdeg;
                Y_Ypower = PV.PolyY_Ydeg;
                if ~isempty(PV.PolyY_Rdeg)
                    Y_Rpower = PV.PolyY_Rdeg;
                else
                    Y_Rpower = 0;
                end
            else                % if no CoefX, return with no distortion
                Y_Ypower = 1;
                Y_Xpower = 0;
                Y_Rpower = 0;
                if Args.plusXY_bool
                    CoefY = 0;
                else
                    CoefY = 1;
                end
            end


            Xd = sum(CoefX(:) .* ((X(:).').^X_Xpower(:) ) .* ((Y(:).').^X_Ypower(:))  .* ((Args.R(:).').^X_Rpower(:)),1);
            Yd = sum(CoefY(:) .* ((X(:).').^Y_Xpower(:) ) .* ((Y(:).').^Y_Ypower(:))  .* ((Args.R(:).').^Y_Rpower(:)),1);

            Xd=reshape(Xd,size(X));
            Yd=reshape(Yd,size(Y));
            
            if Args.plusXY_bool
                Xd = Xd+X;
                Yd = Yd+Y;
            end

        end
        
        
   %======== Functions for related to sky2xy =========
        
        function [Phi,Theta]=alphadelta2phitheta(Alpha,Delta,PhiP,AlphaP,DeltaP,Units)
            % Convert celestial coordinates to native coordinates
            % Input  : - Celestial longitude.
            %          - Celestial latitude.
            %          - Native longitude of the celestial pole.
            %          - Celestial longitude of the native pole.
            %          - Celestial latitude of the native pole.
            %          - Units of the input and output coordinates
            %            Default is 'deg'.
            % Output : - Native longitude
            %          - Native latitude
            % Author : Yossi Shvartzvald (August 2021)
            % Example: [Phi,Theta]=AstroWCS.alphadelta2phitheta(1.1,1.1,0,0,0);

            arguments
                Alpha
                Delta
                PhiP
                AlphaP
                DeltaP
                Units        = 'deg';
            end
            
            RAD = 180./pi;

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
        
        function [X,Y]  = backwardDistortion(PV,Xd,Yd,Args)
            % Apply reverse (i.e. backward) distortion to X,Y coordinates using the PV sturcture
            % Input  : - PV structure (following AstroWCS.DefPVstruct)
            %          - Distorted X coordinate vector
            %          - Distorted Y coordinate vector
            %          * ...,key,val,...
            %            'plusXY_bool' - Add X,Y to the poliniomial. (e.g. in TAN-SIP)
            %                            Default is false.
            %            'Threshold'   - Convergence thershold.
            %                            Default is 1e-7
            %            'MaxIter'     - Maximum number of itertion.
            %                            Default is 100
            %            'Step'        - Step size. Default is 1e-5.
            % Output : - X coordinate vector
            %          - Y coordinate vector
            % Author : Yossi Shvartzvald (August 2021)
            % Example: [X,Y]  = AstroWCS.backwardDistortion(PV,1,1);
            
            arguments
                PV
                Xd
                Yd
                Args.plusXY_bool  = false;
                Args.Threshold    = 1e-7;
                Args.MaxIter      = 100;
                Args.Step         = 1e-5;
            end
            
            Xf = Xd;
            Yf = Yd;
            
            Xi  = Xf;
            Yi  = Yf;
            
            % The ouput from forward should be the input Xf, Yf
            NotConverged = true;
            Iter = 0;
            while NotConverged
                %
                Iter = Iter + 1;
                
                if isempty(PV.PolyX_Rdeg) && isempty(PV.PolyY_Rdeg)
                    R = 1;
                else
                    R = sqrt(Xi.^2 + Yi.^2); % FFU - change to arbitrary function f(x,y)
                end
                
                [Xi1,Yi1] = AstroWCS.forwardDistortion(PV,Xi,Yi,'R',R,'plusXY_bool',Args.plusXY_bool);
                [Xi2,Yi2] = AstroWCS.forwardDistortion(PV,(Xi + Args.Step),(Yi + Args.Step),'R',R,'plusXY_bool',Args.plusXY_bool);
                
                
                DeltaX = (Xi1 - Xi2);
                DeltaY = (Yi1 - Yi2);
                
                IncX = (Xi1 - Xf)./DeltaX .* Args.Step;
                IncY = (Yi1 - Yf)./DeltaY .* Args.Step;
                Xi = Xi + IncX;
                Yi = Yi + IncY;
                
                [Xi1,Yi1] = AstroWCS.forwardDistortion(PV,Xi,Yi,'R',R,'plusXY_bool',Args.plusXY_bool);
                
                DiffX = Xi1 - Xf;
                DiffY = Yi1 - Yf;
                
                if (max(abs(DiffX)) < Args.Threshold) && (max(abs(DiffY)) < Args.Threshold)
                    NotConverged = false;
                end
                if Iter > Args.MaxIter
                    NotConverged = false;
                    %error('backwardDistortion didnot converge after %d iterations',Iter);
                end
                
            end
            
            X = Xi;
            Y = Yi;
        end
        
       
    end

    %----------------------------------------------------------------------
    methods (Access = protected)
        function NewObj = copyElement(Obj)
            % Custom copy of object properties
            % Called from copy() of matlab.mixin.Copyable decendents
            
            % Make shallow copy of all properties
            NewObj = copyElement@Component(Obj);

            % Deep copy class properties
            NewObj.Tran2D = Obj.Tran2D.copy();
        end
    end
    
    %----------------------------------------------------------------------
    
    methods (Static) % Unit-Test
        Result = unitTest()
            % Unit-Test
                        
    end
    
end

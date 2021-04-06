% A Dictionary class
% 
% Author: Eran Ofek (March 2021)
% Example: D=Dictionary; D.unitTest

classdef Dictionary < Component
    properties (Dependent, SetAccess = private)
        NameParts 
        LastNamePart char         = '';
    end
    properties
        Name char                 = '';   % Dictionary name - e.g., 'HeaderKeySynonyms'
        %Family char               = '';   % Dictionary Family
        Dict(1,1) struct          = struct(); % Primary = {list of alternate names}
        Conversion
    end
   
    methods % constructor
        function Obj=Dictionary(Args)
            % Dictionary constructor
            % Input  : Dictionary file name to load. Default is ''.
            
            arguments
                Args.Name char                 % search by Dictionary name
                Args.Family char               % search bt Dictionary family
                Args.NewDict             = []; 
                Args.NewConversion       = [];
            end
           
            if ~isempty(Args.NewDict)
                % set a new dictionary
                if isempty(Args.Name) || isempty(Args.Family)
                    error('When setting a new Dictionary name and family must be provided');
                end
                Obj.Name       = Args.Name;
                Obj.Family     = Args.Family;
                Obj.Dict       = Args.NewDict;
                Obj.Conversion = Args.NewConversion;
            else
                % search existing dictionary
                
                % stab:
                switch Args.Name
                    case 'Header.Synonyms.KeyNames'
                        
                        St.EXPTIME = {'AEXPTIME', 'EXPTIME','EXPOSURE'};
                        St.IMTYPE  = {'IMTYPE', 'IMGTYPE','IMAGETYP'};
                        St.READNOI = {'READNOI', 'READNOIS','RN'};
                        St.OBSLON  = {'OBSLON', 'LON','GEOLON','GEODLON','LONG'};
                        St.OBSLAT  = {'OBSLAT', 'LAT','GEOLAT','GEODLAT'};
                        St.DATEOBS = {'DATEOBS','DATE-OBS'};
                        Obj.Name   = Args.Name;
                        Obj.Dict   = St;
                        Obj.Conversion = [];
                    case 'Header.Synonyms.KeyVal.IMTYPE'
                        St.Bias    = {'Bias'};
                        St.Dark    = {'Dark'};
                        St.Flat    = {'Flat','twfalt','domeflat'};
                        St.Science = {'Science','sci'};
                        St.Focus   = {'Focus','foc'};
                        St.Arc     = {'Arc'};
                        St.Fringe  = {'Fringe'};
                        Obj.Name   = Args.Name;
                    case 'Header.Comments.Default'
                        % List of default comments for header keywords
                        Obj.Name   = Args.Name;
                        St.NAXIS   = {'Number of dimensions'};
                        St.NAXIS1  = {'Length of axis 1 (X)'};
                        St.NAXIS2  = {'Length of axis 2 (Y)'};
                        St.BITPIX  = {'bits per data value'};
                        St.BZERO   = {'zero point in scaling equation'};
                        St.BSCALE  = {'linear factor in scaling equation'};
                        St.MTYPE   = {'Image type'};
                        St.GAIN    = {'Camera gain [e-/ADU]'};
                        St.INTGAIN = {'Camera internal gain level'};
                        St.READNOI = {'Camera Readout noise [e-]'};
                        St.DARKCUR = {'Dark current [e-/s/pix]'};
                        St.CAMNUM  = {'Camera Number'};
                        Obj.Dict   = St;
                        Obj.Conversion = [];
% CAMLOC  : Camera Location :
% CAMTYPE : Camera Type :
% CAMMODEL: Camera Model :
% CAMNAME : Camera Name :
% MOUNTNUM: Mount Number :
% BINX    : Binning along x-axis :
% BINY    : Binning along y-axis :
% OBSLON  : Observatory WGS84 Geodetic longitude [deg] :
% OBSLAT  : Observatory WGS84 Geodetic latitude [deg] :
% OBSALT  : Observatory WGS84 Geodetic altitude [m] :
% M_JRA   : J2000.0 Right Ascension of mount [deg] : 
% M_JDEC  : J2000.0 Declination of mount [deg] : 
% M_JHA   : J2000.0 Hour Angle of mount [deg] :
% M_RA    : Equinox of date R.A. of mount [deg] :
% M_DEC   : Equinox of date Dec. of mount [deg] :
% M_HA    : Equinox of date H.A. of mount [deg] :
% RA      : J2000.0 Right Ascension [deg] : 
% DEC     : J2000.0 Declination [deg] :
% HA      : J2000.0 Hour Angle [deg] :
% EQUINOX : Equinox in Julian years :
% AIRMASS : Hardie Airmass :
% AZ      : Telescope Azimuth [deg] :
% ALT     : Telescope Altitude [deg] :
% LST     : App. LST at the begining of exposure [deg] :
% JD      : Julian date at the begining of exposure [day] :
% MIDJD   : Julian date at the middle of exposure [day] :
% DATE_OBS: YYYYMMDDTHHMMSS.FFF date :
% FILTER  : Filter name :
% FILTER2 : 2nd Filter name :
% TRK_RA  : Tracking speed R.A. [arcsec/s] :
% TRK_DEC : Tracking speed Dec. [arcsec/s] :
% FOCUS   : Focus position :
% PRVFOCUS: Previous focus position :
% ORIGIN  : Organization name :
% TELESCOP: Telescope : 
% OBSERVER: Observer :
% REFERENC: Bibilographic reference :
% EXPTIME : Exposure time [s] :
% TEMP_DET: Detector temperature [C] :
% COOLPWR : Cooling power :
% TEMP_MNT: Mount temperature [C] :
% TEMP_MIR: Telescope mirror temperature [C] :
% TEMP_OUT: Outside temperature [C] :
% TEMP_HUM: Outside humidity [C] :
% PRESSURE: Barometric pressure [mb] :
            
                        
                        
                    otherwise
                        error('Unknown Dictionary name');
                end
                        
                %
                
                %Obj = Dictionary.read('Name',Args.Name,'Family',Args.Family);
            end
            
        end
    end
    
    methods % setters/getters
        function set.Name(Obj,Val)
            % set Dictionary name and load it
            
            % search the dictionary
            Obj.Name = Val;
            %Obj.Dict = 
            %Obj.Conversion = 
        end
        
        function Result = get.NameParts(Obj)
            % getter for NameParts - split names by '.'
            Result = strsplit(Obj.Name,'.');
        end
        
        function Result = get.LastNamePart(Obj)
            % getter for the last name part in the name
            
            Result = Obj.NameParts{end};
        end
    end
    
    
    methods % basic functions
        function [Alt,AltConv]=searchKey(Obj,Key,Args)
            % Retun alternate names of a specific key in a single dictionary
            % Input  : - Dictionary object with a single element
            %          - A single key name.
            %          * ...,key,val,...
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            % Output : - A cell array of alternate names
            %            If key is not found then return [].
            % Example: Alt=searchKey(Obj,'TYPE')
          
            arguments
                Obj(1,1)
                Key char
                Args.CaseSens(1,1) logical            = true;
                Args.SearchAlgo char    {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp';
            end
            
            FN  = fieldnames(Obj.Dict);
            switch Args.SearchAlgo
                case 'strcmp'
                    if Args.CaseSens
                        Flag = strcmp(FN,Key);
                    else
                        Flag = strcmpi(FN,Key);
                    end
                case 'regexp'
                    if Args.CaseSens
                        Flag = regexp(FN,Key,'match');
                    else
                        Flag = regexp(lower(FN),lower(Key),'match');
                    end
                otherwise
                    error('Unknown SearchAlgo option');
            end
            
            Ind  = find(Flag);
            if numel(Ind)>1
                error('More than one Key was found');
            elseif numel(Ind)==0
                Alt = [];
                AltConv = [];
            else
                Alt = Obj.Dict.(FN{Ind});
                if nargout>1
                    if isfield(Obj.Conversion,FN{Ind})
                        AltConv = Obj.Conversion.(FN{Ind});
                    else
                        AltConv = {};
                    end
                end
                
            end
        end
            
            
            
            
        function [Key,AltConv,AllAlt,FlagKey]=searchAlt(Obj,Alt,Args)
            % Return the key name from an alternate name in a dictionary
            % Input  : - A single element dictionary object.
            %          - A string of name to search in the alternate names.
            %          * ...,key,val,...
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            % Output : - The key name in which the alternate name was
            %            found. Empty if not found.
            % Author: Eran Ofek (March 2021)
            % Example: [a,i]=D.searchAlt('AEXPTIME')
            
            arguments
                Obj(1,1)
                Alt
                Args.CaseSens(1,1) logical            = true;
                Args.SearchAlgo char    {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp';
            end
            
            Idic = 1;
            FN = fieldnames(Obj(Idic).Dict);
            Nfn = numel(FN);
            FlagKey = false(Nfn,1);
            Key = [];
            AltConv = [];
            AllAlt = {};
            for Ifn=1:1:Nfn
                switch Args.SearchAlgo
                    case 'strcmp'
                        if Args.CaseSens
                            Flag = strcmp(Obj(Idic).Dict.(FN{Ifn}),Alt);
                        else
                            Flag = strcmpi(Obj(Idic).Dict.(FN{Ifn}),Alt);
                        end
                    case 'regexp'
                        if Args.CaseSens
                            Flag = regexp(Obj(Idic).Dict.(FN{Ifn}),Alt,'match');
                        else
                            Flag = regexp(lower(Obj(Idic).Dict.(FN{Ifn})),lower(Alt),'match');
                        end

                    otherwise
                        error('Unknown SearchAlgo option');
                end
                
                
                
                FlagKey(Ifn) = any(Flag);
                if any(FlagKey(Ifn))
                    Key = FN{FlagKey};
                    AllAlt = Obj(Idic).Dict.(Key);
                    if isfield(Obj.Conversion,FN{Ifn})
                        AltConv = Obj(Idic).Conversion.(FN{Ifn}){Flag};
                    else
                        AltConv = {};
                    end
                end
            end
        end
        
    end
    
    methods % unitTest
        function unitTest(Obj)
            %
            
            St.EXPTIME = {'AEXPTIME','EXPTIME','EXPOSURE'};
            St.IMTYPE  = {'IMTYPE','TYPE','IMGTYPE','IMAGETYP'};
            Conv.EXPTIME = {@(x) x, @(x) x, @(x) x};
            Obj.Dict = St;
            Obj.Conversion = Conv;
            
            
            [Alt,AltConv] = Obj.searchKey('EXPTIME')
            [Key,AltConv,AllAlt,FlagKey] = Obj.searchAlt('AEXPTIME')
            
        end
   end
        
end
    

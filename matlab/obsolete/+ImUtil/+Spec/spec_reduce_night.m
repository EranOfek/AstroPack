function spec_reduce_night(AllSim,varargin)
%--------------------------------------------------------------------------
% spec_reduce_night function                                        ImSpec
% Description: Reduce a single night or a subset of spectroscopic data,
%              given all the science and calibration images.
% Input  : - List of calibration and science images to use/reduce.
%            The following inputs are possible:
%            (1) Cell array of image names in string format.
%            (2) String containing wild cards (see create_list.m for
%                option). E.g., 'lred00[15-28].fits' or 'lred001*.fits'.
%            (3) Structure array of images (SIM).
%                The image should be stored in the 'Im' field.
%                This may contains also mask image (in the 'Mask' field),
%                and an error image (in the 'ErrIm' field).
%            (4) Cell array of matrices.
%            (5) A file contains a list of image (e.g., '@list').
%            Default is '*.fits'.
%          * Arbitrary number of pairs or arguments: ...,keyword,value,...
%            where keyword are one of the followings:

%            --- Additional parameters
%            Any additional key,val, that are recognized by one of the
%            following programs:
%            images2sim.m
% Output : - Structure of flipped images.
%            Note that header information (e.g., NAXIS1/2) is not
%            modified.
% Tested : Matlab R2011b
%     By : Eran O. Ofek                    Mar 2014
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: 
% Reliable: 
%-----------------------------------------------------------------------------
import Util.array.*

ImageField  = 'Im';
HeaderField = 'Header';
FileField   = 'ImageFileName';
MaskField   = 'Mask';
BackImField = 'BackIm';
ErrImField  = 'ErrIm';


DefV.SpecConfigKeyVal   = {'TURRET',{'APERTURE','LAMPS'}};   % key,val
DefV.SpecConfigTypeKey  = {'DICHROIC','FPA','ANGLE','GRATING','DETNAM'};  %,'APERTURE' - (removed because no ARC) % key
DefV.BiasConfigTypeKey  = 'DETNAM';                % a single key
%DefV.SatLevel           = 60000;
%DefV.TrimSec            = [];   % String or four elements vector
DefV.CompareArcKeyVal   = 'strcmpi';
%DefV.DispDirKey         = 'FPA';
%DefV.DispDirMap         = {'DBSP_BLUE','y','DBSP_RED2','x'};
%DefV.FlipDispDir        = {'y','n'};
%DefV.CollapseRegion     = [501 1000];   % for source selection
DefV.BitMaskDef         = @def_bitmask_specpipeline;
%DefV.Gain               = 'GAIN';
%DefV.RN                 = 'RON';
%DefV.BiasSec            = 'BSEC1';
%DefV.TrimSec            = 'TSEC1';
% CR
DefV.CleanCR            = true;
DefV.BWmorphN           = 1;

DefV.KeyRA              = 'RA';
DefV.KeyDec             = 'Dec';
DefV.Equinox            = 2000.0;
%DefV.KeyPA              = 'CASSPA';
%DefV.KeyObject          = 'OBJECT';
%DefV.KeyAirMass         = 'AIRMASS';
%DefV.KeyExpTime         = 'EXPTIME';

%DefV.DefGoodDispRange   = {[200 2600],[420 3720]};

DefV.ExtSemiW           = 50;
DefV.MaxFracBadPix      = 0.1;
%DefV.WaveCalibType      = {{'arc'}, {'arc','sky'}};  % will perform both sky and arc wave calib % the first one is the primary
%DefV.GoodRangeSpatPos   = {[60 370],[20 330]};

DefV.ThreshBackRMS      = 10;
DefV.TiltMethod         = 'lin';
DefV.InterpMethod       = 'linear';

% inpuit of user interaction information
DefV.AllDataOut         = [];
DefV.AllPeaksOut        = [];
%DefV.GoodRange          = [];

InPar = InArg.populate_keyval(DefV,varargin,mfilename);

Def.AllSim = '*00*.fits';
if (nargin==0)
    AllSim = [];
end

if (isempty(AllSim))
    AllSim   = Def.AllSim;
end

%--- Read all headers ---
AllSimHead     = images2sim(AllSim,'ReadImage',false);

%--- Classify images: Get critical information from headers ---
InfoS          = spec_classify_images(AllSimHead);

% let the user edit the observations
[MatchObs,OutMatchObs] = spec_match_arm(InfoS);
% look for images to use (User selection):
IsUseAll    = [OutMatchObs.C{:,OutMatchObs.Col.UseIm}]==1;
OrigInd     = [OutMatchObs.C{:,OutMatchObs.Col.OrigInd}];
UseInd      = OrigInd(IsUseAll);
GroupIm     = [OutMatchObs.C{:,OutMatchObs.Col.GroupIm}];
StitchGroup = GroupIm(IsUseAll);

InfoS       = InfoS(UseInd);
AllSimHead  = AllSimHead(UseInd);


% Identify all spectroscopic related data
IsConfigKeyVal = is_head_keyval(AllSimHead,InPar.SpecConfigKeyVal{:});
IsSpec         = all(IsConfigKeyVal,2);

% Identify all bias images only be header keyword
IsBiasAll      = is_bias_image(AllSimHead,varargin{:},'CheckImage',false);

% get images size
ImSize = imagesize(AllSimHead,'SizeFromImage',false);

%--- identify the number of spectroscopic configuration to process ---
[IndGroup,Group]=sim_group_keyval(AllSimHead,InPar.SpecConfigTypeKey,'IsSkip',IsBiasAll);
NspecConfig = numel(Group);   

%--------------------------------------------
%--- For each spectroscopic configuration ---
%--------------------------------------------
IndSpec = 0;

for Isc=1:1:NspecConfig,
    ListIndSpecConfig = [];
    
    %--- Identify spectroscopy-related images ---
    %Group(Isc)
    FlagImage = ([IndGroup.Igroup] == Isc).';
    
    %--- Identify bias images taken with the same camera / image size ---
    % camera name of current group
    % taken from first image in group 
    Camera       = cell2mat(sim_getkeyval(AllSimHead(find(FlagImage==1,1)),InPar.BiasConfigTypeKey,varargin{:}));
    IsCamera     = is_head_keyval(AllSimHead,InPar.BiasConfigTypeKey,Camera);   % Flag for camera of current group
    IsBiasCamera = IsBiasAll & IsCamera;
    % flag of images in the connfiguration
    IsConfig     = FlagImage | IsBiasCamera;
    IndConfig    = find(IsConfig);
    
    % get RN and Gain and SatLevel for current camera
    % taken from first image in group
    Gain     = InfoS(find(IsCamera,1)).Gain;
    RN       = InfoS(find(IsCamera,1)).RN;
    SatLevel = InfoS(find(IsCamera,1)).SatLevel;
    
%     if (ischar(InPar.Gain)),
%         % attempt to read Gain from header
%         Gain = cell2mat(sim_getkeyval(AllSimHead(find(FlagImage==1,1)),InPar.Gain,varargin{:}));
%     end
%     if (ischar(InPar.RN)),
%         % attempt to read RN from header
%         RN = cell2mat(sim_getkeyval(AllSimHead(find(FlagImage==1,1)),InPar.RN,varargin{:}));
%     end
%     
    
    %--- Image file names to work on in current configuration ---
    % bias images and science images
    %ListImages = {AllSimHead(IsBiasCamera).ImageFileName, AllSimHead(FlagImage).ImageFileName};
    ListImages = {AllSimHead(IsConfig).ImageFileName};
    
    %--- Read specific images ---
    % Read images of the current configuration
    Sim = images2sim(ListImages,varargin{:});
   
    [IsBias,IsGoodNoise,IsGoodMean,IsBiasKey] = is_bias_image(Sim,varargin{:},'Gain',Gain,'RN',RN);
    [IsFlat,IsNotSaturated]=is_flat_image(Sim,varargin{:},'SatLevel',SatLevel,'Gain',Gain,'RN',RN);
    [IsArc,ImageArcName]=is_arc_image(Sim,varargin{:},'CompareArcKeyVal',InPar.CompareArcKeyVal);
    IsScience = ~IsBias & ~IsFlat & ~IsArc;
    
    if (any(IsScience)),
        % Science image was identified in group
        if (~any(IsArc)),
            fprintf('----------------------------------------------\n');
            fprintf('No arc images for current group\n');
            UserAnswer = innput('Either quit and supply arc images (q) or continue (c)','s');
            switch lower(UserAnswer)
                case 'q'
                    error('User quit program');
                otherwise
                    % do nothing
            end
        end
        
        %------------------------------
        %--- Start group processing ---
        %------------------------------
        % This should be moved to a seperate program
        %TrimSec = get_ccdsec_head(Sim(1).(HeaderField),InPar.TrimSec);
        TrimSec = InfoS(find(IsCamera,1)).TrimSec;
        BiasSec = InfoS(find(IsCamera,1)).BiasSec;
        ArmIndex = InfoS(find(IsCamera,1)).ArmIndex;
        DispAxis = InfoS(find(IsCamera,1)).DispAxis;
        FlipDisp = InfoS(find(IsCamera,1)).FlipDisp;
        WaveCalibType = InfoS(find(IsCamera,1)).WaveCalibType;
        CollapseRange = InfoS(find(IsCamera,1)).CollapseRange;
        DispRange     = InfoS(find(IsCamera,1)).DispRange;
        SpatRange     = InfoS(find(IsCamera,1)).SpatRange;
        
        [Sim,BiasSim,FlatSim,Is]=sim_reduce_set(Sim,varargin{:},'TrimSec',TrimSec,...
                                                                'BiasSec',BiasSec,...
                                                                'MaskSaturated',true,...
                                                                'BitMaskFun',InPar.BitMaskDef,...
                                                                'Gain',Gain,...
                                                                'RN',RN);
        Nim = numel(Sim);
                     
      
        [IsArc,ImageArcName]=is_arc_image(Sim,varargin{:},'CompareArcKeyVal','strcmpi');

        %--- Identify dispersion axis direxction ---
%         Val = sim_getkeyval(Sim(find(IsScience,1)),InPar.DispDirKey,varargin{:})
%         ArmIndex = find(strcmpi(Val{1},InPar.DispDirMap(1:2:end-1)));
%         DispDir  = InPar.DispDirMap{ArmIndex.*2};
%         
        % identify std star spectra
        [IsStd,ImageStdData,NumStd,IsBright]=is_stdstar_image(Sim,varargin{:},'DispDir',DispAxis,'KeyRA',InPar.KeyRA,'KeyDec',InPar.KeyDec,'Equinox',InPar.Equinox);
        IsStd = NumStd==1; % & IsBright;
 
        %--- Rotate images so dispersion axis is along the x-axis ---
        % dispersion along the x-axis
        switch lower(DispAxis)
           case 'x'
              % do nothing
           case 'y'
              Sim = sim_flip(Sim,'Op',@transpose);
           otherwise
               error('Unknown DispDir option');
        end
        
        if (FlipDisp),
            % Left-Right flipping of image
            Sim = sim_flip(Sim,'Op',@fliplr);
        end
        
        
        % get ArcType
        %WaveCalibType    = InPar.WaveCalibType{ArmIndex};
        %GoodRangeSpatPos = InPar.GoodRangeSpatPos{ArmIndex};
        
        
        %--- collapse image in spatial direction ---
        MeanCollapseRegion = mean(CollapseRange);

        Collapse = cell(Nim,1);
        Collapse(IsScience) = spec_collapse_dispaxis(Sim(IsScience),'Range',CollapseRange);

        
        
        %--- Select sources in collapsed images
        if (isempty(InPar.AllPeaksOut) && isempty(InPar.AllDataOut)),
            % Get rough positions for sources in slit
            
            clear AllPeaks
            clear AllData
            AllPeaks = cell(Nim,1);
            AllData  = cell(Nim,1);

            for Iim=1:1:Nim,
                %Iim
                Iimall = IndConfig(Iim);
                
                if (~isempty(Collapse{Iim})),
                    [Peaks,Data]=find_src1d([],Collapse{Iim},...
                                  'Gain',Gain,'RN',RN,...
                                  'GoodRangeSpatPos',SpatRange);
                    AllPeaks{Iim} = Peaks;

                    Data.RA       = InfoS(Iimall).RA;
                    Data.Dec      = InfoS(Iimall).Dec;
                    Data.PA       = InfoS(Iimall).SlitPA;
                    Data.Object   = {InfoS(Iimall).Object};
                    AllData{Iim}  = Data;

                    %plot(AllData{Iim}.X,AllData{Iim}.Y,'k-');
                    %hold on;
                    Np=length(AllPeaks{Iim});

                    for Ip=1:1:Np,
                        %plot(AllPeaks{Iim}(Ip).X,AllPeaks{Iim}(Ip).Y,'kx');

                                AllPeaks{Iim}(Ip).OptimAperRad=7;

                    end
                    %input('next')
                    %clf
                end
            end

            %--- Call GUI for source selection in slit ---
            % pipeline GUI changes .OptimAperRad and .Back
            [AllDataOut,AllPeaksOut] = pipeline_gui(AllData,AllPeaks);
      
            fprintf('Save AllPeaksOut.mat and AllDataOut.mat\n');
            save(sprintf('AllPeaksOut_%03d.mat',Isc),'AllPeaksOut');
            save(sprintf('AllDataOut_%03d.mat',Isc),'AllDataOut');
        else
            % get AllPeaksOut and AllDataOut from user input
            AllPeaksOut = InPar.AllPeaksOut;
            AllDataOut  = InPar.AllDataOut;
        end
        
        
        IndIsStd = find(IsStd);
        Nstd = length(IndIsStd);
        
        %--- define good extraction region along the dispersion direction ---
        if (isempty(InfoS(IndConfig(1)).DispRange)),
        %if (isempty(InPar.GoodRange)),
            ds9_disp(Sim(IndIsStd(1)).Im)
            fprintf('--- Select good extraction region in the dispersion direction ---\n');
            fprintf('ds9_exam.m is active - q to quit\n');
            ds9_exam;

            UserAnswer = input('Type left X-position of good spectral region (Default is 420)','s');
            if isempty(UserAnswer),
                %GoodRange(1) = InPar.DefGoodDispRange{ArmIndex}(1);
                GoodRange(1) = InfoS(IndConfig(1)).DispRange(1);
            else
                GoodRange(1) = str2num_nan(UserAnswer);
            end

            UserAnswer = input('Type right X-position of good spectral region (Default is 3720)','s');
            if isempty(UserAnswer),
                %GoodRange(2) = InPar.DefGoodDispRange{ArmIndex}(2);
                GoodRange(2) = InfoS(IndConfig(1)).DispRange(2);
            else
                GoodRange(2) = str2num_nan(UserAnswer);
            end
        else
            % get GoodRange from user input
            GoodRange = InfoS(IndConfig(1)).DispRange;
        end
        NrangeWC = InfoS(IndConfig(1)).NrangeWC;   % number of wavelength ranges to use in wavecalib


        %--- Trace all the spectra of StD stars ---
        %Sim = rmfield(Sim,'Ntraces');
        %Sim = rmfield(Sim,'SpatCut');
        %Sim = rmfield(Sim,'AllTraces');

        MasterTrace = [];
        
        for Is=1:1:Nstd,
            IndS = IndIsStd(Is);

            % select brightest target
            [~,IndBright] = max([AllPeaksOut{IndS}.Ybs]);

            %AllPeaks{IndS}(IndBright)
            StartPos = [MeanCollapseRegion,AllPeaksOut{IndS}(IndBright).X];
            Trace    = spec_trace(Sim(IndS),StartPos,'Int',false,'GoodRange',GoodRange);

            Sim(IndS).Trace = Trace;

            if (Is==1),
                % define master trace
                MasterTrace = [Sim(IndS).Trace.X, Sim(IndS).Trace.SmY];

            end
        end

        %----------------------------
        %--- Trace Science Target ---
        %----------------------------
        for Is=1:1:Nim,
            if (IsScience(Is)), % && ~IsStd(Is)),
                % go over all peaks
                Npeak = numel(AllPeaksOut{Is});

                CurrStitchGroup = StitchGroup(IndConfig(Is));

                % populate Sim
                Sim(Is).SpatCut = AllDataOut{Is};
                Sim(Is).Ntraces = Npeak;

                for Ipeak=1:1:Npeak,
                    [Is, Ipeak]
                   %AllPeaks{Is}(Ipeak)

                   StartPos = [MeanCollapseRegion,AllPeaksOut{Is}(Ipeak).X];
                   % populate Sim
                   Sim(Is).AllTraces(Ipeak).Peak = AllPeaksOut{Is}(Ipeak);
                   Sim(Is).AllTraces(Ipeak).StartPos = StartPos;

                   % Subtract background
                   BackRegion = AllPeaksOut{Is}(Ipeak).Back-AllPeaksOut{Is}(Ipeak).X+InPar.ExtSemiW;
                   % aperture
                   AperRad    = ceil(AllPeaksOut{Is}(Ipeak).OptimAperRad);   % need to modify!
                   %BackRegion = [10 25; 75 90];

                   IndArc = find(IsArc,1,'first');
                   Info=spec_trace_extract_wave(Sim(Is),StartPos,Sim(IndArc),...
                                                'MasterTrace',MasterTrace,...
                                                'BackRegion',BackRegion,...
                                                'GoodRange',GoodRange,...
                                                'AperRad',AperRad,...
                                                'WaveCalibType',WaveCalibType,...
                                                'WaveCalibSourceArc',ImageArcName{IndArc},...
                                                'Deg',3,...
                                                'Nrange',NrangeWC);

                   if (isempty(Info.ExtractedSpec)),
                       Sim(Is).AllTraces(Ipeak).Info = [];
                   else
                       Sim(Is).AllTraces(Ipeak).Info = Info;
                   end

                   % check trace solution
        FlagAll    = Info.Trace.Flag.dYdX & Info.Trace.Flag.dYdX5 & Info.Trace.Flag.DN & Info.Trace.Flag.GoodBitPix;
        max(Info.Trace.MasterOffset(FlagAll))   % above 5 is bad

                   % build spectrum structure
                   IndSpec = IndSpec + 1;   
                   SpecS(IndSpec).ConfigInd = Isc;
                   SpecS(IndSpec).ArmIndex  = ArmIndex;
                   SpecS(IndSpec).StitchGroup = CurrStitchGroup;
                   SpecS(IndSpec).SimInd    = Is;
                   SpecS(IndSpec).PeakInd   = Ipeak;
                   SpecS(IndSpec).Info      = Sim(Is).AllTraces(Ipeak).Info;
                   SpecS(IndSpec).SpatPos   = Sim(Is).AllTraces(Ipeak).StartPos(2);
                   SpecS(IndSpec).Peak      = Sim(Is).AllTraces(Ipeak).Peak;
                   SpecS(IndSpec).Header    = Sim(Is).Header;
                   SpecS(IndSpec).ImageFileName = InfoS(IndConfig(Is)).ImageFileName;
                   SpecS(IndSpec).Object    = InfoS(IndConfig(Is)).Object;
                   SpecS(IndSpec).RA        = InfoS(IndConfig(Is)).RA;
                   SpecS(IndSpec).Dec       = InfoS(IndConfig(Is)).Dec;
                   SpecS(IndSpec).AM        = InfoS(IndConfig(Is)).AM;
                   SpecS(IndSpec).ExpTime   = InfoS(IndConfig(Is)).ExpTime;
                   ListIndSpecConfig        = [ListIndSpecConfig; IndSpec];
                   
                end
            end
        end

        

        %------------------------
        %--- flux calibration ---
        %------------------------
        WaveType = WaveCalibType{1};
        RangeI   = (GoodRange(1):1:GoodRange(2));

        clear Tran
        Ind = 0;
        for Is=1:1:Nstd
            Ind = Ind + 1;
            IndS = IndIsStd(Is);

            % select brightest target
            [~,IndBright] = max([AllPeaksOut{IndS}.Ybs]);

            Wave    = Sim(IndS).AllTraces(IndBright).Info.FitWave.(WaveType).SpecWave;
            VecDisp = Sim(IndS).AllTraces(IndBright).Info.Trace.X;
            %plot(VecDisp(RangeI), Sim(IndS).AllTraces.Info.FitPSF.H(RangeI))
            %plot(Wave(RangeI), Sim(IndS).AllTraces.Info.FitPSF.H(RangeI))
            %graph(ImageStdData{IndS}.Spec)

            % calculate transmission
            Std        = ImageStdData{IndS};
            StdObsSpec = [Wave(RangeI), Sim(IndS).AllTraces(IndBright).Info.FitPSF.H(RangeI)];

            AM      = InfoS(IndConfig(IndS)).AM;
            ExpTime = InfoS(IndConfig(IndS)).ExpTime;
            
            %AM               = cell2mat(sim_getkeyval(Sim(IndS),InPar.KeyAirMass,'ConvNum',true));
            %ExpTime          = cell2mat(sim_getkeyval(Sim(IndS),InPar.KeyExpTime,'ConvNum',true));

            InPar.Ext        = 'KPNO_atmospheric_extinction.dat';
            InPar.R          = 500;

            Tran(Ind).AM     = AM;
            Tran(Ind).ExpTime= ExpTime;
            Tran(Ind).Orig   = StdObsSpec;
            Tran(Ind).Tran   = spec_response(StdObsSpec,Std,'AM',AM,'ExpTime',ExpTime,'R',InPar.R,'Ext',InPar.Ext); %,varargin{:})
            Tran(Ind).SimInd = IndS;
            Tran(Ind).Std    = Std;

        end

        
        %--- compare between the various transmission curves ---
        for Istd=1:1:Nstd,
           semilogy(Tran(Istd).Tran.VecWave, Tran(Istd).Tran.InvTranS,'k-');
           hold on
        end
        TranBest = Tran(1).Tran;

        %--- Build Telluric templates ---
        VecWave = Tran(Istd).Tran.VecWave;
        AllTelluricTemplate = zeros(numel(VecWave),Nstd);
        for Istd=1:1:Nstd
            % search for continuus regions in Tran(Ind).Tran.FlagTelluric:
            Regions=flag2regions(Tran(Istd).Tran.FlagTelluric);
            TelluricTemplate = spec_telluric_template(Tran(Istd).Tran.VecWave,Tran(Istd).Tran.Ratio,Tran(Istd).Tran.FlagTelluric,'TiltMethod',InPar.TiltMethod);
            plot(Tran(Istd).Tran.VecWave,TelluricTemplate,'k-')

            % equalize sampling
            AllTelluricTemplate(:,Istd) = interp1(Tran(Istd).Tran.VecWave,TelluricTemplate,VecWave,InPar.InterpMethod);
        end
        MeanTelluricTemplate = AllTelluricTemplate(:,1); %median(AllTelluricTemplate,2);
        FlagTelluric         = Tran(1).Tran.FlagTelluric;
        TelluricAM           = Tran(1).AM;
        plot(Tran(Istd).Tran.VecWave,MeanTelluricTemplate,'k-')

        
        %--- go over spectra and fit/remove Telluric and applay response ---
        Nspec = numel(SpecS);
        for Ispecconfig=1:1:numel(ListIndSpecConfig)
            Ispec = ListIndSpecConfig(Ispecconfig);
                
            Isim  = SpecS(Ispec).SimInd;
            Ipeak = SpecS(Ispec).PeakInd;
            if (~isempty(SpecS(Ispec).Info)),
                
                ObjectSpec = [SpecS(Ispec).Info.FitWave.(WaveType).SpecWave,SpecS(Ispec).Info.FitPSF.H];
                AM = InfoS(IndConfig(Isim)).AM;
                
                %AM = cell_fitshead_getkey(Sim(Is).Header,'AIRMASS','NaN');
                %AM = str2num_nan(AM{2});
                        
                [Res,CorrTObsSpec] = spec_fit_telluric(ObjectSpec,[VecWave, MeanTelluricTemplate],...
                                                       'TelluricFlag',double(FlagTelluric),...
                                                       'FitTelluric','LogAM',...
                                                       'AM',AM,'TelluricAM',TelluricAM);
                %plot(Res.Min.Factor,Res.Min.RMS)

                [CorrObsSpec,InvTran] = spec_corr_response(CorrTObsSpec,TranBest,'AM',AM);


                % store corrected spectrum
                Sim(Isim).AllTraces(Ipeak).Info.CorrObsSpec = CorrObsSpec;
                SpecS(Ispec).Info.CorrObsSpec = CorrObsSpec;
                
                        
               % graph(CorrObsSpec)

                %input('next','s')
            end
        end
        
        
    else
        % No science images identified
        % Write report about a group without science images
    end
end

save All.mat


%-----------------------------------------------
%--- Organize spectra in stitch/coadd groups ---
%-----------------------------------------------
UniqueStitchGroup = unique([SpecS.StitchGroup]);
Nsg               = numel(UniqueStitchGroup);

IndSpec = 0;
for Isg=1:1:Nsg,
    if (UniqueStitchGroup(Isg)>0),
        IndSpec = IndSpec + 1;
        % select images that belongs to the same stitch group
        IndSG   = find([SpecS.StitchGroup]==UniqueStitchGroup(Isg));
        Nimsg   = numel(IndSG);
        
        IndTrace = 1;  % NOT USED YET!!!
        
        Iimsg = 0;
        for IloopIm=1:1:Nimsg,
            % for each image in the StitchGroup
            IndIm     = IndSG(IloopIm);
            if (isempty(SpecS(IndIm).Info)),
                % do nothing
            else
                Iimsg = Iimsg + 1;

                % for each image in the StitchGroup
                            
                ArmIndex = SpecS(IndIm).ArmIndex;
            
                TmpS = SpecS(IndIm);   % no multiple traces YET!!!
           
                FN = fieldnames(TmpS.Info.FitWave);
                for Iwc=1:1:length(FN),
                    if (Iwc==1),
                        % default wavecalib
                        SpecStruct(IndSpec).PrimWCName  = FN{Iwc};
                        if (~isempty(TmpS.Info.FitWave.(FN{Iwc}))),
                            
                            SpecStruct(IndSpec).IS(Iimsg).PrimWC        = TmpS.Info.FitWave.(FN{Iwc}).SpecWave;
                            SpecStruct(IndSpec).IS(Iimsg).PrimWC_RMS    = TmpS.Info.FitWave.(FN{Iwc}).RMS;
                            SpecStruct(IndSpec).IS(Iimsg).PrimWC_Nlines = TmpS.Info.FitWave.(FN{Iwc}).Ndata;
                            SpecStruct(IndSpec).IS(Iimsg).PrimWC_Npar   = TmpS.Info.FitWave.(FN{Iwc}).Npar;
                            SpecStruct(IndSpec).IS(Iimsg).PrimWC_List   = TmpS.Info.MatchedList.(FN{Iwc});
                                         
                            SpecStruct(IndSpec).IS(Iimsg).ArcTemplate   = TmpS.Info.FitWave.(FN{Iwc}).ArcTemplate;
                            SpecStruct(IndSpec).IS(Iimsg).CalibLamp     = [TmpS.Info.FitWave.(FN{Iwc}).SpecPix, TmpS.Info.FitWave.(FN{Iwc}).SpecWave, TmpS.Info.FitWave.(FN{Iwc}).SpecInt];
                            
                            
                        else
%                             SpecStruct(IndSpec).IS(Iimsg).PrimWC        = [];
%                             SpecStruct(IndSpec).IS(Iimsg).PrimWC_RMS    = [];
%                             SpecStruct(IndSpec).IS(Iimsg).PrimWC_Nlines = [];
%                             SpecStruct(IndSpec).IS(Iimsg).PrimWC_Npar   = [];
%                             SpecStruct(IndSpec).IS(Iimsg).PrimWC_List   = [];
                        end
                     else
                        % secondary wavecalib
                        if (~isempty(TmpS.Info.FitWave.(FN{Iwc}))),
                            SpecStruct(IndSpec).IS(Iimsg).SecWC        = TmpS.Info.FitWave.(FN{Iwc}).SpecWave;
                            SpecStruct(IndSpec).IS(Iimsg).SecWC_RMS    = TmpS.Info.FitWave.(FN{Iwc}).RMS;
                            SpecStruct(IndSpec).IS(Iimsg).SecWC_Nlines = TmpS.Info.FitWave.(FN{Iwc}).Ndata;
                            SpecStruct(IndSpec).IS(Iimsg).SecWC_Npar   = TmpS.Info.FitWave.(FN{Iwc}).Npar;
                            SpecStruct(IndSpec).IS(Iimsg).SecWC_List   = TmpS.Info.MatchedList.(FN{Iwc});
                            
                            SpecStruct(IndSpec).IS(Iimsg).ArcTemplate   = TmpS.Info.FitWave.(FN{Iwc}).ArcTemplate;
                            SpecStruct(IndSpec).IS(Iimsg).CalibLamp     = [TmpS.Info.FitWave.(FN{Iwc}).SpecPix, TmpS.Info.FitWave.(FN{Iwc}).SpecWave, TmpS.Info.FitWave.(FN{Iwc}).SpecInt];
                            
                        else
%                             SpecStruct(IndSpec).IS(Iimsg).SecWC        = [];
%                             SpecStruct(IndSpec).IS(Iimsg).SecWC_RMS    = [];
%                             SpecStruct(IndSpec).IS(Iimsg).SecWC_Nlines = [];
%                             SpecStruct(IndSpec).IS(Iimsg).SecWC_Npar   = [];
%                             SpecStruct(IndSpec).IS(Iimsg).SecWC_List   = [];
                        end
                     end
                end
            
                SpecStruct(IndSpec).IS(Iimsg).ImageFileName      = TmpS.ImageFileName;
                SpecStruct(IndSpec).IS(Iimsg).Object             = TmpS.Object;
                SpecStruct(IndSpec).IS(Iimsg).ArmIndex           = ArmIndex;
                SpecStruct(IndSpec).IS(Iimsg).SpatialPos         = TmpS.Info.StartPos(2);
                SpecStruct(IndSpec).IS(Iimsg).TraceX             = TmpS.Info.Trace.X;
                SpecStruct(IndSpec).IS(Iimsg).TraceY             = TmpS.Info.Trace.Y;
                SpecStruct(IndSpec).IS(Iimsg).TraceSmY           = TmpS.Info.Trace.SmY;
                SpecStruct(IndSpec).IS(Iimsg).Trace_RMS          = TmpS.Info.Trace.Poly.RMS;
                SpecStruct(IndSpec).IS(Iimsg).ExtractedSpec_Im   = TmpS.Info.ExtractedSpec.Im;
                SpecStruct(IndSpec).IS(Iimsg).ExtractedSpec_Mask = TmpS.Info.ExtractedSpec.Mask;
                SpecStruct(IndSpec).IS(Iimsg).ExtractedSpec_BackS= TmpS.Info.BackSubSpec.Im;
                SpecStruct(IndSpec).IS(Iimsg).BackRMS            = TmpS.Info.BackInfo.RMS;
                SpecStruct(IndSpec).IS(Iimsg).FitH               = TmpS.Info.FitPSF.H;
                SpecStruct(IndSpec).IS(Iimsg).FitHrms            = TmpS.Info.FitPSF.RMS;
                SpecStruct(IndSpec).IS(Iimsg).Aper               = TmpS.Info.FitPSF.AperSum;
                SpecStruct(IndSpec).IS(Iimsg).AperErr            = TmpS.Info.FitPSF.AperErr;
                SpecStruct(IndSpec).IS(Iimsg).Mask               = TmpS.Info.FitPSF.Mask;
                SpecStruct(IndSpec).IS(Iimsg).Flux               = TmpS.Info.CorrObsSpec(:,2);
                SpecStruct(IndSpec).IS(Iimsg).FluxErr            = SpecStruct(IndSpec).IS(Iimsg).Flux.*SpecStruct(IndSpec).IS(Iimsg).AperErr./SpecStruct(IndSpec).IS(Iimsg).Aper;
                
            end
        end
    end
end



%--------------------------------
%--- Coadd and stitch spectra ---
%--------------------------------
Ngr = numel(SpecStruct);
% for each coadd/stitch group
for Igr=1:1:Ngr,
    NumInGroup = numel(SpecStruct(Igr).IS);
    % select by ArmIndex
    if (~isempty(SpecStruct(Igr).PrimWCName)),
        UniqueArmIndex = unique([SpecStruct(Igr).IS.ArmIndex]);
        Nai            = numel(UniqueArmIndex);
        % Coadd images in each ArmIndex
        for Iai=1:1:Nai,
            IndAI = find(UniqueArmIndex(Iai)==[SpecStruct(Igr).IS.ArmIndex]);
            % coadd all spectra in: SpecStruct(Igr).IS(IndAI)
            TmpArm = SpecStruct(Igr).IS(IndAI);
            if (~isempty(TmpArm)),
                %~isempty(TmpArm.PrimWC)),
                Out = spec_coadd1d(TmpArm,'WaveField','PrimWC');
                SpecStruct(Igr).Arm(Iai).Arm       = UniqueArmIndex(Iai);
                SpecStruct(Igr).Arm(Iai).Nspec     = numel(IndAI);
                SpecStruct(Igr).Arm(Iai).Wave      = Out.Wave;
                SpecStruct(Igr).Arm(Iai).Flux      = Out.Flux;
                SpecStruct(Igr).Arm(Iai).ErrW      = Out.ErrW;
                SpecStruct(Igr).Arm(Iai).ErrStd    = Out.ErrStd;
                SpecStruct(Igr).Arm(Iai).ErrStdN   = Out.ErrStdN;
                SpecStruct(Igr).Arm(Iai).Mask      = Out.Mask;
            end
        end
        % stitch spectra
        % need to add background...
        if (numel(SpecStruct(Igr).Arm)==2),
            AI = 1;
            Spec1 = [SpecStruct(Igr).Arm(AI).Wave, SpecStruct(Igr).Arm(AI).Flux, SpecStruct(Igr).Arm(AI).ErrW];
            AI = 2;
            Spec2 = [SpecStruct(Igr).Arm(AI).Wave, SpecStruct(Igr).Arm(AI).Flux, SpecStruct(Igr).Arm(AI).ErrW];

            Spec1(isnan(Spec1(:,2)),:) = [];
            Spec2(isnan(Spec2(:,2)),:) = [];

            [ListM]=spec_stitch(Spec1,Spec2,'BM1',SpecStruct(Igr).Arm(1).Mask,...
                                               'BM2',SpecStruct(Igr).Arm(2).Mask,...
                                               'Algo','lsq',...
                                               'Merge','mean');

            SpecStruct(Igr).Spec = ListM;
        end
    end
end

        
I=1;plot(SpecStruct(I).Arm(1).Wave, SpecStruct(I).Arm(1).Flux,'b-');hold on; plot(SpecStruct(I).Arm(2).Wave, SpecStruct(I).Arm(2).Flux,'r-');

graph(SpecStruct(I).Spec) 
    

% TODO: check wavecalib: e.g.
graph(SpecStruct(1).IS(1).ArcTemplate)
hold on;
plot(SpecStruct(1).IS(1).CalibLamp(:,2),SpecStruct(1).IS(1).CalibLamp(:,3),'r-')





function Result = findMeasureSources(Obj, Args)
    % Basic sources finder and measurments on AstroImage object.
    %   This function uses the +imUtil.sources.find_measure_sources function.
    % Input  : - An AstroImage object (multi elements are supported).
    %            'RemoveBadSources' - A logical indicating if to remove
    %                   bad sources using imProc.sources.cleanSources.
    %                   This will work only if the following columns are requested
    %                   'SN_1','SN_2','FLUX_CONV_2','FLUX_CONV_3','STD_ANNULUS'.
    %                   Default is false.
    %            'ReFind' - A logical indicating if to find stars if the
    %                   catalog is already populated. Default is true.
    %            'Threshold' - Detection threshold above background in units of
    %                   the background std. 
    %                   Default is 5.
    %            'Psf' - A PSF stamp or a cube of PSFs. If a cube then the PSF
    %                   index is the third dimesnion.
    %                   The input image will be filtered with each PSF and 
    %                   local maxima will be searched in all the filtered
    %                   images.
    %                   If provided, this parameter overrid the PsfFun input
    %                   argument.
    %                   Default is [].
    %            'PsfFun' - A function handle to generate PSF or a cube of
    %                   PSFs.
    %                   Default is @imUtil.kernel2.gauss.
    %            'PsfFunPar' - A cell array of parameters to pass to the PsfFun
    %                   function.
    %                   Default is {[0.1;1.5;3]} (i.e., will generate a cuve of
    %                   templates with Gaussian PSF with sigmas of 0.1, 1.5 and
    %                   3 pixels).
    %            'RemoveEdgeDist' - Indicating if to remove sources which
    %                   XPEAK/YPEAK are near the image edges.
    %                   If NaN, then no sources are removed. Otherwise, this is
    %                   the distance in pixels from the edge to remove the
    %                   sources. For example, 0 will remove sources located
    %                   exactly at the edge pixels.
    %                   Default is 0.
    %            'ForcedList' - An [X,Y] coordinates on which to perform forced
    %                   photometry measurments.
    %                   Forced photometry requestes have TEMP_ID=NaN.
    %                   Rounded coordinates must be within image boundries.
    %                   Default is [].
    %            'OnlyForced' - A logical flag indicating if to run only forced
    %                   photometry.
    %                   Default is false.
    %            'BackPar' - A cell array of additional parameters to pass to
    %                   the imProc.image.background function.
    %                   Default is {}.
    %            'ReCalcBack' - A logical indicating if to recalculate
    %                   background, even if it is already exist in the
    %                   AstroImage. Default is false.
    %            'MomPar' - A cell array of additional parameters to pass to
    %                   the imUtil.image.moment2 function.
    %                   Default is {}.
    %            'Conn' - Connectivity parameter for local maxima
    %                   identification.
    %                   Default is 8.
    %            'Gain' - Default is 1.
    %            'LupSoftPar' - Luptitude softening parameter. Default is 1e-10.
    %            'ZP' - ZP for magnitude. Default is 25.
    %            'ColCell' - A cell array of column names to generate in the
    %                   output.
    %                   Default is
    %                   {'XPEAK','YPEAK','TEMP_ID','SN','FLUX_CONV','BACK_IM','VAR_IM',...           
    %                                            'X', 'Y',...
    %                                            'X2','Y2','XY',...
    %                                            'FLUX_APER', 'APER_AREA', 'FLUX_BOX','BACK_ANNULUS', 'STD_ANNULUS', ...
    %                                            'FLUXERR_APER',...
    %                                            'MAG_APER', 'MAGERR_APER', 'BACKMAG_ANNULUS',...
    %                                            'MAG_CONV'};
    %            'AddFlags' - A logical indicating if to add a columns of
    %                   flags to the catalog. The flags are generated from
    %                   the mask image. An or operator is used for all the
    %                   pixels within a 'FlagRadius' pixels from the source
    %                   position specified by the 'ColNamesX' and
    %                   'ColNamesY' arguments. Default is true.
    %            'FlagHalfSize' - Half size [pix] of coutout around the source X/Y position
    %                   from which to extract all the Mask values for the
    %                   source Flag. The cutout size is twice the half size plus 1.
    %                   Default is 3.
    %            'FlasgPos' - The column index in which to add the Flags
    %                   column. Default is Inf.
    %            'ColNameFlags' - The column name of Flags to add to the
    %                   catalog. 
    %                   This will be added only of the MaskData is
    %                   populated. Default is 'FLAGS'.
    %            'ColNamesX' - X column names dictionary from which to get
    %                   the X position for the flags retrival.
    %                   Default is AstroCatalog.DefNamesX.
    %            'ColNamesY' - Y column names dictionary from which to get
    %                   the Y position for the flags retrival.
    %                   Default is AstroCatalog.DefNamesY.
    %            'ColNamesXsec' - If ColNamesX column returns NaN replace
    %                   it with valid value from this column name.
    %                   Default is 'XPEAK'.
    %            'ColNamesYsec' - Like 'ColNamesXsec', by for Y.
    %                   Default is 'YPEAK'.
    %            'FlagsType' - A function handle of the class to which to
    %                   convert the Flags into. Default is @double.
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false). Default is false.
    % Output : - An AstroImage object in which the CatData is populated
    %            with sources found in the image.
    % Example: Im=imUtil.kernel2.gauss(2,[128 128]);
    %          Im=Im.*1000 +randn(size(Im));        
    %          AI = AstroImage({Im});
    %          Result = imProc.sources.findMeasureSources(AI);
   
    arguments
        Obj
        Args.RemoveBadSources logical      = false;
        Args.ReFind(1,1) logical           = true;
        Args.Threshold                     = 5;
        Args.Psf                           = [];
        Args.PsfFun function_handle        = @imUtil.kernel2.gauss;
        Args.PsfFunPar cell                = {[0.1; 1.0; 1.5; 2.6; 5]};
        Args.RemoveEdgeDist                = 0;  % NaN for non removal
        Args.ForcedList                    = [];
        Args.OnlyForced(1,1) logical       = false;
        
        Args.Gain                          = 1;      % only for errors calculation
        Args.LupSoftPar                    = 1e-10;
        Args.ZP                            = 25;
        
        Args.ReCalcBack logical            = false;
        Args.BackPar cell                  = {};
        
        Args.MomPar cell                   = {};
        Args.Conn                          = 8;
        Args.ColCell cell                  = {'XPEAK','YPEAK','TEMP_ID','SN','FLUX_CONV','BACK_IM','VAR_IM',...           
                                                'X', 'Y',...
                                                'X2','Y2','XY',...
                                                'FLUX_APER', 'APER_AREA','BACK_ANNULUS', 'STD_ANNULUS', ...
                                                'FLUXERR_APER',...
                                                'MAG_APER', 'MAGERR_APER', 'BACKMAG_ANNULUS',...
                                                'MAG_CONV', 'MAGERR_CONV'};
        
        % Flags
        Args.AddFlags logical              = true;
        Args.FlagHalfSize                  = 3;
        Args.FlasgPos                      = Inf;
        Args.ColNameFlags                  = 'FLAGS';
        Args.ColNamesX                     = AstroCatalog.DefNamesX;
        Args.ColNamesY                     = AstroCatalog.DefNamesY;
        Args.ColNamesXsec                  = 'XPEAK';
        Args.ColNamesYsec                  = 'YPEAK';
        Args.FlagsType                     = @double;
            
        Args.CreateNewObj logical          = false;
        
        % hidden
        Args.ImageProp char            = 'ImageData';
        Args.ImagePropIn char          = 'Image';
        Args.BackProp char             = 'BackData';
        Args.BackPropIn char           = 'Image';
        Args.VarProp char              = 'VarData';
        Args.VarPropIn char            = 'Image';
        Args.CatProp char              = 'CatData';
    end
    
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end
    
    % calculate background
    imProc.background.background(Result, 'CreateNewObj',false, 'ReCalcBack', Args.ReCalcBack, Args.BackPar{:});
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % call the source finder and measurments
        if Args.ReFind || ~isempty(Result(Iobj).(Args.CatProp).Catalog)
            Result(Iobj).(Args.CatProp) = imUtil.sources.find_measure_sources(Result(Iobj).(Args.ImageProp).(Args.ImagePropIn), ...
                                                        'OutType','AstroCatalog',...
                                                        'BackIm',Result(Iobj).(Args.BackProp).(Args.BackPropIn),...
                                                        'VarIm',Result(Iobj).(Args.VarProp).(Args.VarPropIn),...
                                                        'Threshold',Args.Threshold,...
                                                        'Psf',Args.Psf,...
                                                        'PsfFun',Args.PsfFun,...
                                                        'PsfFunPar',Args.PsfFunPar,...
                                                        'RemoveEdgeDist',Args.RemoveEdgeDist,...
                                                        'ForcedList',Args.ForcedList,...
                                                        'OnlyForced',Args.OnlyForced,...
                                                        'MomPar',Args.MomPar,...
                                                        'Conn',Args.Conn,...
                                                        'Gain',Args.Gain,...
                                                        'LupSoftPar',Args.LupSoftPar,...
                                                        'ZP',Args.ZP,...
                                                        'ColCell',Args.ColCell);
                                                    
                                                    
%             [Result(Iobj).(Args.CatProp).Catalog, Result(Iobj).(Args.CatProp).ColNames] = imUtil.sources.find_measure_sources(Result(Iobj).(Args.ImageProp).(Args.ImagePropIn), ...
%                                                         'OutType','mat',...
%                                                         'BackIm',Result(Iobj).(Args.BackProp).(Args.BackPropIn),...
%                                                         'VarIm',Result(Iobj).(Args.VarProp).(Args.VarPropIn),...
%                                                         'Threshold',Args.Threshold,...
%                                                         'Psf',Args.Psf,...
%                                                         'PsfFun',Args.PsfFun,...
%                                                         'PsfFunPar',Args.PsfFunPar,...
%                                                         'RemoveEdgeDist',Args.RemoveEdgeDist,...
%                                                         'ForcedList',Args.ForcedList,...
%                                                         'OnlyForced',Args.OnlyForced,...
%                                                         'MomPar',Args.MomPar,...
%                                                         'Conn',Args.Conn,...
%                                                         'Gain',Args.Gain,...
%                                                         'LupSoftPar',Args.LupSoftPar,...
%                                                         'ZP',Args.ZP,...
%                                                         'ColCell',Args.ColCell);
                                                    
                   
            % remove bad sources
            % works only for Gaussian PSF
            if Args.RemoveBadSources
                [Result(Iobj)] = imProc.sources.cleanSources(Result(Iobj), 'SigmaPSF',Args.PsfFunPar{1}(2:3),...
                                                                           'ColNamsSN',{'SN_1','SN_2'},...
                                                                           'RemoveBadSources',Args.RemoveBadSources,...
                                                                           'CreateNewObj',false);
            end
            
            
            % populate Flags from the Mask image
            if Args.AddFlags
                XY                   = getXY(Result(Iobj).CatData, 'ColX',Args.ColNamesX, 'ColY',Args.ColNamesY); 
                % Replace NaN with valid X/Y position
                XYpeak               = getXY(Result(Iobj).CatData, 'ColX',Args.ColNamesXsec, 'ColY',Args.ColNamesYsec); 
                Fnan                 = isnan(XY(:,1));
                XY(Fnan,:)           = XYpeak(Fnan,:);
                
                % need to decide what to do about NaN positions
                if ~isemptyImage(Result(Iobj).MaskData)
                    Flags                = bitwise_cutouts(Result(Iobj).MaskData, XY, 'or', 'HalfSize',Args.FlagHalfSize);
                    Flags                = Args.FlagsType(Flags);
                    Result(Iobj).CatData = insertCol(Result(Iobj).CatData, Flags, Args.FlasgPos, Args.ColNameFlags, {''});
                end
            end                
        end
                                                    
                                                        
                                                        
    end
    
end
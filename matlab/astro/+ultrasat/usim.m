function [usimImage, AP, ImageSrcNoiseADU] =  usim ( Args ) 
    % Make a simulated ULTRASAT image from a source catalog
    % Package: ultrasat
    % Description: Make a simulated ULTRASAT image from a source catalog
    % Input: -    
    %       * ...,key,val,... 
    %       'Cat'       - a catalog of simulated sources or the number of sources to generate randomly
    %       'SkyCat'    - the flag determines whether the input coordinates are RA, Dec
    %       'PlaneRotation - [deg] rotation of the plane w.r.t. the north celestial pole
    %       'Mag'       - a vector of source magnitudes or 1 magnitude for all the sources
    %       'Ebv'       - a vector of E(B-V) for each of the sources or 1 value for the whole observed field 
    %       'ExtLaw'    - the extinction law used to redden the point and extended sources,
    %                     as 'Model' of astro.extinction.extinction: 'CCM89' (default) or 'F99'
    %       'Rv'        - R_V of the extinction law. Default is 3.08. The F99 law is coded
    %                     for R_V = 3.1 only, so 'F99' needs 'Rv',3.1
    %       'FiltFam'   - the filter family for which the source magnitudes are defined;
    %                     one family for all the sources, or one per source. 'ULTRASAT'
    %                     (any case) may not be mixed with other families in one call
    %       'Filt'      - the filter[s] for which the source magnitudes are defined.
    %                     Ignored for the 'ULTRASAT' family: the magnitude of a source is
    %                     always defined in the filter of the radial zone where it lies
    %       'CalculateULTRASATMag' - if the input magnitudes are not of the ULTRASAT filters, calculate the ULTRASAT magnitudes at output 
    %       'CalculateCrudeSNR' - estimate SNR for the input sources
    %       'SNRMethod' - how CrudeSNR is computed: 'aperture' (default), 'optimal',
    %                     'shot' or 'legacy'. See the Args.SNRMethod comment below.
    %       'MeasurePSF' - if true, measure the width of every point source's final
    %                     (rotated, jittered) PSF and attach a table of them to the output
    %                     as usimImage.UserData.PSFWidths. Default is false.
    %       'SNROnly'   - if true, return right after CrudeSNR is computed, skipping the
    %                     noise/ADU pipeline and file writing (usimImage.Image is [])
    %       'SpecType'  - model of the input spectra ('BB','PL','Pickles') or 'tab'
    %       'Spec'      - parameters of the input spectra (temperature, spectral index) or a table of spectral intensities
    %       'Exposure'  - image exposure
    %       'Tile'      - name of the ULTRASAT tile ('A','B','C','D')
    %       'ImRes'     - image resolution in 1/pix units (allowed values: 1, 2, 5, 10, 47.5)
    %       'RotAng'    - tile rotation angle[s] relative to the axis of the raw PSF database:
    %                     one angle for all the sources, or one per source (per object in
    %                     the extended-object mode), as a row or a column
    %       'Jitter'    - include the PSF blurring due to the S/C jitter. Default is false.
    %       'JitterPar' - [SigmaX0 SigmaY0 Rotation] jitter amplitudes [arcsec] for a 300 s
    %                     exposure; the blur scales as sqrt(Exposure(2)/300), and the
    %                     rotation term grows with the distance from the telescope axis
    %                     (the inner corner of the tile). Default is [2 2 10].
    %       'RA0'       - the RA,  deg of the camera aimpoint 
    %       'Dec0'      - the Dec, deg of the camera aimpoint 
    %       'WCSFile'   - if not empty, the image WCS is read from this file
    %       'ArraySizeLimit' - the maximal array size, machine-dependent, determines the method in specWeight
    %       'MaxNumSrc'      - the maximal size of a source chunk to be worked over at a time
    %       'MaxPSFNum'      - the maximal number of recorded PSFs
    %       'Verbosity'      - 0: silent (default); 1: report the progress and timing on screen
    %       'NoisePoisson'   - add random noise (1/0): the photon/dark-current shot noise and the
    %                          read-out + cross-talk noise. With 0 the image holds the expected
    %                          counts. The Var layer holds the expected variance either way.
    %                          Default is true.
    %       'NoiseZody'      - include zodiacal light in the background (1/0). Default is true.
    %       'NoiseCher'      - include Cherenkov background (1/0). Default is true.
    %       'NoiseStray'     - include stray light background (1/0). Default is true.
    %       'NoiseDark'      - include dark current background (1/0). Default is true.    
    %       'NoiseCross'     - include the cross-talk noise (1/0). Default is true.
    %       'Zodi'           - zodiacal light: a rate [e-/pix/s], or 'model' to compute it for
    %                          the pointing and DateObs with ultrasat.zodiac_bck: the zodiacal
    %                          spectrum at the tile centre folded with the throughput averaged
    %                          over the tile area (one value for the whole tile).
    %                          Default is 0.09 (= 27 e-/pix in 300 s, an estimate by YS).
    %       'Cherenkov'      - Cherenkov background rate [e-/pix/s]. Default is 0.05 (= 15 in 300 s).
    %       'Stray'          - stray light rate [e-/pix/s]. Default is 0.04 (= 12 in 300 s).
    %       'DarkCurrent'    - dark current rate [e-/pix/s], added to the background. Default is 0.04.
    %       'ReadNoise'      - read-out noise [e-/pix] in one frame, added as zero-mean
    %                          Gaussian noise in each frame. Default is sqrt(6) (a variance of
    %                          6 e-^2, an estimate by YS).
    %       'CrossTalk'      - cross-talk noise variance [e-^2/pix] in one frame, added as
    %                          zero-mean Gaussian noise; it does not change the mean level.
    %                          Default is 2 (an estimate by YS).
    %       'PoissonThreshold' - expected count [e-/pix] above which the noise of a pixel is drawn
    %                          from the Gaussian approximation instead of the Poisson distribution.
    %                          Default is 100.
    %       'LowCountNonLin' - the low-count detector non-linearity (charge trapping): 'none' or a
    %                          function handle S = f(S, Par) acting on the charge [e-/pix] in one
    %                          frame, e.g. @ultrasat.lowCountNonLin. It is applied to the noisy
    %                          image (after the cosmic rays, before the full-well cut and the ADU
    %                          conversion), not to the Var layer. Default is 'none'.
    %       'LowCountNonLinPar' - the parameters Par passed to LowCountNonLin. Default is [].
    %       'DateObs'        - the observation start time, 'yyyy-mm-ddTHH:MM:SS' (UTC), used for
    %                          the header and for Zodi = 'model'. Default is '2026-07-01T00:00:00'.
    %       'Seed'           - if not empty, the random number generator is seeded with this
    %                          value for the run (and restored afterwards), so the run can be
    %                          repeated exactly. Default is [].
    %       'Inj'            - source injection method (technical): 'direct' or 'stampcube'.
    %                          See the Args.Inj comment in the arguments block below for the measured
    %                          speed/accuracy trade-off between 'direct' (default) and 'stampcube'.
    %       'OutType'        - 'all' or 'FITS': return the AstroImage and write the FITS image(s),
    %                          the input-catalog text file and the region file(s);
    %                          'AstroImage' or 'none': return the AstroImage only. Default is 'all'.
    %       'ADUOutput'      - the ADU image(s) written for a single-exposure run (Exposure(1) = 1):
    %                          'threshold' - one 14-bit ADU image (_ADU.fits), each pixel read with the
    %                                        gain chosen by the threshold, the gain flag in its top bit;
    %                          'raw'       - two full ADU images, all pixels read with the high gain
    %                                        (_ADU_HI.fits) and all with the low gain (_ADU_LO.fits);
    %                          'production' - one 13-bit ADU image without the gain bit (_ADU.fits)
    %                                        and an 8-bit 0/1 map of the gain used, 1 = low gain
    %                                        (_GAIN.fits). Default is 'threshold'.
    %       'OutDir'         - the output directory
    %       'OutName'        - root name of the output files
    %       'SaveMatFile'    - whether to make an output .mat file with all the modelled structures,
    %                          written to OutDir when OutType is 'FITS' or 'all'
    %       'SaveRegionsBySourceMag' - whether to write additional region files according to the input source magnitudes
    %       'FlatMatrix' - a model flat field, 4738 x 4738 in the image orientation (rows = Y).
    %                    It scales the expected photon counts (sources, zodiacal, Cherenkov and
    %                    stray light) before the noise is drawn; dark current, read-out noise,
    %                    cross-talk and cosmic rays are not flat-fielded. Default is [].
    %       'Vignetting' - 'none' or 'ELOP': the 2-D map of the obscured fraction of the aperture
    %                    (ELOP, 2022; the file ObscurationMapELOP.mat in the ULTRASAT data
    %                    directory, next to the ULTRASAT properties file), fixed to the focal
    %                    plane with its centre at the aimpoint. The sources get the map in place of
    %                    the radial obscuration curve already contained in the throughput (a 0-4%
    %                    correction); the photon background (zodiacal, Cherenkov and stray light)
    %                    is scaled by the map transmission divided by its tile mean, so that the
    %                    given background rates stay tile means (a 10-14% gradient across a tile).
    %                    Orientation: detector +X from the aimpoint along the map's row angle,
    %                    +Y along minus its column angle (as in simulate_superflat.m for tile B).
    %                    Default is 'ELOP'; 'none' gives no vignetting.
    %       'AddCRStreaks' - add cosmic-ray hits. NB: a crude placeholder model: each pixel is hit
    %                    with a probability of 1% and set to the full well; the hits are single
    %                    pixels, not streaks. Single exposures only. Default is false.
    %       'PicklesDir' - a directory containing Pickles' stellar spectra
    %       'Phoenix' - an object containing Phoenix stellar spectra
    %       --- extended object mode: standalone alternative to Cat/Mag/Spec above ---
    %       'ExtProfileType' - '', 'sersic', 'gaussian', 'flat', or 'matrix'; '' = point-source mode (default).
    %                          Setting this to a non-empty value switches usim into extended-object mode,
    %                          simulating N extended objects (N = numel(ExtRA0)) instead of a point-source
    %                          catalog. ExtProfileType and ExtSpecType are shared by all N objects in a call;
    %                          all other Ext* parameters below are per-object (length-N vectors/matrices/cell
    %                          arrays, or a scalar to broadcast the same value to every object where noted).
    %                          Objects whose center falls outside the tile FOV are dropped with a warning.
    %       'ExtProfilePar'   - N x 3: per-object Sersic [Re, n, k] or Gaussian [SigmaX, SigmaY, Rho]
    %                          parameters (one row per object), in ExtOversampling grid units
    %       'ExtProfileMatrix'- a 1 x N cell array of user-supplied 2D profile matrices, one per object,
    %                          used when ExtProfileType = 'matrix'
    %       'ExtOversampling' - profile spatial grid oversampling, shared by all objects. Default is Args.ImRes (recommended)
    %       'ExtAxisRatio'    - b/a axis ratio; a length-N vector or a scalar to apply to all objects
    %       'ExtPA'           - [deg] position angle; a length-N vector or a scalar to apply to all objects
    %       'ExtSizeRA'       - length-N vector: object angular extent in the RA direction, [arcsec]
    %       'ExtSizeDec'      - length-N vector: object angular extent in the Dec direction, [arcsec]
    %       'ExtRA0'          - length-N vector: object center RA, [deg], or X pixel coordinate if
    %                           ExtSkyCat = false; N is taken from this argument
    %       'ExtDec0'         - length-N vector: object center Dec, [deg], or Y pixel coordinate if ExtSkyCat = false
    %       'ExtSkyCat'       - the flag determines whether ExtRA0/ExtDec0 are sky coordinates (true, default)
    %                           or pixel X, Y (false); same convention as SkyCat
    %       'ExtMag'          - length-N vector: total magnitude of each object
    %       'ExtEbv'          - E(B-V); a length-N vector or a scalar to apply to all objects
    %       'ExtSpec'         - N x npar (or, for ExtSpecType = 'tab', Nwave x N / Nwave x (N+1)): per-object
    %                          spectral parameters, same conventions as 'Spec'
    %       'ExtSpecType'     - the spectral model shared by all objects, same conventions as 'SpecType'
    %       'ExtFiltFam'      - filter family for ExtMag, shared by all objects
    %       'ExtFilt'         - filter for ExtMag, shared by all objects
    % Output : - an AstroImage object with filled Catalog property
    %            (also a FITS image file output + ds9 region files, RAW file output)           
    %            Sources are kept if their centre lies on the tile, 0.5 <= X,Y < 4738.5.
    %            Sources beyond the 10 deg radius of the throughput/PSF tables (the far
    %            corner of the tile reaches 10.13 deg) get the 10 deg throughput and PSF.
    %            Stars outside the Phoenix grid get the spectrum at the nearest grid edge, and
    %            stars where the grid has no models (hot, high-gravity stars) keep their Teff and
    %            get the highest log(g) with models at that Teff, both with a warning;
    %            sources whose flux is not finite (no valid spectrum) are dropped, with a warning.
    %          - an AstroPSF holding the PSF stamp of every source (also attached to the image
    %            as usimImage.PSFData): DataPSF is [Ny Nx NumSrc] in the image orientation, at
    %            1/ImRes of a pixel (Scale = ImRes), with the source number (the row of CatData)
    %            as the dimension 'Source'; the stamp of source k is
    %            AP.getPSF('PsfArgs',{'Source',k}). [] if NumSrc >= MaxPSFNum
    %          - an ADU image (int16 array in the image orientation); [] if Exposure(1) > 1.
    %            For 'ADUOutput' = 'raw' a 2-plane cube [HI LO], for 'production' [ADU GainMap]
    % Tested : Matlab R2020b
    % Author : A.M. Krassilchtchikov (2023, 2026)
    % Example: Sim = ultrasat.usim('Cat',1000)
    % (simulate 1000 sources at random positions with the default spectrum and magnitude)
    %
    % Example: Sim = ultrasat.usim('ExtProfileType','sersic','ExtProfilePar',[70 4 1], ...
    %              'ExtAxisRatio',1,'ExtSizeRA',370,'ExtSizeDec',370,'ExtOversampling',1, ...
    %              'ExtRA0',221.787891,'ExtDec0',56.361518,'ExtMag',13, ...
    %              'ExtSpecType','BB','ExtSpec',10000,'Tile','B','Exposure',[1 300])
    % (simulate a single extended Sersic-profile object with a 10000 K blackbody spectrum)
    %
    % Example: Sim = ultrasat.usim('ExtProfileType','sersic', ...
    %              'ExtProfilePar',[40 4 1; 12 2 1; 60 4 1], ...
    %              'ExtSizeRA',[200 60 400],'ExtSizeDec',[200 60 250],'ExtOversampling',1, ...
    %              'ExtRA0',[221.60 221.75 221.99],'ExtDec0',[56.30 56.40 56.45], ...
    %              'ExtMag',[12 15 13],'ExtSpecType','BB','ExtSpec',[8000;6000;12000], ...
    %              'Tile','B','Exposure',[1 300])
    % (simulate 3 extended Sersic-profile objects of different sizes, positions,
    %  magnitudes, and blackbody temperatures, in a single call)
    %
    % Example: Sim = ultrasat.usim('Cat',[2369 2369],'SkyCat',false,'Mag',12)
    % (simulate a single bright point-like source on-axis, at the tile's central pixel)
    %
    arguments
        Args.Cat             =  10;          % if a number (N), generate N random fake sources
                                             % if a 2D table, use X, Y from this table
                                             % if an AstroCatalog object, use source coordinates from this object
        Args.SkyCat logical  = false;        % the flag determines whether the input coordinates are RA, Dec or in X, Y
        Args.PlaneRotation   =   0;          % [deg] rotation of the detector plane w.r.t. the north celestial pole
                                             
        Args.Mag             =  20;          % apparent magnitudes of the input sources: 
                                             % one magnitude for all the objects or a vector of magnitudes                                              
        Args.Ebv             =   0;          % E(B-V) of the input sources: of the observed field 
                                             % one value for the whole field or a vector of individual values
                                             % if E(B-V) > 0 the input magnitudes are treated as dereddened !!
        Args.ExtLaw          = 'CCM89';      % extinction law, as 'Model' of astro.extinction.extinction: 'CCM89' or 'F99'
        Args.Rv              = 3.08;         % R_V of the extinction law
        Args.FiltFam         = {'ULTRASAT'}; % one filter family for all the source magnitudes or an array of families
                                             
        Args.Filt            = {''};         % one filter for all the source magnitudes or an array of filters
        
        Args.CalculateULTRASATMag logical = true; % if the input magnitudes are not of the ULTRASAT filters, 
                                             % calculate the ULTRASAT magnitudes at output  
        Args.CalculateCrudeSNR logical = true; % estimate SNR for the input sources
        Args.SNRMethod        = 'aperture';  % how the CrudeSNR column is computed. The image this
                                             % simulator produces has a per-pixel variance of
                                             % (source counts + BackVar) -- see the poissrnd/normrnd
                                             % call further below -- so the matched estimator for an
                                             % aperture A is  sum_A S / sqrt( sum_A S + N_A*BackVar ).
                                             % 'aperture' : that expression, evaluated exactly on the
                                             %              source's own stamp resampled to the
                                             %              detector grid, maximised over the aperture
                                             %              radius. Default: it is the only variant
                                             %              that predicts what aperture photometry on
                                             %              the output image actually returns.
                                             % 'optimal'  : matched filter, sqrt(sum S^2/(S+BackVar)).
                                             %              The theoretical ceiling (PSF fitting);
                                             %              reads ~10-15% above aperture photometry.
                                             % 'shot'     : the legacy expression with the missing
                                             %              source-shot-noise term restored, i.e.
                                             %              S/sqrt(S + pi*R50^2*BackVar). Keeps the
                                             %              legacy PSFeff = 0.8 numerator.
                                             % 'legacy'   : the historical background-limited formula,
                                             %              0.8*F*T/sqrt(pi*R50^2*BackVar). It omits
                                             %              the source's own shot noise and so reads
                                             %              far too high for bright compact sources
                                             %              (~8x for a 60 counts/s star in 300 s).
                                             %              Kept because it is exactly linear in flux,
                                             %              which callers that invert it rely on.
        Args.MeasurePSF       logical = false; % diagnostics: measure every point source's final
                                             % (rotated, jittered) PSF -- the 50% encircled-flux
                                             % radius and a pseudo-FWHM -- and attach them to the
                                             % output as usimImage.UserData.PSFWidths, a table with
                                             % columns X, Y, FieldRadiusDeg, R50arcsec, FWHMarcsec,
                                             % plus a one-line summary on screen. Point sources
                                             % only (ignored, with a warning, for extended objects)
        Args.SNROnly          logical = false; % if true, return as soon as CrudeSNR (and the rest of the
                                             % output catalog) is computed, skipping the noise/ADU pipeline
                                             % and file writing entirely (usimImage.Image is [] in this case).
                                             % A cheap way to get CrudeSNR at a given input magnitude, e.g. to
                                             % solve for the magnitude needed for a target SNR, without paying
                                             % for noise generation (often the dominant cost, see the
                                             % NoisePoisson/PoissonThreshold branch below)

        Args.SpecType        = {'BB'};       % parameters of the source spectra: 
                                             % either an array of AstroSpec objects
                                             % or an array of model spectra parameters: 
                                             % {'BB'} 3500 [Temperature (K)] -- blackbody
                                             % {'PL'} 2.   [Alpha -- power-law F ~ lambda^alpha]
                                             % {'Pickles'} [6e3 4.6] -- table of Teff [K] and log(g)  
                                             % {'Phoenix'} [6e3 4.6] -- table of Teff [K] and log(g)  
                                             % {'Tab'} table: NumSrc spectra, each spectral flux in a column
                                             % NB: the input spectral flux should be
                                             % in [erg cm(-2) s(-1) A(-1)] as seen near Earth (absorbed)!
        Args.Spec            = 5800;
                                             
        Args.Exposure        = [3 300];      % number and duration of exosures [s]; 1 x 300 s is the standard ULTRASAT exposure
        
        Args.Tile            = 'B';          % the tile name: 'A' and 'B' are the upper tiles, 'D' and 'C' are the lower tiles
        
        Args.ImRes           = 5;            % the internal image resolution: 5 is 1/5 of the ULTRASAT pixel
                                             % possible values: 1, 2, 5, 10, 47.5
                                             
        Args.Jitter          = false;        % include the effect of S/C jitter 
        Args.JitterPar       = [2 2 10];     % [SigmaX0 SigmaY0 Rotation] [arcsec] for a 300 s exposure
                                             
        Args.RotAng          = 0;            % the PSF rotation angle relative to the axis of the raw PSF database (deg)
                                             % may be a vector with individual angle for each of the sources
                                            
        Args.RA0             = 214.99;       % the default position of the [0 0] pixel (~ telescope FOV center)
        Args.Dec0            = 52.78;        % is a certain point in the GALEX GROTH_00 field
        Args.WCSFile         = {''};         % if not empty, the image WCS is read from this file
                                             
        Args.ArraySizeLimit  = 8;            % [Gb] the limit determines the method employed in inUtil.psf.specWeight
        Args.MaxNumSrc       = 10000;        % the maximal size of a source chunk to be worked over at a time
        
        Args.Verbosity       = 0;            % 0: silent; 1: report the progress
        Args.MaxPSFNum       = 10000;        % if the number of input sources is above this value, 
                                             % do not record individual PSF and do not attach them to the output AstroImage 
        Args.NoisePoisson   logical = true;  % random noise: shot noise + read-out/cross-talk noise

        % individual on/off toggles for each background component (see the BackPhot/BackDark/
        % VarRead assignments below); e.g. for a lab test with no sky/space background, set
        % NoiseZody/NoiseCher/NoiseStray to false
        Args.NoiseZody      logical = true;    % zodiacal light
        Args.NoiseCher      logical = true;    % Cherenkov background
        Args.NoiseStray     logical = true;    % stray light
        Args.NoiseDark      logical = true;    % dark current
        Args.NoiseCross     logical = true;    % cross-talk noise

        % background rates [e-/pix/s]; the defaults are the 300 s estimates by YS divided by 300
        Args.Zodi            = 0.09;         % zodiacal light: a rate, or 'model' (ultrasat.zodiac_bck)
        Args.Cherenkov       = 0.05;         % Cherenkov background
        Args.Stray           = 0.04;         % stray light
        Args.DarkCurrent     = 0.04;         % dark current

        % zero-mean noise terms of one frame (they add to the variance, not to the mean level)
        Args.ReadNoise       = sqrt(6);      % [e-/pix] read-out noise
        Args.CrossTalk       = 2;            % [e-^2/pix] cross-talk noise variance

        Args.PoissonThreshold = 100;         % [e-/pix] expected count above which the noise of a pixel is
                                             % drawn from the Gaussian approximation; see drawCounts below

        Args.LowCountNonLin    = 'none';     % 'none' or a function handle S = f(S, Par) [e-/pix/frame]
        Args.LowCountNonLinPar = [];         % the parameters Par of LowCountNonLin

        Args.DateObs         = '2026-07-01T00:00:00'; % observation start (UTC), 'yyyy-mm-ddTHH:MM:SS'
        Args.Seed            = [];           % if not empty, seed the random number generator for this run

        Args.Inj             = 'direct';     % source injection method: 'direct' or 'stampcube'
                                             % 'stampcube' uses the same imUtil.art.createSourceCube/addSources
                                             % pipeline as the extended-object mode (per-source downsample +
                                             % Lanczos sub-pixel shift + a single vectorized embed), instead of
                                             % 'direct's whole-tile imresize up to the PSF oversampled grid and
                                             % back down.
                                             % Trade-off (measured on real ULTRASAT catalogs, ImRes=1..10,
                                             % up to 5000 sources/chunk):
                                             %  - much faster at high ImRes with moderate source counts, e.g.
                                             %    ~17x at ImRes=10/300 sources, ~6x at ImRes=5/300 sources
                                             %    chunked; also avoids 'direct's whole-tile resize memory cost,
                                             %    which grows as ImRes^2 (tens of GB at ImRes=10 and above);
                                             %  - roughly on par, or slightly slower, at low ImRes (1-2) or very
                                             %    large source counts per chunk (~5000+), where 'direct's
                                             %    resize cost is amortized and stampcube's per-source Lanczos
                                             %    step dominates instead;
                                             %  - total injected flux matches 'direct' to ~1e-7 relative in all
                                             %    tested cases (no flux leakage), but individual source flux is
                                             %    redistributed slightly differently within its PSF footprint
                                             %    (Lanczos vs. nearest-pixel sub-pixel interpolation): typically
                                             %    below 1%, up to a few percent for the worst source in a large
                                             %    (~5000) chunk or at ImRes=1.
                                             % Currently opt-in; 'direct' remains the default given the mixed
                                             % performance picture above and to preserve historical pipeline
                                             % output (simulateOmegaCen/simulateKeplerField/simulateN3/unitTest
                                             % all call usim without Inj and rely on 'direct's exact output).
        
        Args.OutType         = 'all';        % output type: 'AstroImage', 'FITS', 'all' (default)
        
        Args.ADUOutput       = 'threshold';  % 'threshold', 'raw' or 'production': the ADU image(s), see the help
        Args.OutDir          = '.';          % the output directory
        Args.OutName         = 'SimImage';   % root name of output files

        Args.SaveMatFile  logical = false;            % whether to make an output .mat file with all the modelled structures
        Args.SaveRegionsBySourceMag logical = false;  % whether to write additional region files according to the input source magnitudes
        
        
        Args.PicklesDir = '~/matlab/data/spec/PicklesStellarSpec/';
        Args.Phoenix    = '~/matlab/data/spec/Phoenix/phoenix_mtl0_rescale10.mat';
        
        Args.FlatMatrix = [];                % a model flat field, 4738 x 4738 in the image orientation (rows = Y)
        Args.Vignetting = 'ELOP';            % 'ELOP' or 'none': the 2-D obscuration map (see the help)

        Args.AddCRStreaks logical = false;   % add cosmic-ray hits (a crude placeholder model, see the help)

        % extended object mode: a standalone alternative to the point-source Cat/Mag/Spec
        % inputs above; active whenever ExtProfileType is non-empty. Simulates N objects,
        % N = numel(ExtRA0). ExtProfileType/ExtSpecType/ExtOversampling/ExtFiltFam/ExtFilt
        % are shared by all N objects; the rest are per-object (length-N, or a scalar to
        % broadcast where noted below).
        Args.ExtProfileType   = '';          % '', 'sersic', 'gaussian', 'flat', or 'matrix'; '' = point-source mode (default)
        Args.ExtProfilePar    = [2 4 1];     % N x 3: per-object Sersic [Re, n, k] or Gaussian [SigmaX, SigmaY, Rho], in ExtOversampling grid units
        Args.ExtProfileMatrix = [];          % 1 x N cell array of user-supplied 2D profiles, used when ExtProfileType = 'matrix'
        Args.ExtOversampling  = [];          % profile spatial grid oversampling, shared by all objects; default: same as Args.ImRes
        Args.ExtAxisRatio     = 1;           % b/a axis ratio, applied to the profile regardless of ExtProfileType; length-N vector or scalar (broadcast)
        Args.ExtPA            = 0;           % [deg] position angle, applied to the profile regardless of ExtProfileType; length-N vector or scalar (broadcast)
        Args.ExtSizeRA        = [];          % length-N: object angular extent in the RA direction, [arcsec]
        Args.ExtSizeDec       = [];          % length-N: object angular extent in the Dec direction, [arcsec]
        Args.ExtRA0           = [];          % length-N: object center RA, [deg], or X pixel coordinate if ExtSkyCat = false; N is taken from this argument
        Args.ExtDec0          = [];          % length-N: object center Dec, [deg], or Y pixel coordinate if ExtSkyCat = false
        Args.ExtSkyCat logical = true;       % the flag determines whether ExtRA0/ExtDec0 are sky coordinates or pixel X, Y (same convention as SkyCat)
        Args.ExtMag           = [];          % length-N: total magnitude of each object
        Args.ExtEbv           = 0;           % E(B-V); length-N vector or scalar (broadcast) (same convention as Ebv)
        Args.ExtSpec          = 5800;        % same conventions as Spec, but N x npar (one row per object)
        Args.ExtSpecType      = 'BB';        % same conventions as SpecType, shared by all N objects
        Args.ExtFiltFam       = 'ULTRASAT';  % filter family for ExtMag, shared by all objects
        Args.ExtFilt          = '';          % filter for ExtMag, shared by all objects
    end
    
    % outputs that are not produced in every mode
    AP               = [];
    ImageSrcNoiseADU = [];

    % the extinction law: F99 is coded for R_V = 3.1 only (astro.extinction.extinction)
    if ~any(strcmpi(Args.ExtLaw, {'CCM89','F99'}))
        error('usim: unknown ExtLaw %s, use CCM89 or F99', Args.ExtLaw);
    end
    if strcmpi(Args.ExtLaw, 'F99') && Args.Rv ~= 3.1
        error('usim: the F99 extinction law is coded for R_V = 3.1 only, Rv = %g given', Args.Rv);
    end

    % input format correction
    if ~iscell(Args.FiltFam)
                Args.FiltFam  = {Args.FiltFam};
    end
    if ~iscell(Args.Filt)
                Args.Filt     = {Args.Filt};
    end
    if ~iscell(Args.SpecType)
                Args.SpecType = {Args.SpecType};
    end
    % NB: SpecType is shared by all the point sources, just as ExtSpecType is shared by
    % all the extended objects: the per-model layouts of Spec (1 parameter per source for
    % 'BB' and 'PL', 2 for 'Pickles' and 'Phoenix', a whole column for 'Tab') can not be
    % held in one array, so a per-source mixture of models is not representable here
    if numel(Args.SpecType) > 1
        if all( strcmpi(Args.SpecType, Args.SpecType{1}) )
            Args.SpecType = Args.SpecType(1);
        else
            error('SpecType is shared by all the point sources: pass a single spectral model, exiting..');
        end
    end
    if ~iscell(Args.WCSFile)
                Args.WCSFile  = {Args.WCSFile};
    end 
    Args.RotAng = Args.RotAng(:);   % a row or a column of per-source angles
    if isempty(regexp(Args.DateObs, '^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}$', 'once'))
        error('DateObs must be given as ''yyyy-mm-ddTHH:MM:SS'', exiting..');
    end
    if ~isa(Args.LowCountNonLin,'function_handle') && ~strcmpi(Args.LowCountNonLin,'none')
        error('LowCountNonLin must be ''none'' or a function handle, exiting..');
    end
    if (ischar(Args.Zodi) || isstring(Args.Zodi)) && ~strcmpi(Args.Zodi,'model')
        error('Zodi must be a rate [e-/pix/s] or ''model'', exiting..');
    end
    if ~isempty(Args.FlatMatrix) && ~isequal(size(Args.FlatMatrix), [4738 4738])
        error('FlatMatrix must be 4738 x 4738 (in the image orientation, rows = Y), exiting..');
    end
    if ~any(strcmpi(Args.Vignetting, {'none','ELOP'}))
        error('Vignetting must be ''none'' or ''ELOP'', exiting..');
    end
    if ~any(strcmpi(Args.ADUOutput, {'threshold','raw','production'}))
        error('ADUOutput must be ''threshold'', ''raw'' or ''production'', exiting..');
    end
    if ~isempty(Args.Seed)   % a repeatable run; the caller's generator state is restored on exit
        RngSaved   = rng;
        rng(Args.Seed);
        RngRestore = onCleanup(@() rng(RngSaved)); %#ok<NASGU>
    end
    
                        % simulation start 
                        Exposure = Args.Exposure(1) * Args.Exposure(2); 
                            
                        if Args.Verbosity > 0, cprintf('hyper','ULTRASAT simulation started\n'); end
                        if Args.Verbosity > 0, fprintf('%s%d%s%s\n','The total requested exposure is ',Exposure,...
                                ' seconds for tile ',Args.Tile); end
                        if Args.Verbosity > 0, fprintf('%s%d%s\n','The requested PSF resolution is 1/',Args.ImRes,' of the pixel size'); end
                            
                        % performance speed test
                        Tstart = tic;        % the whole run
                        T0     = tic;        % the current stage
    
    %%%%%%%%%%%%%%%%%%%%% Simulation parameters and some physical constants
    
    Tiny = 1e-30;
    
    C   = constant.c;       % the speed of light in vacuum, [cm/s]
    H   = constant.h;       % the Planck constant, [erg s]   
    
    RAD = 180./pi;          % the radian
    
    %%%%%%%%%%%%%%%%%%%% ULTRASAT PSF database parameters
               
    Nrad    = 25; 
    Rad     = linspace(0,10,Nrad);          % lab PSF grid points in radius    
            
    MinWave = 2000;  % [A] the band boundaries
    MaxWave = 11000; % [A]
    
    WavePSF = linspace(MinWave,MaxWave,91); % lab PSF grid points in wavelength
    
        
    %%%%%%%%%%%%%%%%%%%% the spectral grid used for countrate calculations

    Nwave   = 9001;                                % number of spectral bins used for countrate calculations
    Wave    = linspace(MinWave,MaxWave,Nwave);     % the wavelength grid 
    
    DeltaLambda = (MaxWave-MinWave)/(Nwave-1);     % the wavelength bin size in Angstroms 
    
    %%%%%%%%%%%%%%%%%%%% basic ULTRASAT imaging parameters

    ImageSizeX  = 4738; % tile size (pix)
    ImageSizeY  = 4738; % tile size (pix)
    
    FocalLength = 360;    % [mm] 
    PixelSizeMm = 9.5e-3; % [mm] pixel size 
    PixSizeDeg  = ( PixelSizeMm / FocalLength ) * RAD;  % [deg] pixel size  
    
    GapMm       = 2.4;  % gap width in mm
    Ngap        = ceil( GapMm / PixelSizeMm); % number of pixels in the gap
    
    DAper       = 33.;                   % [cm]    aperture diameter
    SAper       = pi * DAper ^ 2 / 4;    % [cm(2)] aperture area
    
    % coordinates of the inner core of the tile and the rotation angle of the PSF: 
    % NB: the additional rotation by -90 deg for tile "B" is required to have the PSF coma in the right position!
    % the CRPIX values are chosen so that the CRVAL coordinates are at the aimpoint of the telescope
    switch Args.Tile
        case 'A'
            X0 = ImageSizeX + 0.5; Y0 = 0.5;
            RotAngle = Args.RotAng - 90 + 90;  
            CRPIX1   = ImageSizeX+Ngap/2;
            CRPIX2   = -Ngap/2;
        case 'B'
            X0 = 0.5;              Y0 = 0.5;
            RotAngle = Args.RotAng - 90 + 0;  
            CRPIX1   = -Ngap/2;
            CRPIX2   = -Ngap/2;
        case 'C'
            X0 = 0.5;              Y0 = ImageSizeY + 0.5;
            RotAngle = Args.RotAng - 90 - 90;  
            CRPIX1   = -Ngap/2;
            CRPIX2   = ImageSizeY+Ngap/2;
        case 'D'
            X0 = ImageSizeX + 0.5; Y0 = ImageSizeY + 0.5;
            RotAngle = Args.RotAng - 90 + 180;  
            CRPIX1   = ImageSizeX+Ngap/2;
            CRPIX2   = ImageSizeY+Ngap/2;
        otherwise            
            error('Invalid tile name, exiting..');   
    end
    
    % pixel saturation and per pixel background (from YS),
    %  e-/ADU conversion coefficients
    FullWell    = 1.6e5;  % [e-] the pixel saturation limit for 1 exposure
    GainThresh  = 1.6e4;  % [e-/pix] the gain threshold
    E2ADUhigh   = 1.185;  % below GainThresh e-/pix 
    E2ADUlow    = 0.074;  % above GainThresh e-/pix
    
    %%%%%%%%%%%%%%%%%%%% load the matlab object with the ULTRASAT properties:
    I = Installer;
    UP_db = sprintf('%s%s',I.getDataDir('ULTRASAT_Properties'),'/P90_UP_test_60_ZP_Var_Cern_21.mat');  
    io.files.load1(UP_db,'UP');
    
    %%%%%%%%%%%%%%%%%%%%%  read the chosen PSF database from a .mat file

                                if Args.Verbosity > 0, fprintf('Reading PSF database.. '); end

    PSF_db = sprintf('%s%s%g%s',I.getDataDir('ULTRASAT_PSF'),'/ULTRASATlabPSF',Args.ImRes,'.mat');
    ReadDB = struct2cell ( io.files.load1(PSF_db) ); % PSF data at the chosen spatial resolution
    PSFdata = ReadDB{2}; 

     if size(PSFdata,4) ~= Nrad 
         error('PSF array size mismatch, exiting..');
     end

                                if Args.Verbosity > 0, fprintf('done\n'); end
                                T0 = reportTime(Args.Verbosity, T0);
        
    %%%%%%%%%%%%%%%%%%%% read source coordinates from an input catalog or make a fake catalog
    
    if isempty(Args.WCSFile{1}) % make a local WCS
        
        SimWCS = AstroWCS();
        SimWCS.ProjType  = 'TAN';
        SimWCS.ProjClass = 'ZENITHAL';
        SimWCS.CooName   = {'RA'  'DEC'};
        SimWCS.CTYPE     = {'RA---TAN','DEC--TAN'};
        SimWCS.CUNIT     = {'deg', 'deg'};
        SimWCS.CD(1,1)   = PixSizeDeg;
        SimWCS.CD(2,2)   = PixSizeDeg;  
        SimWCS.CRVAL(1)  = Args.RA0;
        SimWCS.CRVAL(2)  = Args.Dec0;
        SimWCS.CRPIX(1)  = CRPIX1; 
        SimWCS.CRPIX(2)  = CRPIX2; 
        SimWCS.populate_projMeta;
        
        % rotate the image:
        Alpha_rad = Args.PlaneRotation/RAD;
        RotMatrix = [cos(Alpha_rad), -sin(Alpha_rad);
                     sin(Alpha_rad),  cos(Alpha_rad)];
        SimWCS.CD = RotMatrix * SimWCS.CD;
        
    else % or read in an appropriate ULTRASAT header and extract WCS
        SimHeader = AstroHeader(Args.WCSFile{1},1);
        SimWCS    = AstroWCS.header2wcs(SimHeader);
        SimWCS.populate_projMeta;
    end

    %%%%%%%%%%%%%%%%%%%% background levels [e-/pix over the whole exposure] and the
    %%%%%%%%%%%%%%%%%%%% zero-mean noise variance of the frames [e-^2/pix]

    if isnumeric(Args.Zodi)
        ZodiRate = Args.Zodi;
        ZodiMagV = [];
    else   % 'model': the zodiacal light for this pointing and time
        [ZodiRate, ZodiMagV] = zodiModelRate(SimWCS, Args.DateObs, ImageSizeX, ImageSizeY, X0, Y0, ...
                                             PixSizeDeg, UP, Rad, Wave, DeltaLambda, SAper, H*C);
    end
    BackPhot = ( Args.NoiseZody  * ZodiRate + Args.NoiseCher * Args.Cherenkov + ...
                 Args.NoiseStray * Args.Stray ) * Exposure;                     % photons: flat-fielded
    BackDark = Args.NoiseDark * Args.DarkCurrent * Exposure;                    % dark current
    VarRead  = ( Args.ReadNoise.^2 + Args.NoiseCross * Args.CrossTalk ) * Args.Exposure(1);
    BackVar  = BackPhot + BackDark + VarRead;    % the background variance per pixel, for CrudeSNR

    % the vignetting factors of the sources and of the photon background (1 for 'none')
    [VigSrc, VigBkg] = vignettingMaps(Args.Vignetting, ImageSizeX, ImageSizeY, X0, Y0, ...
                                      [CRPIX1 CRPIX2], PixSizeDeg, Rad, UP.obscuration);

    PSFWidths = [];   % filled in only with Args.MeasurePSF, for point sources

    if isempty(Args.ExtProfileType)

    if isa(Args.Cat,'AstroCatalog') % read sources from an AstroCatalog object
        NumSrc        = size(Args.Cat.Catalog,1); 
        RA            = Args.Cat.Catalog(:,find(strcmp(Args.Cat.ColNames, 'RA' ))); 
        DEC           = Args.Cat.Catalog(:,find(strcmp(Args.Cat.ColNames, 'Dec'))); 
        if isempty(find(strcmp(Args.Cat.ColNames, 'X'), 1)) % no pixel coordinates in the catalog
            [CatX, CatY]  = SimWCS.sky2xy(RA,DEC);          % get them from the SimWCS
        else                                                % read pixel coordinates
            CatX      = Args.Cat.Catalog(:,find(strcmp(Args.Cat.ColNames, 'X'))); 
            CatY      = Args.Cat.Catalog(:,find(strcmp(Args.Cat.ColNames, 'Y'))); 
        end               
                            if Args.Verbosity > 0, fprintf('%d%s\n',NumSrc,' sources read from the input AstroCatalog object'); end
    elseif size(Args.Cat,2) > 1 && size(Args.Cat,3) == 1 % read source coordinates from a text table
        NumSrc = size(Args.Cat,1);
        if Args.SkyCat   % the input coordinates are sky coordinates
            RA  = Args.Cat(:,1);
            DEC = Args.Cat(:,2);
            [CatX, CatY] = SimWCS.sky2xy(RA,DEC); 
        else             % the input coordinates are pixel coordinates
            CatX   = Args.Cat(:,1);
            CatY   = Args.Cat(:,2);
            [RA, DEC] = SimWCS.xy2sky(CatX,CatY);
        end
                            if Args.Verbosity > 0, fprintf('%d%s\n',NumSrc,' sources read from the input table'); end
    elseif size(Args.Cat,2) == 1 % make a fake catalog with Args.Cat sources randomly distributed over the FOV
        NumSrc = Args.Cat;  % the number of fake sources
        CatX    = max(0.51, ImageSizeX * rand(NumSrc,1) ); 
        CatY    = max(0.51, ImageSizeY * rand(NumSrc,1) ); 
        [RA, DEC] = SimWCS.xy2sky(CatX,CatY);
                            if Args.Verbosity > 0, fprintf('%d%s\n',NumSrc,' random sources generated'); end
    else
                            error('Incorrect catalog input, exiting..');                             
    end
    
    %%% make arrays of magnitudes and filters:
    
    InMag = perSourceColumn(Args.Mag, NumSrc, 'Mag');
    InEbv = perSourceColumn(Args.Ebv, NumSrc, 'Ebv');
    if numel(RotAngle) ~= 1 && numel(RotAngle) ~= NumSrc
        error('RotAng must hold 1 or NumSrc (=%d) angles, exiting..', NumSrc);
    end
    if isa(Args.Spec,'AstroSpec') && numel(Args.Spec) ~= 1 && numel(Args.Spec) ~= NumSrc
        error('Spec must hold 1 or NumSrc (=%d) AstroSpec objects, exiting..', NumSrc);
    end
    % NB: a per-source list is used as it is (one cell per source), it is not to be
    % wrapped into a single cell: FiltFam/Filter are indexed per source further below
    if numel(Args.FiltFam) > 1
        if numel(Args.FiltFam) ~= NumSrc
            error('FiltFam must hold 1 or NumSrc (=%d) filter families, exiting..', NumSrc);
        end
        FiltFam = reshape(Args.FiltFam,1,NumSrc);
    else
        FiltFam = repmat(Args.FiltFam,1,NumSrc);
    end
    if numel(Args.Filt) > 1
        if numel(Args.Filt) ~= NumSrc
            error('Filt must hold 1 or NumSrc (=%d) filters, exiting..', NumSrc);
        end
        Filter = reshape(Args.Filt,1,NumSrc);
    else
        Filter = repmat(Args.Filt,1,NumSrc);
    end
    % the ULTRASAT family uses the radial-zone filters, the others the named filters:
    % one call handles one kind only
    IsUltrasatFam = strcmpi(FiltFam,'ULTRASAT');
    if any(IsUltrasatFam) && ~all(IsUltrasatFam)
        error('FiltFam: the ULTRASAT family can not be mixed with other filter families in one call, exiting..');
    end
    UseZoneFilt = all(IsUltrasatFam);
    
    %%% check if some the of the magnitudes are non-numerical and make them efficiently zero:
    Ind = isnan(InMag); 
    InMag(Ind) = 100;
                        if sum(Ind) > 0
                            if Args.Verbosity > 0, cprintf('red','%d%s\n',sum(Ind),' input magnitudes are non-numerical'); end
                        end
                        if sum(InEbv) > 0
                            if Args.Verbosity > 0, cprintf('blue','%s\n','Non-zero extinction requested: please, make sure, that the input magnitudes are dereddened!'); end
                        end 
    %%% check which of the sources fall out of the tile FOV and cut the input lists
    InFOV = (CatX >= 0.5) .* (CatY >= 0.5) .* (CatX < ImageSizeX+0.5) .* (CatY < ImageSizeY+0.5);
    if sum(InFOV) < 1 
        error('No objects in the FOV, exiting...');
    end
    if sum(InFOV) < NumSrc 
        if Args.Verbosity > 0, cprintf('red','%d%s\n',NumSrc-sum(InFOV),' objects out of the FOV'); end
        Ind = logical(InFOV);
        NumSrc  = sum(InFOV);
        InMag   = InMag(Ind);
        FiltFam = FiltFam(Ind);
        Filter  = Filter(Ind);
        CatX    = CatX(Ind);
        CatY    = CatY(Ind);
        RA      = RA(Ind);
        DEC     = DEC(Ind);
        InEbv   = InEbv(Ind);
        Args.Spec = cutSpecToFOV(Args.Spec, Args.SpecType{1}, Ind);
        if ( numel(RotAngle) ~= 1)
            RotAngle = RotAngle(Ind);   % NB: RotAngle, not Args.RotAng: it is derived above and used below
        end
    end
    CatFlux  = zeros(NumSrc,1);  % will be determined below from spectra * transmission 
    MagU     = zeros(NumSrc,1);  % will be calculated below if requested 
    CrudeSNR = zeros(NumSrc,1);  % will be calculated below if requested 
    Dropped  = false(NumSrc,1);  % sources with a non-finite flux, removed after the chunk loop
    KeepPSF  = NumSrc < Args.MaxPSFNum;   % record the individual PSF stamps
    if Args.MeasurePSF
        PSFWidths = NaN(NumSrc,3);  % [field radius (deg), R50 (pix), FWHM (pix)]
    end
    
    %%%%%%%%%%%%%%%%%%%%% split the list of objects into chunks and work
    %%%%%%%%%%%%%%%%%%%%% chunk-by-chunk 
    
    NCh  = ceil ( NumSrc / Args.MaxNumSrc );
    ChL  = zeros(NCh,1,'uint32'); % upto 4,29 x 10(9) objects 
    ChR  = zeros(NCh,1,'uint32');
    
    for ICh = 1:NCh-1
        
        ChL(ICh) = (ICh - 1) * Args.MaxNumSrc + 1;
        ChR(ICh) = ICh * Args.MaxNumSrc;
        
    end
    
    ChL(NCh) = (NCh-1) * Args.MaxNumSrc + 1;
    ChR(NCh) = NumSrc; 
    
    ImageSrc = zeros(ImageSizeX, ImageSizeY, 'single'); % make an empty source image

    if strcmpi(Args.SpecType{1},'phoenix') && ~isa(Args.Spec,'AstroSpec')
        Phoenix = io.files.load2(Args.Phoenix);   % the Phoenix spectral grid, read once for all the chunks
    end
    
    %%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%
    
    for ICh = 1:NCh   % main loop over source chunks: radial distances, spectra, convolution, injection 

        if Args.Verbosity > 0, cprintf('hyper','%s%d%s%d\n','Chunk ',ICh, ' out of ',NCh); end

        NumSrcCh = ChR(ICh) - ChL(ICh) + 1;  % number of sources in the current chunk (Args.MaxNumSrc or less)
        Range    = ChL(ICh):ChR(ICh); 

        %%%%%%%%%%%%%%%%%%%%% obtain radial distances of the sources from the INNER CORNER of the tile 
        %%%%%%%%%%%%%%%%%%%%% and regrid the throughput array at given source positions 

        RadSrc = sqrt( ( CatX(Range) - X0 ).^2 + ( CatY(Range) - Y0 ).^2 ) .*  PixSizeDeg;   % the source radii [deg]        
        RadTab = min(RadSrc, Rad(end));     % the tables end at 10 deg, the tile corner is at 10.13 deg
        TotT   = interpn(UP.wavelength, Rad', UP.TotT, Wave', RadTab', 'linear', Tiny)';     % regrid the throughput 
        [~, IndR] = min( abs(RadTab - Rad), [], 2);                                          % IndR is the RXX filter 
                                                                                             % zone number for each of the sources
        %%%%%%%%%%%%%%%%%%%%% read or generate source spectra
        %%%%%%%%%%%%%%%%%%%%%

                                if Args.Verbosity > 0, fprintf('Reading source spectra.. '); end

        SpecIn  = zeros(NumSrcCh, Nwave);  % the incoming spectra 

        % read the input spectra or generate synthetic spectra 
                 
        switch isa(Args.Spec,'AstroSpec')

            case 0  % make a synthetic spectrum for a given model

                switch lower( Args.SpecType{1} ) 

                    case 'bb'                   

                        if numel( Args.Spec ) ~= NumSrc && numel( Args.Spec ) ~= 1
                            error('The size of the source temperature array is incorrect, exiting..');
                        elseif numel( Args.Spec ) == 1
                            if Args.Verbosity > 0, fprintf('%s%5.0f%s','generating BB spectra for T = ',Args.Spec,' K .. '); end
                            Temp = Args.Spec .* ones(NumSrcCh,1);
                        else
                            if Args.Verbosity > 0, fprintf('%s','generating BB spectra for individual source temperatures .. '); end
                            Temp = Args.Spec( Range );
                        end

                        SpecIn = blackBodyShape(Wave, Temp);   % the spectral shape only: rescaled to the magnitudes below

                    case 'pl'

                        if numel( Args.Spec ) ~= NumSrc && numel( Args.Spec ) ~= 1
                            error('The size of the source spectral index array is incorrect, exiting..');
                        elseif numel( Args.Spec ) == 1
                            if Args.Verbosity > 0, fprintf('%s%5.0f','generating PL spectra for Alpha = ',Args.Spec); end
                            Alpha = Args.Spec .* ones(NumSrcCh,1);
                        else
                            if Args.Verbosity > 0, fprintf('%s','generating PL spectra for individual spectral indexes .. '); end
                            Alpha = Args.Spec( Range );
                        end
                        
                        SpecIn = Wave .^ Alpha;                                      % erg s(-1) cm(-2) A(-1)   

                    case 'pickles' 
                        
                        if Args.Verbosity > 0, fprintf('%s','generating Pickles spectra for individual values of Teff and log(g) .. '); end
                        SpecPar = chunkSpecPar(Args.Spec, Range, NumSrc, NumSrcCh); % this chunk's Teff and log(g)
                        % determine the Pickles class of each source and read each of the
                        % involved class spectra only once (there are few distinct classes)
                        Class = cell(NumSrcCh,1);
                        for Isrc = 1:1:NumSrcCh
                            R = astro.stars.tlogg2picklesClass(SpecPar(Isrc,1), SpecPar(Isrc,2)); % Teff and log(g)
                            Class{Isrc} = strcat(lower(R.class),lower(R.lumclass));
                        end
                        [UniqClass, ~, IndClass] = unique(Class);
                        for Icl = 1:1:numel(UniqClass)
                            PicklesFile = strcat(Args.PicklesDir,'uk',UniqClass{Icl},'.mat');
                            SPick = io.files.load2(PicklesFile);
                            SpecIn(IndClass == Icl,:) = repmat( interp1( SPick(:,1), SPick(:,2), Wave, 'linear', 0 ), ...
                                                                sum(IndClass == Icl), 1 );
                        end
                        
                    case 'phoenix'
                        
                        if Args.Verbosity > 0, fprintf('%s','generating Phoenix spectra for individual values of Teff and log(g) .. '); end
                        SpecPar = chunkSpecPar(Args.Spec, Range, NumSrc, NumSrcCh); % this chunk's Teff and log(g)
                        SpecIn  = phoenixSpectra(Phoenix, Wave, SpecPar);
                        
                    case 'tab'

                        if Args.Verbosity > 0, fprintf('%s','Reading source spectra from a table..'); end

                        if size( Args.Spec, 2) == NumSrc && size( Args.Spec, 1) == Nwave
                            % NumSrc spectra at the standard grid 2000:1:11000
                            for Isrc = 1:1:NumSrcCh

                                Isrc_gl = Isrc + ChL(ICh) - 1;   % global source number 

                                SpecIn(Isrc,:) = Args.Spec(:,Isrc_gl); % read the spectra from table columns

                            end
                        elseif size( Args.Spec, 2) == NumSrc + 1
                            % NumSrc spectra at a nonstandard grid, the wavelength
                            % grid is in the column number NumSrc + 1 
                            for Isrc = 1:1:NumSrcCh 

                                Isrc_gl = Isrc + ChL(ICh) - 1;   % global source number 

                                SpecIn(Isrc,:) = interp1(Args.Spec(:,NumSrc+1), Args.Spec(:,Isrc_gl), Wave, 'linear', 0);

                            end
                        else
                            error('Number of columns or rows in the spectral input table is incorrect, exiting..');
                        end

                    otherwise

                        error('Spectral parameters not properly defined in uSim, exiting..');
                end

            case 1  % read the table from an AstroSpec object and regrid it to Wave set of wavelengths

                if numel(Args.Spec) == 1   % one spectrum shared by all the sources
                    SpecIn = repmat( interp1( Args.Spec.Wave, Args.Spec.Flux, Wave, 'linear', 0 ), NumSrcCh, 1 );
                else
                    NumWav = arrayfun(@(S) numel(S.Wave), Args.Spec( Range ));
                    if any(NumWav ~= NumWav(1))
                        error('The AstroSpec spectra must all have the same number of wavelength points, exiting..');
                    end
                    Flx = cell2mat({Args.Spec( Range ).Flux});
                    Wav = cell2mat({Args.Spec( Range ).Wave});

                    for Isrc = 1:1:NumSrcCh
                        SpecIn(Isrc,:) = interp1( Wav(:,Isrc), Flx(:,Isrc), Wave, 'linear', 0 );
                    end
                end
                

        end
                                if Args.Verbosity > 0, fprintf('done\n'); end
                                T0 = reportTime(Args.Verbosity, T0);

                    
        %%%%%%%%%%%%%%%%%%%%%  rescale the spectra to the input magnitudes and account for the extinction: 

                                if Args.Verbosity > 0, fprintf('%s%s%s%s%s','Rescaling spectra to fit the input ',...
                                         FiltFam{1},'/',Filter{1},' magnitudes...'); end

        

        % synthetic photometry, one call per filter: the radial-zone filter of each source
        % for the ULTRASAT family, otherwise the named family/filter of each source
        if UseZoneFilt
            MagSc = synPhotByFilter(Wave, SpecIn, IndR, @(Iz) UP.U_AstFilt(Iz), 'R1');
        else
            [~, ~, IndF] = unique( strcat(FiltFam(Range), '|', Filter(Range)) );
            Ifirst = Range(1) - 1;
            MagSc  = synPhotByFilter(Wave, SpecIn, IndF, ...
                        @(If) FiltFam{Ifirst + find(IndF == If, 1)}, @(If) Filter{Ifirst + find(IndF == If, 1)});
        end
        Factor   = 10.^(-0.4.*(MagSc - InMag(Range))); % rescaling factor
        SpecIn   = SpecIn ./ Factor;  
        % account for the extinction:
        ExtMag   = astro.extinction.extinction(InEbv(Range)',(Wave./1e4)', [], Args.Rv, 'Model', Args.ExtLaw);
        Extinction = 10.^(-0.4.*ExtMag);
        SpecObs  = SpecIn .* Extinction';               % observed (extincted) spectrum
        % if requested produce the ULTRASAT magnitude for the source
        if Args.CalculateULTRASATMag
            MagU(Range) = synPhotByFilter(Wave, SpecObs, IndR, @(Iz) UP.U_AstFilt(Iz), 'R1');
        end

                                if Args.Verbosity > 0, fprintf('done\n'); end
                                T0 = reportTime(Args.Verbosity, T0);

        %%%%%%%%%%%%%%%%%%%%%  convolve the spectrum with the ULTRASAT throughut and
        %%%%%%%%%%%%%%%%%%%%%  fill the CatFlux column with the spectrum-intergated countrate fluxes

        % [ counts /s /bin ] = [ erg s(-1) cm(-2) A(-1) ] * [ counts / ph ] * [ A / bin ] * [ cm(2) ]/ [ erg / ph ]:
        SpecCts = SpecObs .* TotT .* DeltaLambda .* SAper ./ ( H*C ./(1e-8 .* Wave) );
        % the wavelength integrated source fluxes [ counts / s ]:
        CatFlux(Range) = sum(SpecCts,2);
        
                                if Args.Verbosity > 0, fprintf('Source spectra convolved with the throughput\n'); end

        %%%%%%%%%%%%%%%%%%%%%  integrate the throughput-convolved source spectra 
        %%%%%%%%%%%%%%%%%%%%%  S_i(λ)*Th(λ,r_i) with their PSF_i(λ,r_i) over the frequency range 
        %%%%%%%%%%%%%%%%%%%%%  and obtain spectrum-weighted PSF_i for each source

                                if Args.Verbosity > 0, fprintf('Weighting source PSFs with their spectra.. '); end

        WPSF = imUtil.psf.specWeight(SpecCts, RadTab, PSFdata, 'Rad', Rad, 'Method', 'slices', ...
                                     'Lambda',WavePSF,'SpecLam',Wave); 

                                if Args.Verbosity > 0, fprintf('done\n'); end
                                T0 = reportTime(Args.Verbosity, T0);
        %%%%%%%%%%%%%%%%%%%%% rotate the weighted source PSFs, blur them due to the S/C jitter 
        %%%%%%%%%%%%%%%%%%%%% and inject them into an empty tile image

                                if Args.Verbosity > 0, fprintf('Rotating the PSFs and injecting them into an empty image.. '); end
                                
        % a source with a non-finite flux (e.g. a spectrum that could not be built) is not
        % injected: a single NaN stamp fills a patch of the image with NaN, and slows the
        % injection of all the later sources many times over
        Good   = isfinite(CatFlux(Range));
        Dropped(Range) = ~Good;
        RangeG = Range(Good);

        CatX_ch    = CatX(RangeG);   % cut the appropriate parts of the Cat catalog
        CatY_ch    = CatY(RangeG);
        CatFlux_ch = CatFlux(RangeG);
        
        if numel(RotAngle) > 1 
            RotAngle_ch = RotAngle(RangeG);
        else
            RotAngle_ch = RotAngle;
        end
        
        % NB: injectArtSrc has been moved into this file from
        % imUtil.art.injectArtSrc for the time being, but 
        % this need to be rewritten with the new tools from imUtil.art

        if any(Good)
        [Image_ch, PSF_ch, Widths_ch] = injectArtSrc (CatX_ch, CatY_ch, CatFlux_ch, ImageSizeX, ImageSizeY,...
                                 WPSF(:,:,Good), 'PSFScaling', Args.ImRes, 'RotatePSF', RotAngle_ch,...
                                 'PerSourceRotation', numel(RotAngle) > 1, ...
                                 'Jitter', Args.Jitter, 'JitterPar', Args.JitterPar, ...
                                 'JitterExposure', Args.Exposure(2), 'AxisXY', [X0 Y0], ...
                                 'Method', Args.Inj, 'MeasurePSF', Args.MeasurePSF);
        if Args.MeasurePSF
            PSFWidths(RangeG,:) = [RadSrc(Good) Widths_ch];   %#ok<AGROW> preallocated above when MeasurePSF
        end

                                if Args.Verbosity > 0, fprintf(' done\n'); end
                                T0 = reportTime(Args.Verbosity, T0);
        %%%%%%%%%%%%%%%%%%%% add the chunk image to the summed source image
        %%%%%%%%%%%%%%%%%%%% and populate the PSF of the sources 
        
        ImageSrc = ImageSrc + Image_ch;  % add the image containing the newly added sources to the current summed image
        
        if KeepPSF
            PSF(:,:,RangeG) = PSF_ch;  %#ok<AGROW> fill in the resulting PSF array if the number of sources is not too large
        end
                                if Args.Verbosity > 0, fprintf('Partial source image added to the stacked source image \n'); end
                                
        if Args.CalculateCrudeSNR
            % NB: PSF_ch are the stamps actually injected into the image (rotated,
            % jitter-blurred), so the S/N below is measured on the very profile the
            % source has in the output -- not on a nominal one
            CrudeSNR(RangeG) = crudeSNR(PSF_ch, CatFlux(RangeG), Exposure, BackVar, ...
                                        Args.ImRes, Args.SNRMethod);
        end
        end % any(Good)
        
    end % end the loop over source chunks

    % remove the sources that were not injected
    if any(Dropped)
        warning('ultrasat:usim:NonFiniteFlux', ...
            '%d source(s) with a non-finite flux (e.g. no valid spectrum) dropped', nnz(Dropped));
        Keep     = ~Dropped;
        CatX     = CatX(Keep);
        CatY     = CatY(Keep);
        CatFlux  = CatFlux(Keep);
        InMag    = InMag(Keep);
        MagU     = MagU(Keep);
        CrudeSNR = CrudeSNR(Keep);
        RA       = RA(Keep);
        DEC      = DEC(Keep);
        if ~isempty(PSFWidths)
            PSFWidths = PSFWidths(Keep,:);
        end
        if KeepPSF && exist('PSF','var')
            PSF = PSF(:,:,find(Keep));   % NB: PSF may be shorter than Keep if the last sources were dropped
        end
        NumSrc = nnz(Keep);
    end

    else % Args.ExtProfileType is non-empty: simulate one or more extended objects, each with its own shared spectrum

        if isempty(Args.ExtRA0) || isempty(Args.ExtDec0) || isempty(Args.ExtMag) || ...
                isempty(Args.ExtSizeRA) || isempty(Args.ExtSizeDec)
            error('ExtRA0, ExtDec0, ExtMag, ExtSizeRA, and ExtSizeDec must all be specified in extended-object mode, exiting..');
        end

        NumExt = numel(Args.ExtRA0);
        if numel(RotAngle) ~= 1 && numel(RotAngle) ~= NumExt
            error('RotAng must hold 1 or NumExt (=%d) angles in extended-object mode, exiting..', NumExt);
        end
        if numel(Args.ExtDec0) ~= NumExt || numel(Args.ExtMag) ~= NumExt || ...
                numel(Args.ExtSizeRA) ~= NumExt || numel(Args.ExtSizeDec) ~= NumExt
            error('ExtRA0, ExtDec0, ExtMag, ExtSizeRA, and ExtSizeDec must all have the same number of elements, exiting..');
        end

        if Args.Verbosity > 0, cprintf('hyper','%s%s%s%d%s\n','Simulating extended object(s), profile: ',Args.ExtProfileType,', N = ',NumExt,''); end

        % per-object parameters that may also be given as a single value to broadcast to all objects
        ExtEbv       = extBroadcast(Args.ExtEbv,       NumExt, 'ExtEbv');
        ExtAxisRatio = extBroadcast(Args.ExtAxisRatio, NumExt, 'ExtAxisRatio');
        ExtPA        = extBroadcast(Args.ExtPA,        NumExt, 'ExtPA');

        % a single [Re n k] / [SigmaX SigmaY Rho] row broadcasts to all objects
        if size(Args.ExtProfilePar,1) == 1 && NumExt > 1
            ExtProfilePar = repmat(Args.ExtProfilePar, NumExt, 1);
        else
            ExtProfilePar = Args.ExtProfilePar;
        end
        if ~any(strcmpi(Args.ExtProfileType,{'flat','matrix'})) && size(ExtProfilePar,1) ~= NumExt
            error('ExtProfilePar must have NumExt (=%d) rows, or a single row to broadcast to all objects, exiting..', NumExt);
        end

        % normalize ExtProfileMatrix into a 1 x NumExt cell array
        if strcmpi(Args.ExtProfileType,'matrix')
            if isempty(Args.ExtProfileMatrix)
                error('ExtProfileMatrix must be provided when ExtProfileType = ''matrix'', exiting..');
            elseif ~iscell(Args.ExtProfileMatrix)
                if NumExt > 1
                    error('ExtProfileMatrix must be a 1 x NumExt cell array when simulating more than one object, exiting..');
                end
                ExtProfileMatrix = {Args.ExtProfileMatrix};
            elseif numel(Args.ExtProfileMatrix) ~= NumExt
                error('ExtProfileMatrix must contain NumExt (=%d) matrices, exiting..', NumExt);
            else
                ExtProfileMatrix = Args.ExtProfileMatrix;
            end
        end

        CatX     = zeros(NumExt,1);
        CatY     = zeros(NumExt,1);
        RA       = zeros(NumExt,1);
        DEC      = zeros(NumExt,1);
        InMag    = Args.ExtMag(:);
        CatFlux  = zeros(NumExt,1);
        MagU     = NaN(NumExt,1);
        CrudeSNR = NaN(NumExt,1);
        ConvStampCell = cell(1,NumExt);
        if strcmpi(Args.ExtSpecType,'phoenix') && ~isa(Args.ExtSpec,'AstroSpec')
            Phoenix = io.files.load2(Args.Phoenix);   % the Phoenix spectral grid, read once for all the objects
        end
        WPSFRotCell   = cell(1,NumExt);
        InFOVExt = true(NumExt,1);

        for Iext = 1:1:NumExt

            if Args.ExtSkyCat   % the input coordinates are sky coordinates
                [CatX(Iext), CatY(Iext)] = SimWCS.sky2xy(Args.ExtRA0(Iext), Args.ExtDec0(Iext));
                RA(Iext)  = Args.ExtRA0(Iext);
                DEC(Iext) = Args.ExtDec0(Iext);
            else                 % the input coordinates are pixel coordinates
                CatX(Iext) = Args.ExtRA0(Iext);
                CatY(Iext) = Args.ExtDec0(Iext);
                [RA(Iext), DEC(Iext)] = SimWCS.xy2sky(CatX(Iext), CatY(Iext));
            end

            if (CatX(Iext) < 0.5) || (CatY(Iext) < 0.5) || (CatX(Iext) >= ImageSizeX+0.5) || (CatY(Iext) >= ImageSizeY+0.5)
                warning('ultrasat:usim:ExtOutOfFOV', ...
                    'Extended object #%d center falls out of the tile FOV, skipping it..', Iext);
                InFOVExt(Iext) = false;
                continue
            end

            % warn if the object is large enough that the field-position dependence of the PSF
            % across its own extent may no longer be negligible (a single PSF kernel is used regardless)
            ExtSizeDeg = max(Args.ExtSizeRA(Iext), Args.ExtSizeDec(Iext)) / 3600;
            if ExtSizeDeg > 1
                warning('ultrasat:usim:ExtLargeObject', ...
                    ['Extended object #%d size (%.2f deg) exceeds 1 deg. A single PSF kernel is used\n', ...
                     'for the whole object, but the ULTRASAT PSF varies over the field on this scale.'], Iext, ExtSizeDeg);
            end

            %%%%%%%%%%%%%%%%%%%%% radial distance of the object center from the inner corner of the tile,
            %%%%%%%%%%%%%%%%%%%%% and the throughput there

            RadSrc = sqrt( ( CatX(Iext) - X0 ).^2 + ( CatY(Iext) - Y0 ).^2 ) .* PixSizeDeg;     % [deg]
            RadTab = min(RadSrc, Rad(end));     % the tables end at 10 deg, the tile corner is at 10.13 deg
            TotT   = interpn(UP.wavelength, Rad', UP.TotT, Wave', RadTab', 'linear', Tiny)';
            [~, IndR] = min( abs(RadTab - Rad) );

            %%%%%%%%%%%%%%%%%%%%% read or generate this object's spectrum

                                    if Args.Verbosity > 0, fprintf('Reading the spectrum of extended object #%d/%d.. ', Iext, NumExt); end

            if isa(Args.ExtSpec,'AstroSpec')
                Ispec = min(Iext, numel(Args.ExtSpec)); % a single spectrum object broadcasts to all objects
                SpecIn = interp1( Args.ExtSpec(Ispec).Wave, Args.ExtSpec(Ispec).Flux, Wave, 'linear', 0 );
            else
                switch lower(Args.ExtSpecType)
                    case 'bb'
                        SpecIn = AstroSpec.blackBody(Wave', extSpecRow(Args.ExtSpec,Iext,NumExt,1)).Flux'; % erg s(-1) cm(-2) A(-1)
                    case 'pl'
                        SpecIn = Wave .^ extSpecRow(Args.ExtSpec,Iext,NumExt,1);                           % erg s(-1) cm(-2) A(-1)
                    case 'pickles'
                        Par = extSpecRow(Args.ExtSpec,Iext,NumExt,2);
                        R = astro.stars.tlogg2picklesClass(Par(1), Par(2)); % Teff and log(g)
                        PicklesFile = strcat(Args.PicklesDir,'uk',lower(R.class),lower(R.lumclass),'.mat');
                        SPick = io.files.load2(PicklesFile);
                        SpecIn = interp1( SPick(:,1), SPick(:,2), Wave, 'linear', 0 );
                    case 'phoenix'
                        Par = extSpecRow(Args.ExtSpec,Iext,NumExt,2);
                        SpecIn = phoenixSpectra(Phoenix, Wave, Par);
                    case 'tab'
                        % Args.ExtSpec: Nwave x NumExt (one flux column per object, at the standard Wave grid),
                        % or Nwave x (NumExt+1) with a shared custom wavelength grid in the last column
                        if size(Args.ExtSpec,2) == NumExt && size(Args.ExtSpec,1) == Nwave
                            SpecIn = Args.ExtSpec(:,Iext)';
                        elseif size(Args.ExtSpec,2) == NumExt + 1
                            SpecIn = interp1( Args.ExtSpec(:,NumExt+1), Args.ExtSpec(:,Iext), Wave, 'linear', 0 );
                        else
                            error('Number of columns or rows in the extended-object spectral input table (ExtSpec) is incorrect, exiting..');
                        end
                    otherwise
                        error('Extended object spectral type not recognized, exiting..');
                end
            end
            SpecIn = reshape(SpecIn, 1, Nwave);

                                    if Args.Verbosity > 0, fprintf('done\n'); end

            %%%%%%%%%%%%%%%%%%%%% rescale the spectrum to the requested magnitude and account for extinction

            if strcmpi(Args.ExtFiltFam,'ULTRASAT')
                MagSc = astro.spec.synthetic_phot([Wave' SpecIn'],UP.U_AstFilt(IndR),'R1','AB');
            else
                MagSc = astro.spec.synthetic_phot([Wave' SpecIn'],Args.ExtFiltFam,Args.ExtFilt,'AB');
            end
            Factor  = 10.^(-0.4.*(MagSc - Args.ExtMag(Iext)));
            SpecIn  = SpecIn ./ Factor;

            ExtMagAtt  = astro.extinction.extinction(ExtEbv(Iext), (Wave./1e4)', [], Args.Rv, 'Model', Args.ExtLaw);
            Extinction = 10.^(-0.4.*ExtMagAtt);
            SpecObs    = SpecIn .* Extinction';           % observed (extincted) spectrum

            if Args.CalculateULTRASATMag
                MagU(Iext) = astro.spec.synthetic_phot([Wave' SpecObs'],UP.U_AstFilt(IndR),'R1','AB');
            end

            %%%%%%%%%%%%%%%%%%%%% convolve the spectrum with the throughput and get the total object count rate

            SpecCts = SpecObs .* TotT .* DeltaLambda .* SAper ./ ( H*C ./(1e-8 .* Wave) ); % [ counts /s /bin ]
            CatFlux(Iext) = sum(SpecCts, 2);                                               % [ counts /s ]
            if ~isfinite(CatFlux(Iext))
                warning('ultrasat:usim:NonFiniteFlux', ...
                    'Extended object #%d has a non-finite flux (e.g. no valid spectrum), skipping it..', Iext);
                InFOVExt(Iext) = false;
            end

            %%%%%%%%%%%%%%%%%%%%% build this object's spectrum-weighted PSF kernel and rotate it
            %%%%%%%%%%%%%%%%%%%%% to the tile's PSF axis, exactly as done for point sources

            WPSF = imUtil.psf.specWeight(SpecCts, RadTab, PSFdata, 'Rad', Rad, 'Method', 'slices', ...
                                         'Lambda',WavePSF,'SpecLam',Wave);
            WPSF = WPSF(:,:,1);

            RotAngle1 = RotAngle(min(Iext, numel(RotAngle)));   % one angle per object, or one for all
            if abs(RotAngle1) < 1 || abs(RotAngle1 - 360) < 1
                WPSFRot = WPSF;
            else
                WPSFRot = imrotate(WPSF, RotAngle1, 'bilinear', 'loose');
                WPSFRot = WPSFRot ./ sum(WPSFRot, 'all');
            end
            if Args.Jitter   % blur the kernel with the S/C jitter at the object centre
                WPSFRot = ultrasat.jitter(WPSFRot, [CatX(Iext)-X0 CatY(Iext)-Y0], ...
                                          'Exposure', Args.Exposure(2), 'SigmaX0', Args.JitterPar(1), ...
                                          'SigmaY0', Args.JitterPar(2), 'Rotation', Args.JitterPar(3), ...
                                          'Scaling', Args.ImRes);
            end

            %%%%%%%%%%%%%%%%%%%%% build the spatial surface-brightness profile, on the same
            %%%%%%%%%%%%%%%%%%%%% oversampled grid as the PSF kernel above

            if isempty(Args.ExtOversampling)
                ExtOversampling = Args.ImRes;
            else
                ExtOversampling = Args.ExtOversampling;
            end

            Grain = PixSizeDeg * 3600 / ExtOversampling;              % [arcsec] per profile grid cell
            Nx    = max(3, ceil( Args.ExtSizeRA(Iext)  / Grain ));
            Ny    = max(3, ceil( Args.ExtSizeDec(Iext) / Grain ));
            % imUtil.art.addSources requires odd-sized stamps; round up to the next odd
            % value here so the final convolved stamp (ConvStamp below) is never left even
            Nx    = Nx + 1 - mod(Nx, 2);
            Ny    = Ny + 1 - mod(Ny, 2);

            switch lower(Args.ExtProfileType)
                case 'sersic'
                    Profile = imUtil.kernel2.sersic(ExtProfilePar(Iext,:), [Nx Ny]);
                case 'gaussian'
                    Profile = imUtil.kernel2.gauss(ExtProfilePar(Iext,:), [Nx Ny]);
                case 'flat'
                    Profile = ones(Ny, Nx);
                case 'matrix'
                    Profile = ExtProfileMatrix{Iext};
                    if ~isequal(size(Profile),[Ny Nx])
                        Profile = imresize(Profile, [Ny Nx], 'bilinear');
                    end
                otherwise
                    error('Unsupported ExtProfileType, exiting..');
            end

            % apply the object's axis ratio and position angle, uniformly for any ExtProfileType
            % (a stretch + rotate approximation of an elliptical profile, since imUtil.kernel2.* are circular-only)
            if ExtAxisRatio(Iext) ~= 1 || mod(ExtPA(Iext),360) ~= 0
                Profile = imresize(Profile, [size(Profile,1), max(3,round(size(Profile,2)*ExtAxisRatio(Iext)))], 'bilinear');
                Profile = imrotate(Profile, ExtPA(Iext), 'bilinear', 'loose');
            end

            Profile = Profile ./ sum(Profile, 'all'); % normalize to unit flux

            %%%%%%%%%%%%%%%%%%%%% convolve the profile with the rotated PSF kernel

            % conv2_fft's internal recentering offset, Sh1 = floor(min(size(Mat1),
            % size(Mat2))*0.5), is only correct when Mat2 (the kernel) is at its own
            % natural, un-padded size and is genuinely the smaller of the two -- if
            % Mat1 and Mat2 end up the same size (e.g. from padding the smaller one up
            % to match the larger), Sh1 degenerates into half of the WHOLE canvas
            % instead of half the kernel, and the result lands near a corner. Profile is
            % usually larger than WPSFRot for extended sources, but WPSFRot (the PSF
            % stamp) is often the larger one for compact sources -- e.g. Template A/B in
            % ultrasat.ELOPsim's small circles vs. a broad off-axis PSF -- so always
            % orient the call with the genuinely larger side as Mat1 (natural size) and
            % the genuinely smaller side as Mat2 (natural size, never padded).
            % Convolution is commutative, so swapping which one is Mat1 only changes the
            % output canvas size, not the result's content.
            if all(size(Profile) >= size(WPSFRot))
                Mat1 = Profile; Mat2 = WPSFRot;
            elseif all(size(WPSFRot) >= size(Profile))
                Mat1 = WPSFRot; Mat2 = Profile;
            else
                % neither dominates in both dimensions (a rare mixed aspect-ratio case
                % for our profile/PSF shapes) -- pad each up to the elementwise max as
                % before; conv2_fft's Sh1 offset is not exactly right in this fallback
                % (same known limitation, now confined to this edge case only)
                SzC  = max(size(Profile), size(WPSFRot));
                Mat1 = padarray(Profile, SzC-size(Profile), 0, 'post');
                Mat2 = padarray(WPSFRot, SzC-size(WPSFRot), 0, 'post');
            end

            % conv2_fft returns a complex-typed array via ifft2 (a floating-point-noise
            % imaginary part, since it never discards it); take the real part, since the
            % convolution of two real inputs is mathematically real.
            ConvStamp = real(imUtil.filter.conv2_fft(Mat1, Mat2));
            ConvStamp = ConvStamp ./ sum(ConvStamp, 'all'); % renormalize to unit flux

            ConvStampCell{Iext} = ConvStamp;
            WPSFRotCell{Iext}   = WPSFRot;

            % crude SNR estimate, using the actual (profile-convolved) object image
            if Args.CalculateCrudeSNR
                CrudeSNR(Iext) = crudeSNR(ConvStamp, CatFlux(Iext), Exposure, BackVar, ...
                                          Args.ImRes, Args.SNRMethod);
            end

        end % end the loop over extended objects

        % drop objects that fell outside the tile FOV
        CatX          = CatX(InFOVExt);
        CatY          = CatY(InFOVExt);
        RA            = RA(InFOVExt);
        DEC           = DEC(InFOVExt);
        InMag         = InMag(InFOVExt);
        CatFlux       = CatFlux(InFOVExt);
        MagU          = MagU(InFOVExt);
        CrudeSNR      = CrudeSNR(InFOVExt);
        ConvStampCell = ConvStampCell(InFOVExt);
        WPSFRotCell   = WPSFRotCell(InFOVExt);

        NumSrc  = numel(CatX);
        KeepPSF = NumSrc < Args.MaxPSFNum;

        %%%%%%%%%%%%%%%%%%%%% resample the convolved stamps down to the detector pixel scale, sub-pixel
        %%%%%%%%%%%%%%%%%%%%% center them on each object's position, flux-scale, and inject them all at once

        ImageSrc = zeros(ImageSizeX, ImageSizeY, 'single');

        if NumSrc > 0
                                    if Args.Verbosity > 0, fprintf('Injecting %d extended object(s) into an empty image.. ', NumSrc); end

            [CubePSF, XYInj] = imUtil.art.createSourceCube(ConvStampCell, [CatX CatY], CatFlux, ...
                                    'Oversample', Args.ImRes, 'Recenter', true, ...
                                    'RecenterMethod', 'lanczos');

            ImageSrc = imUtil.art.addSources(ImageSrc, CubePSF, XYInj);

                                    if Args.Verbosity > 0, fprintf('done\n'); end
        end

        % store the (unscaled) per-object spectrum-weighted PSF kernels, for consistency with the point-source output
        PSF = WPSFRotCell;

        if Args.MeasurePSF
            warning('ultrasat:usim:MeasurePSFExt', ...
                'Args.MeasurePSF is implemented for point sources only; ignored for extended objects..');
        end

    end

    % PSF width diagnostics (Args.MeasurePSF), in arcsec
    if ~isempty(PSFWidths)
        PixSizeArcsec = PixSizeDeg .* 3600;
        PSFWidths = table(CatX, CatY, PSFWidths(:,1), PSFWidths(:,2).*PixSizeArcsec, ...
                          PSFWidths(:,3).*PixSizeArcsec, ...
                          'VariableNames', {'X','Y','FieldRadiusDeg','R50arcsec','FWHMarcsec'});
        fprintf(['PSF widths (N = %d): R50 median %.2f" [%.2f-%.2f], ', ...
                 'FWHM median %.2f" [%.2f-%.2f]\n'], height(PSFWidths), ...
                median(PSFWidths.R50arcsec),  min(PSFWidths.R50arcsec),  max(PSFWidths.R50arcsec), ...
                median(PSFWidths.FWHMarcsec), min(PSFWidths.FWHMarcsec), max(PSFWidths.FWHMarcsec));
    end

    if Args.SNROnly
        % CatX/CatY/CatFlux/InMag/MagU/CrudeSNR/RA/DEC are already finalized above (for
        % both the point-source and extended-object branches); build the same CatData
        % catalog a normal run would attach to its output, and return immediately,
        % skipping the noise/ADU pipeline and file writing entirely
        if Args.Verbosity > 0
            fprintf('SNROnly = true: returning CrudeSNR only, usimImage.Image is empty, no files written..\n');
        end
        Cat = [CatX CatY CatFlux InMag MagU CrudeSNR RA DEC];
        OutCat = AstroCatalog({Cat},'ColNames',{'X', 'Y', 'Counts/s', 'InMAG', 'MagU', 'SNR', 'RA','Dec'}, 'HDU', 1);
        usimImage = AstroImage();
        usimImage.Image = [];
        usimImage.CatData = OutCat;
        if ~isempty(PSFWidths)
            usimImage.UserData.PSFWidths = PSFWidths;
        end
        if KeepPSF && NumSrc > 0
            AP = psfObject(PSF, Args.ImRes);
            usimImage.PSFData = AP;
        end
        return
    end

    %%%%%%%%%%%%%%%%%%%%% add and apply various types of noise to the tile image
    %%%%%%%%%%%%%%%%%%%%% NB: while ImageSrc is in [counts/s], ImageSrcNoise is already in [counts] !!
    
                                if Args.Verbosity > 0, cprintf('hyper','Adding noise .. '); end
                    
        
    % the flat field scales the expected photon counts (sources, zodiacal, Cherenkov and
    % stray light), as does the vignetting; the internal arrays are (X, Y), the transpose
    % of the image orientation
    if isempty(Args.FlatMatrix)
        Flat = 1;
    else
        Flat = single(Args.FlatMatrix.');
    end
    SkyPhot   = BackPhot .* VigBkg;                                                      % the vignetted photon background
    BackMean  = (SkyPhot .* Flat + BackDark) .* ones(ImageSizeX,ImageSizeY,'single');   % the expected background
    ExpCounts = (ImageSrc .* Exposure) .* Flat .* VigSrc + BackMean;                     % the expected counts
    % stamp resampling/recentering interpolation (bilinear/Lanczos) can ring slightly negative
    % near sharp profile gradients; ExpCounts itself is kept as-is (its negative dips are a
    % real, flux-conserving part of the ringing), but the noise draw and the variance need a
    % non-negative expected count
    ImageVar  = max(ExpCounts,0) + VarRead;                                              % the expected variance

    if Args.NoisePoisson
        ImageSrcNoise = drawCounts(max(ExpCounts,0), SkyPhot.*min(Flat,[],'all') + BackDark, ...
                                   Args.PoissonThreshold);
        if VarRead > 0   % read-out + cross-talk noise, zero mean
            ImageSrcNoise = ImageSrcNoise + sqrt(VarRead) .* randn(ImageSizeX, ImageSizeY, 'single');
        end
    else
        ImageSrcNoise = ExpCounts;
    end
                                         
                            if Args.Verbosity > 0, fprintf(' done\n'); end
                            T0 = reportTime(Args.Verbosity, T0);
                            
    %%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%  add CR streaks
    
    if Args.AddCRStreaks
        if Args.Exposure(1) == 1
            CRProb = 1e-2;
            CRAmplitude = FullWell; % is it correct???
            ImageCR = CRAmplitude .* ( rand(ImageSizeX, ImageSizeY) < CRProb );
            ImageSrcNoise = ImageSrcNoise + ImageCR;
        else
            if Args.Verbosity > 0, fprintf('NOTE: CR streaks are not included once multiple exposures are modelled..\n'); end
        end
    end    

    %%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%  the low-count detector non-linearity, applied to the charge of
    %%%%%%%%%%%%%%%%%%%%%%  one frame (the image is a sum of Exposure(1) frames)

    if isa(Args.LowCountNonLin,'function_handle')
        ImageSrcNoise = Args.Exposure(1) .* ...
            Args.LowCountNonLin(ImageSrcNoise ./ Args.Exposure(1), Args.LowCountNonLinPar);
    end
        
    %%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%  cut the saturated pixels (to be refined later) 
    
    Thresh        = FullWell * Args.Exposure(1);
    AboveThresh   = sum(ImageSrcNoise(:) > Thresh);
    ImageSrcNoise = min(ImageSrcNoise, Thresh);
    
                            if Args.Verbosity > 0, fprintf('%d saturated pixels cutted\n',AboveThresh); end
    %%%%%%%%%%%%%%%%%%%%%%  make an ADU and mask ADU images (for a single
    %%%%%%%%%%%%%%%%%%%%%%  exposure observation only)
    
    if Args.Exposure(1) == 1
        ImageSrcNoiseGainMask = ImageSrcNoise > GainThresh;  % if the signal is above the threshold, use low gain
        % ultrasat.e2ADU documents its valid input domain as [1, 1e8] (it takes log10 of the
        % count to get an exponent); Gaussian-approximated noise can fluctuate to <=0 at low
        % counts, which would otherwise silently go complex (log10 of a negative number) or
        % -Inf (log10 of 0) -- clamp to the documented floor before calling it.
        % The ADU images are transposed to the orientation of the output AstroImage
        % e- to the ADU scale with the gain of each pixel (LowGain = 1 for the low gain)
        ToGain = @(LowGain) max(ImageSrcNoise .* ( LowGain .* E2ADUlow + ...
                                (ones(ImageSizeX,ImageSizeY,'single')-LowGain) .* E2ADUhigh ), 1);
        switch lower(Args.ADUOutput)
            case 'raw'          % all pixels in each gain, the gain flag set accordingly
                LowGain = true(ImageSizeX,ImageSizeY);
                ImageSrcNoiseADU = cat(3, ultrasat.e2ADU(ToGain(~LowGain), ~LowGain)', ...
                                          ultrasat.e2ADU(ToGain(LowGain),   LowGain)');
            case 'production'   % 13-bit ADU without the gain bit + a separate gain map
                ImageSrcNoiseADU = cat(3, ultrasat.e2ADU(ToGain(ImageSrcNoiseGainMask), ImageSrcNoiseGainMask, false)', ...
                                          int16(ImageSrcNoiseGainMask)');
            otherwise           % 14-bit ADU, the gain chosen per pixel by the threshold in the top bit
                ImageSrcNoiseADU = ultrasat.e2ADU(ToGain(ImageSrcNoiseGainMask), ImageSrcNoiseGainMask)';
        end
    else
        if Args.Verbosity > 0, fprintf('NOTE: the ADU image is not produced once multiple exposures are modelled..\n'); end
    end

    %%%%%%%%%%%%%%%%%%%%%%  output: a) an AstroImage object with filled image, header, and PSF properties  
    %%%%%%%%%%%%%%%%%%%%%%          b) a FITS image c) a native (RAW) format image 
    
                            if Args.Verbosity > 0, fprintf('Compiling output structures and writing files..\n'); drawnow('update'); end
                            T0 = tic;
    
    % compile a catalog table
    Cat = [CatX CatY CatFlux InMag MagU CrudeSNR RA DEC];
    
    OutCat = AstroCatalog({Cat},'ColNames',{'X', 'Y', 'Counts/s', 'InMAG', 'MagU', 'SNR', 'RA','Dec'}, 'HDU', 1);
        
    % make an AstroImage (note, the images are to be transposed!)
    usimImage = AstroImage( {ImageSrcNoise'} ,'Back', {BackMean'}, 'Var', {ImageVar'});
    usimImage.CatData = OutCat;
    if ~isempty(PSFWidths)
        usimImage.UserData.PSFWidths = PSFWidths;
    end

    % save the final source PSFs into an AstroPSF array and attach it to
    % the resulting AstroImage object, if the source number is not too large
    if KeepPSF && NumSrc > 0
        AP = psfObject(PSF, Args.ImRes);
        usimImage.PSFData = AP;
    else
        if Args.Verbosity > 0, fprintf('NOTE: the number of input sources is too large to record each of their PSFs..\n'); end
    end
    
    % add the WCS data to the AstroImage object:
    usimImage.WCS = SimWCS;

    AH = usimImage.WCS.wcs2header;       % make a header from the WCS
    usimImage.HeaderData.Data = AH.Data; % add the header data to the AstroImage

    % add some more keywords and values to the image header:
    DateObs = Args.DateObs;   % NB: by default not a real observation time, a placeholder
    usimImage.setKeyVal('EXPTIME',Exposure);
    usimImage.setKeyVal('DATEOBS',DateObs);

    % additional header keywords, matching the real camera's own FITS header (e.g.
    % 20251112_131706_pixel_injection_A_LOW.fits) where derivable from usim.m's own
    % simulation parameters; keywords with no simulated equivalent (camera hardware IDs,
    % raw timestamps, readout-order/mosaic-placement metadata) are still added, with a
    % placeholder value and a comment flagging them as not yet computed, so downstream
    % code can already rely on the keyword's presence.
    % celestial.time.date2jd (via str2date) needs 'yyyy-mm-dd HH:MM:SS.FFF' (a space, not
    % 'T', and an explicit fractional-seconds part), not DateObs' ISO-8601 'T' format
    MjdBeg  = celestial.time.date2jd([strrep(DateObs,'T',' ') '.000'], 'MJD');
    MjdEnd  = MjdBeg + Exposure/86400;
    NowIso  = celestial.time.get_atime([],0,0).ISO;

    HeaderRows = { ...
        'DATE',     NowIso,       'HDU create date/time (ISO-8601, UTC)'; ...
        'TIMESYS',  'UTC',        'time scale of the time-related keywords'; ...
        'TIMEUNIT', 's',          'time unit of the time-related keywords'; ...
        'XPOSURE',  Exposure,     'exposure duration [s]'; ...
        'TELAPSE',  Exposure,     'time elapsed between observation start and end [s] (assumes no inter-exposure dead time, not modeled)'; ...
        'MJD-BEG',  MjdBeg,       'start time of data acquisition'; ...
        'MJD-END',  MjdEnd,       'end time of data acquisition'; ...
        'MJDREF',   0,            'reference time'; ...
        'BUNIT',    'electron',   'physical unit of the image data'; ...
        'SEGMENT',  Args.Tile,    'camera segment'; ...
        'SEGFRAME', 'sensor',     'coordinate frame of the image (mosaic/sensor)'; ...
        'PIXSIZE',  PixelSizeMm,  'width of a pixel (mm)'; ...
        'RAW',      'F',          'dummy rows and columns are included (not modeled in simulation)'; ...
        'GCHANNEL', '',           'gain channel mode: LOW/HIGH/THRESH/BOTH (n/a, this image is not gain/ADU-encoded)'; ...
        'GTHRESH',  '',           'gain threshold used for THRESH mode (n/a, this image is not gain/ADU-encoded)'; ...
        'INDEX',    0,            'sequential image index (not computed for a single simulated frame)'; ...
        'TIMECAM',  0,            'integer timestamp from camera (no simulated equivalent)'; ...
        'UUIDIMG',  '',           'unique image ID (no simulated equivalent)'; ...
        'ID-SEG',   '',           'camera segment ID (no simulated equivalent)'; ...
        'VER-GSS',  '',           'GSS version (no simulated equivalent)'; ...
        'SEGPOSX',  '',           'mosaic X position of this segment (not yet computed)'; ...
        'SEGPOSY',  '',           'mosaic Y position of this segment (not yet computed)'; ...
        'SEGPOSL',  '',           'left edge, mosaic coordinate (not yet computed)'; ...
        'SEGPOSR',  '',           'right edge, mosaic coordinate (not yet computed)'; ...
        'SEGPOSB',  '',           'bottom edge, mosaic coordinate (not yet computed)'; ...
        'SEGPOST',  '',           'top edge, mosaic coordinate (not yet computed)'; ...
        'PIXORIG',  '',           'location of the first readout pixel (not yet computed)'; ...
        'ROWDIR',   '',           'direction of the first readout row (not yet computed)'; ...
        'STARTROW', '',           'start readout row index (not yet computed)'; ...
        'ENDROW',   '',           'end readout row index (not yet computed)'; ...
        'STARTCOL', '',           'start readout column index (not yet computed)'; ...
        'ENDCOL',   '',           'end readout column index (not yet computed)'; ...
        };
    % the simulated background and detector model
    if isa(Args.LowCountNonLin,'function_handle')
        NonLinName = func2str(Args.LowCountNonLin);
    else
        NonLinName = 'none';
    end
    SimRows = { ...
        'ZODIMODE', ternaryStr(isnumeric(Args.Zodi),'const','model'), 'zodiacal light: given rate or ultrasat.zodiac_bck'; ...
        'ZODIRATE', Args.NoiseZody  * ZodiRate,       'zodiacal light rate [e-/pix/s]'; ...
        'CHERRATE', Args.NoiseCher  * Args.Cherenkov, 'Cherenkov background rate [e-/pix/s]'; ...
        'STRAYRAT', Args.NoiseStray * Args.Stray,     'stray light rate [e-/pix/s]'; ...
        'DARKRATE', Args.NoiseDark  * Args.DarkCurrent, 'dark current rate [e-/pix/s]'; ...
        'RDNOISE',  Args.ReadNoise,                   'read-out noise per frame [e-/pix]'; ...
        'XTALKVAR', Args.NoiseCross * Args.CrossTalk, 'cross-talk noise variance per frame [e-^2/pix]'; ...
        'NONLIN',   NonLinName,                       'low-count non-linearity function'; ...
        'NONLINPA', mat2str(Args.LowCountNonLinPar),  'low-count non-linearity parameters'; ...
        'VIGNET',   Args.Vignetting,                  'vignetting: none or the obscuration map'; ...
        'EXTLAW',   upper(Args.ExtLaw),               'extinction law applied to the sources'; ...
        'EXTRV',    Args.Rv,                          'R_V of the extinction law'; ...
        };
    if ~isempty(ZodiMagV)
        SimRows(end+1,:) = {'ZODIMAGV', ZodiMagV, 'zodiacal V surface brightness at tile centre [mag/arcsec^2]'};
    end
    if ~isempty(Args.Seed)
        SimRows(end+1,:) = {'SIMSEED', mat2str(Args.Seed), 'random number generator seed'};
    end
    usimImage.HeaderData.insertKey([HeaderRows; SimRows]);

    % write the image to a FITS file 
    % if you do not wish to write any files, use 'OutType','none'
    if strcmp( Args.OutType,'FITS') || strcmp( Args.OutType,'all')

        % save the AstroImage object in a .mat file for a future usage (if requested):
        if Args.SaveMatFile 
            OutObjName = fullfile(Args.OutDir, sprintf('%s%s%s%s',Args.OutName,'_tile',Args.Tile,'.mat'));   
            save(OutObjName,'usimImage','-v7.3');
        end
                
        OutFITSName = sprintf('%s%s%s%s%s%s%s','!',Args.OutDir,'/',Args.OutName,'_tile',Args.Tile,'.fits');
        FITS.write(usimImage.Image, OutFITSName, 'Header',usimImage.HeaderData.Data,...
                    'DataType','single', 'Append',false,'OverWrite',true,'WriteTime',true);

        if Args.Exposure(1) == 1 % the ADU image is put out only when we model 1 exposure
            % a copy of the main image header, with the keywords that describe this
            % file's own per-pixel gain-selected ADU encoding (n/a on the main,
            % un-gain-encoded counts image) filled in for real
            OutRoot = sprintf('!%s/%s_tile%s', Args.OutDir, Args.OutName, Args.Tile);
            switch lower(Args.ADUOutput)
                case 'raw'
                    Suffix = {'_ADU_HI', '_ADU_LO'};
                    Vals   = {{'adu','HIGH',''}, {'adu','LOW',''}};
                    Type   = {'int16', 'int16'};
                case 'production'
                    Suffix = {'_ADU', '_GAIN'};
                    Vals   = {{'adu','THRESHOLD',GainThresh}, {'','THRESHOLD',GainThresh}};
                    Type   = {'int16', 'int8'};      % 'int8' is written as BITPIX 8 (unsigned bytes)
                otherwise
                    Suffix = {'_ADU'};
                    Vals   = {{'adu','THRESHOLD',GainThresh}};
                    Type   = {'int16'};
            end
            for Ip = 1:numel(Suffix)
                AduHeader = usimImage.HeaderData.copy();
                AduHeader.replaceVal({'BUNIT','GCHANNEL','GTHRESH'}, Vals{Ip});
                FITS.write(ImageSrcNoiseADU(:,:,Ip), [OutRoot Suffix{Ip} '.fits'], 'Header',AduHeader.Data, ...
                           'DataType',Type{Ip}, 'Append',false,'OverWrite',true,'WriteTime',true);
            end
        end
               
        % make a text file with the input catalog:
        OutTxtName = sprintf('%s%s%s%s%s%s',Args.OutDir,'/',Args.OutName,'_tile',Args.Tile,'_InCat.txt'); 
        fileID = fopen(OutTxtName,'w'); 
        fprintf(fileID,'# DetX DetY CPS InMag MagU CrudeSNR RA DEC\n');
        fprintf(fileID,'%7.1f %7.1f %5.2f %.2f %.2f %.2f %.4f %.4f\n',Cat');
        fclose(fileID);
        
        % an accompanying region file: 
        OutRegName  = sprintf('%s%s%s%s%s%s',Args.OutDir,'/',Args.OutName,'_tile',Args.Tile,'.reg');
        DS9_new.regionWrite([CatX CatY],'FileName',OutRegName,'Color','blue','Marker','b','Size',1,'Width',4,...
                            'Precision','%.2f','PrintIndividualProp',0); 
        
        % optional region files for various parts of the source distribution (these can be also extracted later from *InCat.txt):
        if Args.SaveRegionsBySourceMag
            idx = Cat(:,4) > 24.5 & Cat(:,4) < 25.5;      % faintest sources
            CatFaint = Cat(idx,:);  
            OutRegName  = sprintf('%s%s%s%s%s%s',Args.OutDir,'/',Args.OutName,'_tile',Args.Tile,'faint.reg');
            DS9_new.regionWrite([CatFaint(:,1) CatFaint(:,2)],'FileName',OutRegName,'Color','blue','Marker','o','Size',1,'Width',4,...
                                 'Precision','%.2f','PrintIndividualProp',0);     
          
            idx = Cat(:,4) > 23.5 & Cat(:,4) < 24.5;      % medium brightness sources 
            CatMed = Cat(idx,:);  
            OutRegName  = sprintf('%s%s%s%s%s%s',Args.OutDir,'/',Args.OutName,'_tile',Args.Tile,'medium.reg');
            DS9_new.regionWrite([CatMed(:,1) CatMed(:,2)],'FileName',OutRegName,'Color','green','Marker','o','Size',1,'Width',4,...
                                 'Precision','%.2f','PrintIndividualProp',0);
            
            idx = Cat(:,4) > 22.5 & Cat(:,4) < 23.5;      % bright sources 
            CatBright = Cat(idx,:);  
            OutRegName  = sprintf('%s%s%s%s%s%s',Args.OutDir,'/',Args.OutName,'_tile',Args.Tile,'bright.reg');
            DS9_new.regionWrite([CatBright(:,1) CatBright(:,2)],'FileName',OutRegName,'Color','cyan','Marker','b','Size',1,'Width',4,...
                                'Precision','%.2f','PrintIndividualProp',0);     
        end
        
    end
    %%%%%%%%%%%%%%%%%%%%    
                    T0 = reportTime(Args.Verbosity, T0); %#ok<NASGU>
                    if Args.Verbosity > 0
                        cprintf('hyper','Simulation completed in %.1f s\n', toc(Tstart));
                    end
                                     
end



%%%%%
%%%%% internal functions (to be replaced later by ones from the core AstroPack set)
%%%%%




function [Image, JPSF, Widths] = injectArtSrc (X, Y, CPS, SizeX, SizeY, PSF, Args)
    % Make an artificial image with rotated and jitter-blurred source PSFs injected to the catalog positions     
    % Package: imUtil.art
    % Description: Make an artificial image with rotated and jitter-blurred source PSFs injected to the catalog positions
    % Input:   - X, Y, CPS      : pixel coordinates and countrates of the sources
    %          - SizeX, SizeY   : pixel sizes of the image containing the source PSFs
    %          - PSF            : either a single 2D PSF for all the object or 
    %                             a 3D array of individual PSFs
    %          * ...,key,val,...
    %          'PSFScaling'     - Image pixel size / PSF pixel size ratio
    %          'RotatePSF'      - PSF rotation angle, either a single value
    %                             for all the sources or a vector of angles
    %          'PerSourceRotation' - if true, rotate each stamp by its own angle on a
    %                             padded canvas cropped back to one common size, so that
    %                             stamps rotated by different angles fit one array
    %          'Jitter'         - apply PSF blurring due to the S/C jitter  
    %          'JitterPar'      - [SigmaX0 SigmaY0 Rotation] jitter amplitudes [arcsec]
    %          'JitterExposure' - single-exposure duration [s] for the jitter blur
    %          'AxisXY'         - [X Y] of the telescope axis on the tile (the roll axis)
    %          'Method'         - source injection method: 'direct' or 'stampcube'.
    %                             See the Args.Inj comment in usim.m for the measured speed/accuracy
    %                             trade-off between 'direct' (default) and 'stampcube'.
    %          'MeasurePSF'     - whether to measure PSF flux containment and pseudo-FWHM (diagnostics)
    %          
    % Output : - Image: a 2D array containing the resulting source image 
    %          - JPSF:  a 2+1 D array of rotated and jittered source PSFs
    %          - Widths: [NumSrc 2] = [R50 FWHM] of each final PSF in IMAGE pixels, with
    %                    'MeasurePSF'; [] otherwise. R50 is the 50% encircled-flux radius
    %                    (imUtil.psf.quantileRadius), FWHM the geometric mean
    %                    sqrt(wX*wY) of imUtil.psf.pseudoFWHM's X and Y widths -- equal to
    %                    the FWHM for a round PSF, and the equal-area width otherwise
    %            
    % Tested : Matlab R2020b
    % Author : A.M. Krassilchtchikov et al. (Feb 2023)
    % Example: [Image, JPSF] = imUtil.art.injectArtSrc (X, Y, CPS, SizeX, SizeY, PSF,...
    %                                        'PSFScaling',5,'RotatePSF',-90,'Jitter',1);
    arguments        
        X
        Y
        CPS
        SizeX
        SizeY
        PSF 
        Args.PSFScaling     =    1;       % Image/PSF pixel size ratio
        Args.RotatePSF      =    0;       % PSF stamp rotation angle
        Args.PerSourceRotation = false;   % rotate each stamp by its own angle (pad + crop)
        Args.Jitter         =    0;       % PSF blurring due to the S/C jitter
        Args.JitterPar      = [2 2 10];   % [SigmaX0 SigmaY0 Rotation] [arcsec]
        Args.JitterExposure =  300;       % [s]
        Args.AxisXY         = [0 0];      % [X Y] of the telescope axis on the tile
        Args.Method         =   'direct'; % injection method: 'direct' or 'stampcube'
        Args.MeasurePSF     =    false;   % measure PSF flux containment and pseudo-FWHM        
    end
    % create an impty image of the given size
    Image0 = repmat(0, SizeX, SizeY);

    % get the number of sources and produce a source "catalog" array
    NumSrc = size(CPS,1);    
    Cat = [X Y CPS];

    % consistency checks
    if size(X,1) ~= size(Y,1) || size(X,1) ~= NumSrc
        error('Input sizes inconsistent in injectArtSrc, exiting..');
    end

    % rotate the PSFs if needed 
    %
    % the rotation does not conserve the flux, thus also need to renormalize
    % NB: with 'loose' the size of a rotated stamp depends on the rotation angle,
    % varying between Nx x Ny and sqrt(2) * Nx x sqrt(2) * Ny
    if Args.PerSourceRotation   % an individual angle for each source (AstroPack issue #1424)
        % pad each stamp to its diagonal and rotate with 'crop': all the stamps keep one
        % size, and no part of a rotated stamp is cut off
        if numel(Args.RotatePSF) ~= NumSrc
            error('injectArtSrc: RotatePSF must hold one angle per source, exiting..');
        end
        [Ny0, Nx0, ~] = size(PSF);
        Diag = hypot(Ny0, Nx0);
        Pad  = [ceil((Diag-Ny0)/2) ceil((Diag-Nx0)/2)] + 1;
        RotPSF = zeros(Ny0+2*Pad(1), Nx0+2*Pad(2), NumSrc);
        for Isrc = 1:1:NumSrc
            RotPSF(:,:,Isrc) = imrotate(padarray(PSF(:,:,Isrc), Pad), Args.RotatePSF(Isrc), 'bilinear', 'crop'); 
            RotPSF(:,:,Isrc) = RotPSF(:,:,Isrc) / sum ( RotPSF(:,:,Isrc), 'all' ); % rescale
        end
    elseif abs( Args.RotatePSF(1) ) < 1 || abs( Args.RotatePSF(1) - 360) < 1 % do nothing for small angles
        RotPSF = PSF;
    else                                % rotate all the PSFs by the same angle
        for Isrc = 1:1:NumSrc
            RotPSF(:,:,Isrc) = imrotate(PSF(:,:,Isrc), Args.RotatePSF(1), 'bilinear', 'loose'); 
            RotPSF(:,:,Isrc) = RotPSF(:,:,Isrc) / sum ( RotPSF(:,:,Isrc), 'all' ); % rescale
        end
    end
                
    % apply PSF blurring due to the S/C jitter; the roll term grows with the
    % distance from the telescope axis, so the positions are passed relative to it
    if Args.Jitter
        JPSF = ultrasat.jitter(RotPSF, [X-Args.AxisXY(1) Y-Args.AxisXY(2)], ...
                               'Exposure', Args.JitterExposure, 'SigmaX0', Args.JitterPar(1), ...
                               'SigmaY0', Args.JitterPar(2), 'Rotation', Args.JitterPar(3), ...
                               'Scaling', Args.PSFScaling);    
    else
        JPSF = RotPSF;
    end
    
    % measure the final PSF widths (if requested). NB: both imUtil.psf functions take the
    % stamp POSITIONALLY; the field radius, pixel scale and any plotting belong to the
    % caller (usim), which knows where the optical axis is on this tile
    Widths = [];
    if Args.MeasurePSF
        fprintf('Final PSF stamp size %.1f x %.1f image pixels\n', ...
                size(JPSF,1)./Args.PSFScaling, size(JPSF,2)./Args.PSFScaling);
        NumPSF = size(JPSF,3);   % 1 if a single PSF is shared by all the sources
        Widths = zeros(NumSrc,2);
        for Isrc = 1:1:NumSrc
            Stamp = JPSF(:,:,min(Isrc,NumPSF));
            Widths(Isrc,1) = imUtil.psf.quantileRadius(Stamp,'Level',0.5);
            [WidthX, WidthY] = imUtil.psf.pseudoFWHM(Stamp,'Level',0.5);
            Widths(Isrc,2) = sqrt(WidthX .* WidthY);
        end
        Widths = Widths ./ Args.PSFScaling;   % oversampled -> image pixels
    end

    % PSF injection: inject all the rotated source PSFs into the blank image 
    switch lower(Args.Method)        
        case 'direct'
            Image = imUtil.art.addSources(Image0,JPSF.*reshape(CPS,1,1,NumSrc),...
                           [X Y],'Oversample',Args.PSFScaling,'Method','direct');

        case 'stampcube'
            % rotation and jitter are already applied to JPSF above; here only
            % downsample to detector resolution, sub-pixel center, and flux-scale
            % each source's stamp (imUtil.art.createSourceCube), then embed them
            % into the image with a single vectorized slice per source
            % (imUtil.art.addSources), avoiding the whole-tile imresize that
            % the 'direct' method performs.
            [CubePSF, XYInj] = imUtil.art.createSourceCube(JPSF, [X Y], CPS, ...
                                    'Oversample', Args.PSFScaling, 'RotAngle', [], ...
                                    'Recenter', true, 'RecenterMethod', 'lanczos');
            Image = imUtil.art.addSources(Image0, CubePSF, XYInj);

        otherwise
            error('Injection method not defined! Exiting..');
    end
end





function Mag = synPhotByFilter(Wave, Spec, IndFilt, FiltFamOf, FiltNameOf)
    % AB magnitudes of the spectra Spec [Nsrc Nwave] through their filters, with one
    % astro.spec.synthetic_phot call per filter (and per block of up to 2000 spectra).
    % IndFilt [Nsrc 1] is the filter index of each spectrum; FiltFamOf(I) returns the filter
    % family (or the filter object) of index I, FiltNameOf the filter name: a function
    % handle of I, or a fixed name
    Mag = zeros(size(Spec,1),1);
    for If = unique(IndFilt(:)).'
        Ind = find(IndFilt(:) == If);
        if isa(FiltNameOf,'function_handle')
            Name = FiltNameOf(If);
        else
            Name = FiltNameOf;
        end
        Fam = FiltFamOf(If);
        for I1 = 1:2000:numel(Ind)
            Ib = Ind(I1:min(I1+1999, numel(Ind)));
            Mag(Ib) = astro.spec.synthetic_phot([Wave(:) Spec(Ib,:).'], Fam, Name, 'AB');
        end
    end
end

function Spec = blackBodyShape(Wave, Temp)
    % The Planck spectral shape B_lambda(T) [arbitrary units] for each temperature, as an
    % [Ntemp Nwave] array; each distinct temperature is computed once, in blocks of 1000
    C2   = constant.h .* constant.c ./ constant.kB;     % [cm K], the second radiation constant
    Lam  = Wave(:).' .* 1e-8;                           % [cm]
    Temp = Temp(:);
    Spec = zeros(numel(Temp), numel(Lam));
    [UT, ~, IT] = unique(Temp);
    for I1 = 1:1000:numel(UT)
        Ib   = I1:min(I1+999, numel(UT));
        B    = 1 ./ (Lam.^5 .* expm1(C2 ./ (Lam .* UT(Ib))));
        Rows = IT >= Ib(1) & IT <= Ib(end);
        Spec(Rows,:) = B(IT(Rows) - Ib(1) + 1, :);
    end
end

function Spec = phoenixSpectra(Phoenix, Wave, Par)
    % Phoenix model spectra at the wavelengths Wave for the [Teff log(g)] rows of Par, by
    % linear interpolation in the Phoenix grid, in blocks of 500 spectra
    % a star outside the grid would get a NaN spectrum: use the nearest grid edge instead
    Lo  = [min(Phoenix.PhoenixTGrid) min(Phoenix.PhoenixLoggGrid)];
    Hi  = [max(Phoenix.PhoenixTGrid) max(Phoenix.PhoenixLoggGrid)];
    Out = any(Par(:,1:2) < Lo | Par(:,1:2) > Hi, 2);
    if any(Out)
        warning('ultrasat:usim:PhoenixGridEdge', ...
            '%d star(s) outside the Phoenix grid (Teff %g-%g K, log(g) %g-%g) get the spectrum at the nearest grid edge', ...
            nnz(Out), Lo(1), Hi(1), Lo(2), Hi(2));
        Par(:,1:2) = min(max(Par(:,1:2), Lo), Hi);
    end
    % hot, high-gravity models are missing from the grid (stored as all-zero spectra): a star
    % with no model at any of its surrounding grid points keeps its Teff and gets the highest
    % log(g) at which both bracketing Teff nodes have models
    Tg    = Phoenix.PhoenixTGrid(:);
    Gg    = Phoenix.PhoenixLoggGrid(:);
    Full  = squeeze(any(Phoenix.PhoenixSpec ~= 0, 1));                  % [NTeff Nlogg] models present
    Empty = interpn(Tg, Gg, double(Full), Par(:,1), Par(:,2), 'linear') < 1e-9;
    if any(Empty)
        It = discretize(Par(Empty,1), Tg);                              % the lower bracketing Teff node
        Ok = Full(It,:) & Full(It+1,:);                                 % [Nempty Nlogg]
        [~, Ilast] = max(fliplr(Ok), [], 2);
        Par(Empty,2) = Gg(numel(Gg) - Ilast + 1);
        warning('ultrasat:usim:PhoenixNoModel', ...
            '%d star(s) with no Phoenix model around their Teff/log(g) get the highest log(g) with models at their Teff', ...
            nnz(Empty));
    end
    Nsrc = size(Par,1);
    Spec = zeros(Nsrc, numel(Wave));
    for I1 = 1:500:Nsrc
        Ib = I1:min(I1+499, Nsrc);
        [Tq, Wq] = ndgrid(Par(Ib,1), Wave(:));
        Gq       = repmat(Par(Ib,2), 1, numel(Wave));
        Spec(Ib,:) = interpn(Phoenix.PhoenixWaveGrid, Phoenix.PhoenixTGrid, Phoenix.PhoenixLoggGrid, ...
                             Phoenix.PhoenixSpec, Wq, Tq, Gq);
    end
end

function T0 = reportTime(Verbosity, T0)
    % report the time spent in the current stage (if Verbosity > 0) and restart the stage timer
    if Verbosity > 0
        fprintf('%4.1f sec\n', toc(T0));
        drawnow('update');   % flush the progress lines to the Command Window
    end
    T0 = tic;
end

function N = drawCounts(Lam, Lam0, Thresh)
    % Draw the noisy counts of pixels with the expected counts Lam (Poisson below Thresh,
    % Gaussian above). Lam0 is the expected count of the plain sky, which most pixels share:
    % it is drawn for all of them at once from its tabulated cumulative distribution -- exact,
    % and much faster than poissrnd -- and only the excess Lam - Lam0 (the sources, flat-field
    % variations) is drawn with poissrnd, as a sum of Poisson variables is a Poisson variable.
    % Lam0 may also be a smooth map (vignetting): the sky is then drawn in bins of the sky
    % level 0.1% wide, each from its tabulated distribution at the bin's own level, which
    % shifts the mean sky of a pixel by at most 0.05%
    N  = zeros(size(Lam), 'single');
    Hi = Lam >= Thresh;
    N(Hi) = Lam(Hi) + sqrt(Lam(Hi)) .* randn(nnz(Hi), 1, 'single');
    Below = ~Hi & (Lam < Lam0);    % e.g. resampling dips under the sky level
    N(Below) = poissrnd(Lam(Below));
    Sky = ~Hi & ~Below;
    if isscalar(Lam0)
        if Lam0 > 0 && any(Sky(:))
            N(Sky) = drawSky(Lam0, nnz(Sky));
            Ex = Sky & (Lam > Lam0);
            N(Ex) = N(Ex) + poissrnd(Lam(Ex) - Lam0);
        end
    else
        Sky = Sky & (Lam0 > 0);
        Step = log1p(1e-3);
        L0   = min(Lam0(Sky));
        K    = round( log(Lam0(Sky) ./ L0) ./ Step );   % the bin of each sky pixel
        Ind  = find(Sky);
        NSky = zeros(numel(Ind), 1, 'single');
        for Kb = unique(K)'
            InBin = (K == Kb);
            NSky(InBin) = drawSky(L0 .* exp(Kb .* Step), nnz(InBin));
        end
        N(Ind) = NSky;
        Ex = Sky & (Lam > Lam0);
        N(Ex) = N(Ex) + poissrnd(Lam(Ex) - Lam0(Ex));
    end
end

function N = drawSky(Lam0, Num)
    % Num Poisson counts of the expected count Lam0, from its tabulated cumulative distribution
    Cdf = cumsum(poisspdf(0:1:ceil(Lam0 + 15.*sqrt(Lam0) + 15), Lam0));
    Cdf = Cdf(Cdf < 1 - 1e-15);
    [~, ~, Bin] = histcounts(rand(Num, 1), [0 Cdf 1]);   % edges may repeat: empty bins
    N = Bin - 1;
end

function [VigSrc, VigBkg] = vignettingMaps(Name, SizeX, SizeY, X0, Y0, CRPIX, PixSizeDeg, Rad, ObscRad)
    % The vignetting by the obscured part of the aperture, in the internal (X, Y) orientation.
    % VigSrc takes the sources, whose throughput already holds the radial obscuration curve
    % ObscRad(Rad) at the radius measured from (X0, Y0), to the 2-D map: (1-map)/(1-ObscRad(r)).
    % VigBkg is the map transmission over its tile mean. The map is fixed to the focal plane
    % and centred on the aimpoint (CRPIX): detector +X runs along its row angle, +Y along minus
    % its column angle. Both factors are 1 for 'none'.
    if strcmpi(Name, 'none')
        VigSrc = 1;
        VigBkg = 1;
        return
    end
    I = Installer;
    File = fullfile(I.getDataDir('ULTRASAT_Properties'), 'ObscurationMapELOP.mat');
    if ~isfile(File)
        error('The obscuration map %s is not found; copy it there or run with ''Vignetting'',''none'', exiting..', File);
    end
    S = load(File, 'ObscMap');
    Map    = griddedInterpolant({S.ObscMap.AngleRow, S.ObscMap.AngleCol}, S.ObscMap.Obscuration, ...
                                'cubic', 'linear');   % the tile corners lie ~0.15 deg beyond the map
    DX     = ((1:SizeX)' - CRPIX(1)) .* PixSizeDeg;  % [deg] from the aimpoint
    DY     = ((1:SizeY)  - CRPIX(2)) .* PixSizeDeg;
    Trans  = single( 1 - Map({DX, -DY}) );
    R      = min( hypot( single((1:SizeX)' - X0), single((1:SizeY) - Y0) ) .* PixSizeDeg, Rad(end) );
    VigSrc = Trans ./ (1 - interp1(single(Rad), single(ObscRad), R, 'linear'));
    VigBkg = Trans ./ mean(Trans, 'all');
end

function [Rate, MagV] = zodiModelRate(WCS, DateObs, SizeX, SizeY, X0, Y0, PixSizeDeg, UP, Rad, Wave, DeltaLambda, SAper, HC)
    % The zodiacal light count rate [e-/pix/s] of a tile: the zodiacal spectrum at the tile
    % centre (ultrasat.zodiac_bck) folded with the throughput averaged over the tile area
    RAD = 180./pi;
    [RA, Dec] = WCS.xy2sky((SizeX+1)/2, (SizeY+1)/2);                         % [deg]
    JD = celestial.time.date2jd([strrep(DateObs,'T',' ') '.000'], 'JD');
    Z  = ultrasat.zodiac_bck(RA./RAD, Dec./RAD, JD, 'Wave', Wave(:));
    MagV = Z.MagV;
    if abs(MagV - 21.5) < 1e-10   % the fill value of ultrasat.zodiac_bck_V where its table has no data
        warning('ultrasat:usim:ZodiNearSun', ...
            'The tile centre is in a direction with no zodiacal table data (near the Sun); the fill value V = 21.5 is used');
    end
    % the tile-area mean of the throughput: average the linear-interpolation weights of the
    % radius nodes over a grid of pixels (every 10th one), radii clamped to the table
    [Xg, Yg] = meshgrid(1:10:SizeX, 1:10:SizeY);
    R  = min( sqrt((Xg(:) - X0).^2 + (Yg(:) - Y0).^2) .* PixSizeDeg, Rad(end) );
    K  = discretize(R, Rad);
    Wt = (R - reshape(Rad(K),[],1)) ./ (reshape(Rad(K+1),[],1) - reshape(Rad(K),[],1));
    Wn = ( accumarray(K, 1-Wt, [numel(Rad) 1]) + accumarray(K+1, Wt, [numel(Rad) 1]) ) ./ numel(R);
    [Wq, Rq] = ndgrid(Wave(:), Rad(:));
    MeanT = interpn(UP.wavelength, Rad', UP.TotT, Wq, Rq, 'linear', 0) * Wn;  % [Nwave 1]
    PixArcsec2 = (PixSizeDeg .* 3600).^2;
    % [e-/s/pix] = sum [erg cm(-2) s(-1) A(-1) arcsec(-2)] * [A] * [cm(2)] * [arcsec(2)] / [erg / ph]
    Rate = sum( Z.Spec(:) .* MeanT .* DeltaLambda .* Wave(:) ) .* SAper .* PixArcsec2 ./ (HC .* 1e8);
end

function S = ternaryStr(Cond, A, B)
    % A if Cond, else B
    if Cond
        S = A;
    else
        S = B;
    end
end

function V = perSourceColumn(V, NumSrc, Name)
    % broadcast a scalar per-source parameter to a NumSrc x 1 column, or reshape a
    % length-NumSrc vector (a row or a column) into a column
    if numel(V) == 1
        V = V .* ones(NumSrc,1);
    elseif numel(V) == NumSrc
        V = V(:);
    else
        error('%s must hold 1 or NumSrc (=%d) values, exiting..', Name, NumSrc);
    end
end

function AP = psfObject(PSF, Scale)
    % Pack the per-source PSF stamps into one AstroPSF with the source number (the row of
    % CatData) as its third dimension 'Source', in the image orientation (rows = Y) and at
    % the oversampled scale of the stamps (Scale = ImRes). The stamps of extended objects
    % (a cell array, of different sizes) are zero-padded to the largest size, centred to
    % within half an oversampled pixel.
    % Example: Stamp = AP.getPSF('PsfArgs', {'Source', 5});
    if iscell(PSF)
        Sz   = max(cell2mat(cellfun(@(S) [size(S,1) size(S,2)], PSF(:), 'UniformOutput', false)), [], 1);
        Cube = zeros([Sz numel(PSF)]);
        for Isrc = 1:1:numel(PSF)
            [Ny, Nx] = size(PSF{Isrc});
            Oy = floor((Sz(1) - Ny)./2);
            Ox = floor((Sz(2) - Nx)./2);
            Cube(Oy+(1:Ny), Ox+(1:Nx), Isrc) = PSF{Isrc};
        end
    else
        Cube = PSF;
    end
    AP = AstroPSF;
    AP.DataPSF      = permute(Cube, [2 1 3]);   % the internal (X, Y) layout -> the image orientation
    AP.DimName      = {'Source'};
    AP.DimVals      = {1:size(Cube,3)};
    AP.InterpMethod = {'nearest'};
    AP.Scale        = [Scale Scale];
end

function V = extBroadcast(V, NumExt, Name)
    % broadcast a scalar extended-object parameter to NumExt elements,
    % or pass a length-NumExt vector through unchanged
    if numel(V) == 1
        V = repmat(V, NumExt, 1);
    elseif numel(V) ~= NumExt
        error('%s must be a scalar or have NumExt (=%d) elements, exiting..', Name, NumExt);
    else
        V = V(:);
    end
end

function Par = extSpecRow(Spec, Iext, NumExt, Npar)
    % return the Iext-th row of Npar per-object spectral parameters,
    % broadcasting a single shared row to all NumExt objects if only one row was given
    if size(Spec,1) == 1 && NumExt > 1
        Par = Spec(1,1:Npar);
    else
        if size(Spec,1) ~= NumExt
            error('ExtSpec must have NumExt (=%d) rows, or a single row to broadcast to all objects, exiting..', NumExt);
        end
        Par = Spec(Iext,1:Npar);
    end
end


function SpecPar = chunkSpecPar(Spec, Range, NumSrc, NumSrcCh)
    % return the [Teff log(g)] rows of the sources of the current chunk,
    % broadcasting a single shared row to all NumSrc sources if only one row was given.
    % NB: Range is the GLOBAL source index range of the chunk: indexing Spec with the
    % chunk-local index gives every chunk but the first the spectra of other sources.
    if size(Spec,2) < 2
        error('The source Teff/log(g) array must have 2 columns, exiting..');
    end
    if size(Spec,1) == 1
        SpecPar = repmat(Spec(1,1:2), NumSrcCh, 1);
    else
        if size(Spec,1) ~= NumSrc
            error('The size of the source Teff/log(g) array is incorrect: %d rows for %d sources, exiting..', ...
                   size(Spec,1), NumSrc);
        end
        SpecPar = Spec(Range,1:2);  % NB: Range is GLOBAL, the chunk-local index must not be used here
    end
end

function Spec = cutSpecToFOV(Spec, SpecType, Ind)
    % keep in Spec only the entries belonging to the sources inside the FOV.
    % NB: the layout of Spec depends on the spectral model -- 'tab' holds one COLUMN per
    % source (optionally followed by a shared wavelength column), the parametric models
    % hold one ROW per source -- so the cut has to be dispatched on SpecType. A single
    % spectrum (or parameter row) is broadcast to all the sources and is left uncut.
    if isa(Spec,'AstroSpec')
        if numel(Spec) > 1
            Spec = Spec(Ind);
        end
        return
    end
    NumSrc0 = numel(Ind);   % the number of sources before the cut
    switch lower(SpecType)
        case 'tab'
            if size(Spec,2) == NumSrc0 + 1      % the wavelength grid is the last column
                Spec = Spec(:,[Ind(:); true]);
            elseif size(Spec,2) == NumSrc0
                Spec = Spec(:,Ind);
            end
        case {'pickles','phoenix'}              % NumSrc x 2
            if size(Spec,1) == NumSrc0
                Spec = Spec(Ind,:);
            end
        otherwise                               % 'bb', 'pl': 1 parameter per source
            if numel(Spec) == NumSrc0
                Spec = Spec(Ind);
            end
    end
end

function SNR = crudeSNR (Stamps, Flux, Exposure, BackPerPix, Scaling, Method)
    % Estimate the S/N of sources from the very stamps that are injected into the image
    % Description: The image this simulator builds has a per-pixel variance of
    %              (source counts + BackPerPix) -- see the poissrnd/normrnd call in usim --
    %              so the matched estimator over an aperture A is
    %                   sum_A S / sqrt( sum_A S + N_A * BackPerPix ).
    %              The historical formula ('legacy' below) is that expression with the
    %              sum_A S term dropped from the variance, which is only valid in the
    %              background-limited regime; it overestimates a 60 counts/s star in a
    %              300 s exposure by a factor ~8.
    % Input:  - Stamps     : [Ny Nx Nsrc] oversampled source stamps (PSF, or a
    %                        profile-convolved object stamp), each ~ unit total flux
    %         - Flux       : [Nsrc 1] source count rates [counts/s]
    %         - Exposure   : total exposure [s]
    %         - BackPerPix : background variance [counts/pix] over the whole exposure
    %         - Scaling    : stamp oversampling factor (usim's Args.ImRes)
    %         - Method     : 'aperture' | 'optimal' | 'shot' | 'legacy', see Args.SNRMethod
    % Output: - SNR        : [Nsrc 1]
    % Author : A.M. Krassilchtchikov (Sep 2026)
    Flux   = Flux(:);
    NumSrc = numel(Flux);
    SNR    = zeros(NumSrc,1);
    if NumSrc < 1
        return
    end
    Stot = Flux .* Exposure;   % total source counts collected over the whole exposure

    switch lower(Method)
        case {'legacy','shot'}
            % the historical 50%-containment-radius aperture, kept bit-for-bit for
            % 'legacy' because callers that invert CrudeSNR rely on it being exactly
            % linear in flux (see ultrasat.ELOPsim)
            PSFeff           = 0.8;
            ContainmentLevel = 0.5;
            for Isrc = 1:1:NumSrc
                Rad = imUtil.psf.quantileRadius(Stamps(:,:,Isrc),'Level',ContainmentLevel) ./ Scaling;
                Sig = PSFeff .* Stot(Isrc);
                if strcmpi(Method,'legacy')
                    SNR(Isrc) = Sig ./ sqrt( pi .* Rad.^2 .* BackPerPix );
                else
                    SNR(Isrc) = Sig ./ sqrt( Sig + pi .* Rad.^2 .* BackPerPix );
                end
            end
        case {'aperture','optimal'}
            % work in blocks: the radius cube built below is [Ny Nx Nblock]
            MaxBlock = 2000;
            for I1 = 1:MaxBlock:NumSrc
                I2  = min(I1+MaxBlock-1, NumSrc);
                Sub = I1:1:I2;
                SNR(Sub) = snrFromStamps( stamps2det(Stamps(:,:,Sub), Scaling), ...
                                          Stot(Sub), BackPerPix, Method );
            end
        otherwise
            error('ultrasat:usim:UnknownSNRMethod', ...
                  'Args.SNRMethod = ''%s'' is not one of aperture/optimal/shot/legacy, exiting..', Method);
    end
end

function D = stamps2det (S, Scaling)
    % Resample oversampled stamps onto the detector grid by summing each Scaling x Scaling
    % block: a detector pixel integrates the flux falling on its area, so for an integer
    % factor the block sum is that integral exactly (the same reason 'box' is the only
    % correct imresize kernel for this step, cf. AstroPack issue #1296)
    [Ny, Nx, Ns] = size(S);
    if abs(Scaling - 1) < 1e-10
        D = S;
        return
    end
    if abs(Scaling - round(Scaling)) < 1e-10
        Sc = round(Scaling);
        Py = mod(-Ny, Sc);
        Px = mod(-Nx, Sc);
        if Py > 0 || Px > 0
            % pad symmetrically, so the block grid stays centred on the stamp
            Pad = zeros(Ny+Py, Nx+Px, Ns, 'like', S);
            Pad(floor(Py./2)+(1:1:Ny), floor(Px./2)+(1:1:Nx), :) = S;
            S  = Pad;
            Ny = Ny + Py;
            Nx = Nx + Px;
        end
        Ny2 = Ny ./ Sc;
        Nx2 = Nx ./ Sc;
        A = sum( reshape(S, Sc, Ny2.*Nx.*Ns), 1 );
        A = permute( reshape(A, Ny2, Nx, Ns), [2 1 3] );
        A = sum( reshape(A, Sc, Nx2.*Ny2.*Ns), 1 );
        D = permute( reshape(A, Nx2, Ny2, Ns), [2 1 3] );
    else
        % non-integer oversampling (ImRes = 47.5): imresize 'box' averages over the block,
        % so multiply the block area back in to conserve the total flux
        D = imresize(S, 1./Scaling, 'box') .* Scaling.^2;
    end
end

function SNR = snrFromStamps (P, Stot, BackPerPix, Method)
    % S/N of sources whose detector-grid stamps are P, given their total counts Stot
    [Ny, Nx, Ns] = size(P);
    Stot = reshape(Stot, 1, 1, Ns);

    % the stamps are only approximately normalised after rotation/jitter/resampling
    Norm = sum(P, [1 2]);
    Norm(Norm == 0) = 1;
    Sig  = (P ./ Norm) .* Stot;   % expected source counts in each detector pixel

    if strcmpi(Method,'optimal')
        % matched filter: the highest S/N any pixel weighting can reach, i.e. what PSF
        % fitting approaches. Reads above aperture photometry by ~10-15%.
        SNR = sqrt( sum( Sig.^2 ./ (max(Sig,0) + BackPerPix), [1 2] ) );
        SNR = SNR(:);
        return
    end

    % 'aperture': grow a circular aperture around the stamp centroid and keep the radius
    % that maximises S/N -- the aperture an observer would end up choosing
    [Yg, Xg] = ndgrid(1:1:Ny, 1:1:Nx);
    Wgt  = max(P, 0);             % resampling can ring slightly negative; ignore that here
    Wsum = sum(Wgt, [1 2]);
    Wsum(Wsum == 0) = 1;
    Cy  = sum(Wgt .* Yg, [1 2]) ./ Wsum;
    Cx  = sum(Wgt .* Xg, [1 2]) ./ Wsum;
    Rad = sqrt( (Yg - Cy).^2 + (Xg - Cx).^2 );

    RadMax  = min(Ny, Nx) ./ 2;
    NRad    = min( max(ceil(RadMax./0.5), 1), 200 );   % 0.5 px steps, capped for big stamps
    RadGrid = linspace(RadMax./NRad, RadMax, NRad);

    SNR = zeros(Ns,1);
    for Ir = 1:1:NRad
        Mask  = Rad <= RadGrid(Ir);
        SigR  = sum( Sig .* Mask, [1 2] );
        NpixR = sum( Mask, [1 2] );
        SnrR  = SigR ./ sqrt( max(SigR,0) + NpixR .* BackPerPix );
        SNR   = max(SNR, SnrR(:));
    end
end

function Result = unitTest()
    % Package Unit-Test   
    
    % testing usim simulation utility:
    
    SimA = ultrasat.usim('Cat',1000, 'Tile', 'A');
    SimB = ultrasat.usim('Cat',1000, 'Tile', 'B');
    SimC = ultrasat.usim('Cat',1000, 'Tile', 'C');
    SimD = ultrasat.usim('Cat',1000, 'Tile', 'D');

    MergedImage = ultrasat.umergeTileImages ();

    % testing multiple extended-object simulation in a single call:

    SimExt = ultrasat.usim('ExtProfileType','sersic', ...
        'ExtProfilePar',[40 4 1; 12 2 1; 60 4 1], ...
        'ExtSizeRA',[200 60 400],'ExtSizeDec',[200 60 250],'ExtOversampling',1, ...
        'ExtRA0',[221.60 221.75 221.99],'ExtDec0',[56.30 56.40 56.45], ...
        'ExtMag',[12 15 13],'ExtSpecType','BB','ExtSpec',[8000;6000;12000], ...
        'Tile','B','Exposure',[1 300],'OutName','SimImageExt');

    % testing the CrudeSNR estimate (usim's Args.SNRMethod):

    testCrudeSNR();

    % testing ULTRASAT PSF image content:

    I = Installer;
    PSF_db = sprintf('%s%s',I.getDataDir('ULTRASAT_PSF'),'/ULTRASATlabPSF5.mat');
    ReadDB = struct2cell ( io.files.load1(PSF_db) ); % PSF data at the chosen spatial resolution
    PSFdata = ReadDB{2};
    
    ContRad = zeros(91,25);
    Lam = 200:10:1100;
    Rad = linspace(0,10,25);
    for iR = 1:25
        for iL = 1:91
            ContRad(iL,iR) = imUtil.psf.quantileRadius(PSFdata(:,:,iL,iR),'Level',0.9)./5;
        end
    end
    imagesc('XData',Rad,'YData',Lam,'CData',ContRad)
        
	Result = true;
    
    !rm SimImage* 
end

function testCrudeSNR()
    % Regression tests for usim's CrudeSNR column (Args.SNRMethod).
    % The simulated image has a per-pixel variance of (source counts + Back.Tot), so the
    % S/N of an aperture A is  sum_A S / sqrt( sum_A S + N_A*Back.Tot ). Until Sep 2026
    % the column dropped the sum_A S term (now SNRMethod = 'legacy') and read ~8x too high
    % for bright sources. The checks below do not reuse usim's own S/N code: the
    % reference is computed from the noiseless image usim writes, and verified by a
    % Monte Carlo over Poisson realisations of that same image.

    RngState = rng;
    rng(20260923);
    Cleanup  = onCleanup(@() rng(RngState));

    % three well-separated sources: bright (shot limited), medium, faint (background limited)
    Cat    = [1000 1000; 2400 2400; 3600 1400];
    Common = {'Cat',Cat,'SkyCat',false,'SpecType','BB','Spec',5800, ...
              'Exposure',[1 300],'Tile','B','OutType','none'};
    Mag    = [14; 17; 21];

    getSNR = @(Sim) Sim.CatData.Catalog(:, strcmp(Sim.CatData.ColNames,'SNR'));
    getCPS = @(Sim) Sim.CatData.Catalog(:, strcmp(Sim.CatData.ColNames,'Counts/s'));

    % 1. 'legacy' must stay exactly linear in flux: ultrasat.ELOPsim used to invert it in
    %    closed form, and it is kept only for callers that rely on that
    Leg1 = ultrasat.usim(Common{:}, 'Mag', Mag,       'SNROnly', true, 'SNRMethod', 'legacy');
    Leg2 = ultrasat.usim(Common{:}, 'Mag', Mag + 2.5, 'SNROnly', true, 'SNRMethod', 'legacy');
    assert( all( abs( getSNR(Leg1)./getSNR(Leg2) - 10 ) < 1e-8 ), ...
            'usim: SNRMethod=''legacy'' is no longer linear in flux');

    % 2. ordering and physical bounds of the other methods, per source
    Ap  = ultrasat.usim(Common{:}, 'Mag', Mag, 'SNROnly', true);   % default = 'aperture'
    Opt = ultrasat.usim(Common{:}, 'Mag', Mag, 'SNROnly', true, 'SNRMethod', 'optimal');
    Sh  = ultrasat.usim(Common{:}, 'Mag', Mag, 'SNROnly', true, 'SNRMethod', 'shot');
    SNRap = getSNR(Ap);  SNRopt = getSNR(Opt);  SNRleg = getSNR(Leg1);  SNRsh = getSNR(Sh);
    Scnt  = getCPS(Ap) .* 300;                               % total source counts
    assert( all(SNRap > 0) && all(isfinite(SNRap)), 'usim: aperture CrudeSNR not positive/finite');
    assert( all(SNRopt >= SNRap .* (1 - 1e-12)), 'usim: optimal S/N below aperture S/N');
    assert( all(SNRap  < SNRleg), 'usim: aperture S/N not below the background-only legacy value');
    assert( all(SNRsh  < SNRleg), 'usim: shot S/N not below the background-only legacy value');
    assert( all(SNRopt < sqrt(Scnt)), 'usim: S/N exceeds the pure shot-noise ceiling sqrt(counts)');
    % the bright source is shot limited, so its legacy value must be far too high
    assert( SNRleg(1) ./ SNRap(1) > 3, 'usim: legacy/aperture ratio for a bright source is implausibly small');

    % 3. unknown method names must be rejected
    Threw = false;
    try
        ultrasat.usim(Common{:}, 'Mag', Mag, 'SNROnly', true, 'SNRMethod', 'nonsense');
    catch ME
        Threw = strcmp(ME.identifier, 'ultrasat:usim:UnknownSNRMethod');
    end
    assert(Threw, 'usim: an unknown SNRMethod was not rejected with ultrasat:usim:UnknownSNRMethod');

    % 4. end to end: the 'aperture' S/N against the noiseless image usim actually writes
    Sim   = ultrasat.usim(Common{:}, 'Mag', Mag, 'NoisePoisson', false);
    Image = Sim.Image;                           % [row = Y, col = X], expected counts
    SNR   = getSNR(Sim);
    X     = Sim.CatData.Catalog(:, strcmp(Sim.CatData.ColNames,'X'));
    Y     = Sim.CatData.Catalog(:, strcmp(Sim.CatData.ColNames,'Y'));
    Back  = median(Image, 'all');                % = Back.Tot: the image is almost all sky
    Half  = 20;
    Nmc   = 2000;
    for Isrc = 1:1:numel(SNR)
        Rows = round(Y(Isrc)) + (-Half:Half);
        Cols = round(X(Isrc)) + (-Half:Half);
        Cut  = Image(Rows, Cols);
        Src  = Cut - Back;                       % expected source counts per pixel
        [Cc, Rr] = meshgrid(Cols, Rows);
        W    = max(Src, 0);
        Rad  = sqrt( (Rr - sum(W.*Rr,'all')./sum(W,'all')).^2 + ...
                     (Cc - sum(W.*Cc,'all')./sum(W,'all')).^2 );
        % best circular aperture, computed independently of usim's S/N code: grow it one
        % pixel at a time in order of distance from the centroid. NB: a coarse radius
        % grid is NOT good enough here -- a faint, background-limited source's optimal
        % aperture holds only ~7 pixels, and missing the right pixel set costs several
        % percent (a 0.5 px grid put the mag-21 source 3.3% low). For the same reason
        % the source positions are integer pixels: moving the aperture centre by up to
        % half a pixel changes that source's best S/N by up to ~14%, and usim's estimate
        % (made on the stamp's own grid) is only comparable at matching pixel phase.
        [~, Order] = sort(Rad(:));
        CumS  = cumsum(Src(Order));
        Snr   = CumS ./ sqrt( max(CumS,0) + (1:1:numel(CumS)).'.*Back );
        [Best, Npix] = max(Snr);
        Mask  = false(size(Src));
        Mask(Order(1:Npix)) = true;
        assert( abs(SNR(Isrc)./Best - 1) < 0.03, ...
                'usim: source %d: CrudeSNR %.3g differs from the image-based value %.3g by >3%%', ...
                Isrc, SNR(Isrc), Best);

        % Monte Carlo: the formula must describe the Poisson scatter in that aperture
        Flux = zeros(Nmc,1);
        for Imc = 1:1:Nmc
            Real      = poissrnd(Cut);
            Flux(Imc) = sum(Real(Mask)) - Npix.*Back;
        end
        SNRmc = mean(Flux) ./ std(Flux);
        % MC relative error ~ 1/sqrt(2*Nmc) = 1.6%, plus the faint source's mean(Flux) noise
        assert( abs(SNRmc./Best - 1) < 0.06, ...
                'usim: source %d: Monte Carlo S/N %.3g differs from the predicted %.3g by >6%%', ...
                Isrc, SNRmc, Best);
    end
end

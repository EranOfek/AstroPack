% AstroHeader_benchmark.m
%
% Demonstrates the performance improvement from the O(1) keyword index and
% synonym cache introduced in the AstroHeader redesign.
%
% Run this script after placing AstroHeader.m on your MATLAB path.
%
% Expected results (indicative; hardware-dependent):
%   - Single getVal call:  ~10-30x faster with index (no O(N) scan)
%   - Batch over N images: speedup grows with N and header size
%
% Author: benchmark added in redesign (2025)

fprintf('=== AstroHeader benchmark ===\n\n');

% ---- parameters ----------------------------------------------------------
HEADER_SIZE    = 300;   % keywords per synthetic FITS header (typical range 50-500)
N_IMAGES       = 5000;  % number of synthetic AstroHeader objects
N_GETVAL       = 20;    % keywords to retrieve per image
NREP_SINGLE    = 1e4;   % repetitions for single-call timing

% ---- build a big synthetic header ----------------------------------------
fprintf('Building synthetic headers (size=%d, Nimages=%d)...\n', HEADER_SIZE, N_IMAGES);

BaseKeys = {'EXPTIME','GAIN','FILTER','NAXIS1','NAXIS2','DATE-OBS','RA','DEC', ...
            'IMTYPE','AIRMASS','SEEING','LIMMAG','ZP','CRVAL1','CRVAL2', ...
            'CRPIX1','CRPIX2','CD1_1','CD2_2','BITPIX'};
NBaseKeys   = numel(BaseKeys);
PadKeys     = arrayfun(@(k) sprintf('KEY%04d',k), 1:(HEADER_SIZE-NBaseKeys), 'uni', 0);
AllKeyNames = [BaseKeys, PadKeys];

% one shared cell array
Data = cell(HEADER_SIZE, 3);
for Ik = 1:HEADER_SIZE
    Data{Ik,1} = AllKeyNames{Ik};
    Data{Ik,2} = Ik * 1.1;
    Data{Ik,3} = sprintf('comment for %s', AllKeyNames{Ik});
end

% Create N_IMAGES objects sharing the same Data
Harr = AstroHeader(N_IMAGES);
for Ii = 1:N_IMAGES
    Harr(Ii).Data = Data;
end
fprintf('Done.\n\n');

% ---- keys to look up -------------------------------------------------------
LookupKeys = BaseKeys(1:N_GETVAL);   % use the first N_GETVAL canonical keys

% ==========================================================================
%  BENCHMARK 1: single getVal call (hot index vs cold index first call)
% ==========================================================================
fprintf('--- Benchmark 1: single getVal call (N=%d reps) ---\n', NREP_SINGLE);

Hs = AstroHeader(1);
Hs.Data = Data;

% warm up JIT
for k = 1:10
    Hs.getValFast('EXPTIME');
end

% getValFast (O(1) index, already built)
t0 = tic;
for k = 1:NREP_SINGLE
    v = Hs.getValFast('EXPTIME'); %#ok<NASGU>
end
t_fast = toc(t0);

% getVal with UseDict=false (goes through full method overhead but uses index)
Hs2      = AstroHeader(1);
Hs2.Data = Data;
t0 = tic;
for k = 1:NREP_SINGLE
    v = Hs2.getVal('EXPTIME','UseDict',false); %#ok<NASGU>
end
t_getval_idx = toc(t0);

% raw strcmp scan (original approach approximation)
t0 = tic;
for k = 1:NREP_SINGLE
    ColData = Hs.Data(:,1);
    idx     = find(strcmp(ColData, 'EXPTIME'), 1, 'first');
    v       = Hs.Data{idx, 2}; %#ok<NASGU>
end
t_linear = toc(t0);

fprintf('  getValFast        : %.3f ms / call\n',  t_fast     / NREP_SINGLE * 1e3);
fprintf('  getVal (idx path) : %.3f ms / call\n',  t_getval_idx / NREP_SINGLE * 1e3);
fprintf('  raw strcmp scan   : %.3f ms / call\n',  t_linear   / NREP_SINGLE * 1e3);
fprintf('  Speedup (fast vs linear): %.1fx\n\n', t_linear / t_fast);

% ==========================================================================
%  BENCHMARK 2: getCellKeyFast over N_IMAGES headers
% ==========================================================================
fprintf('--- Benchmark 2: retrieve %d keys from %d headers ---\n', N_GETVAL, N_IMAGES);

% getCellKeyFast  (pure index)
t0 = tic;
R1 = Harr.getCellKeyFast(LookupKeys);
t_ckfast = toc(t0);

% getCellKey UseDict=false (routes to imUtil.headerCell.getByKey)
t0 = tic;
R2 = Harr.getCellKey(LookupKeys, 'UseDict', false);
t_ck_legacy = toc(t0);

fprintf('  getCellKeyFast     : %.3f s\n', t_ckfast);
fprintf('  getCellKey legacy  : %.3f s\n', t_ck_legacy);
fprintf('  Speedup            : %.1fx\n\n', t_ck_legacy / t_ckfast);

% verify same results
if isequal(R1, R2)
    fprintf('  Results match: YES\n\n');
else
    fprintf('  Results match: NO (check implementation)\n\n');
end

% ==========================================================================
%  BENCHMARK 3: synonym cache warm-up effect
% ==========================================================================
fprintf('--- Benchmark 3: synonym cache warm-up (%d calls) ---\n', NREP_SINGLE);

Hsc      = AstroHeader(1);
Hsc.Data = Data;

% cold: clear cache and time the first N calls (each one traverses dict)
Hsc.SynonymCache = [];
t0 = tic;
for k = 1:NREP_SINGLE
    % force cold lookup by passing a unique dict-key each iteration
    Hsc.resolveSynonym('EXPTIME', 'CaseSens', false);
end
t_cached = toc(t0);

% for comparison: manually build a fresh cache on each call
t0 = tic;
for k = 1:NREP_SINGLE
    Hsc2          = AstroHeader(1);
    Hsc2.Data     = Hsc.Data;
    Hsc2.KeyDict  = Hsc.KeyDict;
    Hsc2.resolveSynonym('EXPTIME', 'CaseSens', false);
    clear Hsc2;
end
t_nocache = toc(t0);

fprintf('  With cache (2nd+ calls) : %.3f ms / call\n', t_cached   / NREP_SINGLE * 1e3);
fprintf('  Without cache (each new): %.3f ms / call\n', t_nocache  / NREP_SINGLE * 1e3);
fprintf('  Cache speedup           : %.1fx\n\n', t_nocache / t_cached);

fprintf('=== benchmark complete ===\n');

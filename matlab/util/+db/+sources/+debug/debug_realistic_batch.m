%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debug_realistic_batch.m
% Author      : Chen Tishler
% Created     : 19/08/2026
% Updated     : 09/09/2026
% Description : Realistic sky-field catalogs and visit batch simulation (MATLAB).
%==========================================================================

function varargout = debug_realistic_batch(action, varargin)
%DEBUG_REALISTIC_BATCH  LAST-like visit batches for continuous MATLAB insert.
%
%   State = db.sources.debug.debug_realistic_batch('load', stateFile, 'Seed', 1042, 'Fields', 8)
%   tbl   = db.sources.debug.debug_realistic_batch('simulate', State, sky, 'Rows', 1000, 'VisitIndex', 0)
%   db.sources.debug.debug_realistic_batch('save', stateFile, State)
%   visitIdx = db.sources.debug.debug_realistic_batch('bumpVisit', State, fieldName)
%   fields = db.sources.debug.debug_realistic_batch('defaultFields', 8)
%
%   Sky field RA centers are offset +12 deg vs Python debug/client so MATLAB
%   first visits are mostly NEW alongside the running Python inserter.

    C = db.sources.debug.debugConstants_();

    switch lower(action)
        case 'defaultfields'
            n = varargin{1};
            varargout{1} = defaultSkyFields_(n, C.RaOffsetDeg);
        case 'load'
            p = inputParser;
            addRequired(p, 'stateFile', @(x) ischar(x) || isstring(x));
            addParameter(p, 'Seed', C.DefaultSeed, @isnumeric);
            addParameter(p, 'Fields', C.DefaultFields, @isnumeric);
            parse(p, varargin{:});
            varargout{1} = loadBatchState_(char(p.Results.stateFile), ...
                p.Results.Seed, p.Results.Fields, C.RaOffsetDeg);
        case 'save'
            stateFile = varargin{1};
            state = varargin{2};
            saveBatchState_(char(stateFile), state);
        case 'bumpvisit'
            state = varargin{1};
            fieldName = char(varargin{2});
            key = visitCountKey_(state, fieldName);
            if isfield(state.visit_counts, key)
                count = state.visit_counts.(key);
            else
                count = 0;
            end
            state.visit_counts.(key) = count + 1;
            varargout{1} = count;
            varargout{2} = state;
        case 'simulate'
            state = varargin{1};
            sky = varargin{2};
            p = inputParser;
            addParameter(p, 'Rows', 1000, @isnumeric);
            addParameter(p, 'VisitIndex', 0, @isnumeric);
            addParameter(p, 'FieldNameTag', '', @(x) ischar(x) || isstring(x));
            parse(p, varargin{3:end});
            [tbl, state] = simulateVisitBatch_(state, sky, p.Results.Rows, ...
                p.Results.VisitIndex, C, char(p.Results.FieldNameTag));
            varargout{1} = tbl;
            varargout{2} = state;
        otherwise
            error('debug_realistic_batch:UnknownAction', 'Unknown action: %s', action);
    end
end


function fields = defaultSkyFields_(n, raOffset)
%DEFAULTSKYFIELDS_  Return n LAST-like sky field descriptors with RA offset.

    fields(1) = struct('name', 'LAST_galactic_center', 'ra_center', 266.4 + raOffset, ...
        'dec_center', -29.0, 'fov_deg', 2.5, 'density_factor', 3.0, 'seed', 1000);
    fields(2) = struct('name', 'LAST_high_latitude', 'ra_center', 185.0 + raOffset, ...
        'dec_center', 65.0, 'fov_deg', 2.5, 'density_factor', 0.6, 'seed', 2000);
    fields(3) = struct('name', 'LAST_ecliptic_belt', 'ra_center', 45.0 + raOffset, ...
        'dec_center', 8.0, 'fov_deg', 2.5, 'density_factor', 1.2, 'seed', 3000);
    fields(4) = struct('name', 'LAST_south_pole', 'ra_center', mod(0.0 + raOffset, 360), ...
        'dec_center', -88.0, 'fov_deg', 2.5, 'density_factor', 0.4, 'seed', 4000);
    if n <= 4
        fields = fields(1:n);
        return;
    end

    raSteps = (0:3) * 45.0;
    decSteps = [-20.0, 20.0];
    seedBase = 5000;
    idx = 4;
    for di = 1:numel(decSteps)
        for ri = 1:numel(raSteps)
            if numel(fields) >= n
                break;
            end
            idx = idx + 1;
            fields(idx) = struct('name', sprintf('LAST_tile_%02d', idx), ...
                'ra_center', mod(raSteps(ri) + raOffset, 360), ...
                'dec_center', decSteps(di), 'fov_deg', 2.5, ...
                'density_factor', 1.0, 'seed', seedBase + idx);
        end
    end
    fields = fields(1:n);
end


function state = loadBatchState_(path, seed, nFields, raOffset)
%LOADBATCHSTATE_  Load or initialize continuous-insert simulation state from JSON.

    state = struct();
    state.seed = seed;
    state.round_idx = 0;
    state.fields = defaultSkyFields_(nFields, raOffset);
    state.catalogs = struct();
    state.visit_counts = struct();
    state.field_names = struct();
    for i = 1:numel(state.fields)
        key = sprintf('f%03d', i);
        state.field_names.(key) = state.fields(i).name;
    end
    if ~isfile(path)
        return;
    end

    raw = jsondecode(fileread(path));
    if isfield(raw, 'seed')
        state.seed = raw.seed;
    end
    if isfield(raw, 'round_idx')
        state.round_idx = raw.round_idx;
    end
    if isfield(raw, 'fields') && ~isempty(raw.fields)
        state.fields = raw.fields;
        state.field_names = struct();
        for i = 1:numel(state.fields)
            key = sprintf('f%03d', i);
            state.field_names.(key) = state.fields(i).name;
        end
    end
    if isfield(raw, 'catalogs')
        state.catalogs = raw.catalogs;
    end
    if isfield(raw, 'visit_counts')
        state.visit_counts = raw.visit_counts;
    end
end


function saveBatchState_(path, state)
%SAVEBATCHSTATE_  Persist simulation state to catalog.json.

    dirPath = fileparts(path);
    if ~isempty(dirPath) && ~isfolder(dirPath)
        mkdir(dirPath);
    end

    payload = struct();
    payload.seed = state.seed;
    payload.fields = state.fields;
    payload.catalogs = state.catalogs;
    payload.visit_counts = state.visit_counts;
    payload.round_idx = state.round_idx;
    fid = fopen(path, 'w');
    if fid < 0
        error('debug_realistic_batch:SaveFailed', 'Cannot write %s', path);
    end
    fprintf(fid, '%s', jsonencode(payload));
    fclose(fid);
end


function key = visitCountKey_(state, fieldName)
%VISITCOUNTKEY_  Map sky field name to stable struct key fNNN.

    for i = 1:numel(state.fields)
        if strcmp(state.fields(i).name, fieldName)
            key = sprintf('f%03d', i);
            return;
        end
    end
    key = matlab.lang.makeValidName(fieldName);
end


function pop = generatePopulation_(sky, popSeed, size, fluxMax)
%GENERATEPOPULATION_  Random source population within one sky field FOV.

    half = sky.fov_deg / 2.0;
    n = max(size, 100);
    rng(popSeed);
    ra = mod(sky.ra_center + (rand(n, 1) * 2 - 1) * half, 360.0);
    dec = max(-89.9, min(89.9, sky.dec_center + (rand(n, 1) * 2 - 1) * half));

    magnitude = single(15.0 + rand(n, 1) * 4.5);
    flux = single(min(fluxMax, max(0.1, 10 .^ ((25.0 - double(magnitude)) / 2.5))));
    flags = zeros(n, 1, 'uint32');
    pop = struct('ra', ra, 'dec', dec, 'magnitude', magnitude, ...
        'flux', flux, 'flags', flags);
end


function [tbl, state] = simulateVisitBatch_(state, sky, n, visitIndex, C, fieldNameTag)
%SIMULATEVISITBATCH_  Build one visit batch table (first visit or revisit mix).

    catKey = matlab.lang.makeValidName(sky.name);
    if ~isfield(state.catalogs, catKey)
        popSeed = sky.seed;
        if popSeed == 0
            popSeed = state.seed;
        end
        state.catalogs.(catKey) = generatePopulation_(sky, popSeed, C.CatalogSize, C.FluxSafeMax);
    end
    pop = state.catalogs.(catKey);
    nCat = numel(pop.ra);
    ts = int64(posixtime(datetime('now')));

    rng(state.seed + visitIndex + mod(sum(double(sky.name)), 10000));

    if visitIndex == 0
        idx = randperm(nCat, min(n, nCat));
        tbl = rowsToTable_(pop.ra(idx), pop.dec(idx), pop.magnitude(idx), ...
            pop.flux(idx), pop.flags(idx), ts, fieldNameTag, sky.name);
        state.catalogs.(catKey) = pop;
        return;
    end

    idx = randperm(nCat, min(n, nCat));
    [ra, dec, magnitude, flux, flags] = applyRevisitNoise_(pop, idx, C.FluxSafeMax);
    [magnitude, flux] = injectVariableStars_(magnitude, flux, pop, idx, C.FluxSafeMax, C.VariableStarFraction);
    [ra, dec, magnitude, flux, flags] = injectTransients_(ra, dec, magnitude, flux, flags, n, C.FluxSafeMax, C.TransientFraction);
    [ra, dec, magnitude, flux, flags] = injectArtifacts_(ra, dec, magnitude, flux, flags, n, C.ArtifactFraction);

    tbl = rowsToTable_(ra, dec, magnitude, flux, flags, ts, fieldNameTag, sky.name);
    state.catalogs.(catKey) = pop;
end


function [ra, dec, magnitude, flux, flags] = applyRevisitNoise_(pop, idx, fluxMax)
%APPLYREVISITNOISE_  Astrometric jitter and photometric noise on catalog subset.

    noiseDeg = 0.4 / 3600.0;
    ra = mod(pop.ra(idx) + randn(numel(idx), 1) * noiseDeg, 360.0);
    dec = max(-89.9, min(89.9, pop.dec(idx) + randn(numel(idx), 1) * noiseDeg));
    photNoise = single(randn(numel(idx), 1) * 0.003);
    magnitude = pop.magnitude(idx) + photNoise;
    flux = single(min(fluxMax, max(0.01, double(pop.flux(idx)) .* 10 .^ (-double(photNoise) / 2.5))));
    flags = pop.flags(idx);
end


function [magnitude, flux] = injectVariableStars_(magnitude, flux, pop, idx, fluxMax, fraction)
%INJECTVARIABLESTARS_  Mark ~fraction of rows as photometrically CHANGED.

    nVariable = max(1, round(numel(idx) * fraction));
    varIdx = randperm(numel(idx), nVariable);
    deltaMag = single((randi([0 1], nVariable, 1) * 2 - 1) .* (0.1 + rand(nVariable, 1) * 0.7));
    magnitude(varIdx) = magnitude(varIdx) + deltaMag;
    flux(varIdx) = single(min(fluxMax, max(0.01, double(pop.flux(idx(varIdx))) ...
        .* 10 .^ (-double(deltaMag) / 2.5))));
end


function [ra, dec, magnitude, flux, flags] = injectTransients_(ra, dec, magnitude, flux, flags, n, fluxMax, fraction)
%INJECTTRANSIENTS_  Append ~fraction of batch size as NEW transient detections.

    nNew = max(0, round(n * fraction));
    if nNew <= 0
        return;
    end

    raNew = min(ra) + rand(nNew, 1) * (max(ra) - min(ra));
    decNew = min(dec) + rand(nNew, 1) * (max(dec) - min(dec));
    magNew = single(18.5 + rand(nNew, 1) * 3.0);
    fluxNew = single(min(fluxMax, max(0.1, 10 .^ ((25.0 - double(magNew)) / 2.5))));
    flagsNew = zeros(nNew, 1, 'uint32');
    ra = [ra; raNew];
    dec = [dec; decNew];
    magnitude = [magnitude; magNew];
    flux = [flux; fluxNew];
    flags = [flags; flagsNew];
end


function [ra, dec, magnitude, flux, flags] = injectArtifacts_(ra, dec, magnitude, flux, flags, n, fraction)
%INJECTARTIFACTS_  Append ~fraction of batch size as invalid-coordinate artifacts.

    nArt = max(0, round(n * fraction));
    if nArt <= 0
        return;
    end

    raArt = 360.01 + rand(nArt, 1) * 359.99;
    decArt = -90 + rand(nArt, 1) * 180;
    magArt = single(14.0 + rand(nArt, 1) * 6.0);
    fluxArt = single(10 + rand(nArt, 1) * 190);
    flagsArt = zeros(nArt, 1, 'uint32');
    ra = [ra; raArt];
    dec = [dec; decArt];
    magnitude = [magnitude; magArt];
    flux = [flux; fluxArt];
    flags = [flags; flagsArt];
end


function tbl = rowsToTable_(ra, dec, magnitude, flux, flags, ts, fieldNameTag, skyName)
%ROWSTOTABLE_  Assemble service insert table from column vectors.

    n = numel(ra);
    timestamp = repmat(ts, n, 1);
    if isempty(fieldNameTag)
        fieldCol = repmat({['matlab_' skyName]}, n, 1);
    else
        fieldCol = repmat({fieldNameTag}, n, 1);
    end
    tbl = table(ra, dec, magnitude, flux, flags, timestamp, fieldCol, ...
        'VariableNames', {'ra', 'dec', 'magnitude', 'flux', 'flags', 'timestamp', 'field_name'});
end

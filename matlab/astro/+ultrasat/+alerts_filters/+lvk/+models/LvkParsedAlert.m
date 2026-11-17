%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% File        : +ultrasat/+alerts_filters/+lvk/+models/LvkParsedAlert.m
% Author      : Chen Tishler
% Created     : 09/02/2026
% Updated     : 15/11/2026
% Description : LVK parsed alert: slim JSON (alert + derived) or legacy flat fields.
%==========================================================================

classdef LvkParsedAlert
    % Hydrated LVK alert for MATLAB filters (lvk_filter_*).
    %
    % On disk, parsed_alert.json is slim: top-level identity and times, plus nodes
    % alert (GraceDB notice minus skymap), fits_header, and derived (Python-computed).
    % Filters read flat properties (prob_bns, far_per_year, ...); fromStruct fills
    % those from alert + derived, or from legacy flat JSON.
    %
    % Load paths (first match wins):
    %   1) s.alert present — current slim schema
    %   2) raw_fields.lvc_notice — transitional GW share files
    %   3) else — legacy flat top-level fields (pre-slim; do not emit from Python)

    properties
        % ------------------------- Identity
        alert_id string = ""
        superevent_id string = ""
        alert_type string = ""

        % ------------------------- Timing (UTC)
        time_created datetime = NaT
        event_time datetime = NaT

        % ------------------------- Classification probabilities (0..1)
        prob_bns double = NaN
        prob_nsbh double = NaN
        prob_bbh double = NaN
        prob_terrestrial double = NaN

        % ------------------------- Event properties (0..1)
        has_ns double = NaN
        has_remnant double = NaN
        has_mass_gap double = NaN

        % ------------------------- Rates and metadata
        significant logical = false
        group string = ""
        far_hz double = NaN
        far_per_year double = NaN

        % ------------------------- Localization
        skymap_path string = ""
        localization_area_deg2 double = NaN

        % ------------------------- Embedded FITS header (from notice skymap decode)
        fits_header struct = struct()

        % ------------------------- Slim JSON nodes (optional, after load)
        alert struct = struct()
        derived struct = struct()

        % ------------------------- Legacy / extended fields
        instruments string = strings(0,1)
        pipeline string = ""
        search string = ""
        raw_fields struct = struct()

        % ------------------------- Parser metadata
        parsed_time datetime = NaT
    end


    methods  % Constructor
        function obj = LvkParsedAlert(varargin)
            % Constructor with name=value pairs for unit tests and tooling.

            if mod(nargin, 2) ~= 0
                error("LvkParsedAlert:Constructor", "Arguments must be name/value pairs");
            end

            for i = 1:2:nargin
                name = varargin{i};
                value = varargin{i+1};

                if isprop(obj, name)
                    obj.(name) = value;
                else
                    error("LvkParsedAlert:InvalidProperty", "Unknown property: %s", name);
                end
            end
        end

        function result = toJsonString(obj)
            % Converts the object to a JSON string (flat properties, not slim schema).
            s = struct(obj);
            result = jsonencode(s, 'PrettyPrint', true);
        end

        function saveToJsonFile(obj, file_path)
            % Saves the object to a JSON file using flat property serialization.
            result = obj.toJsonString();
            fid = fopen(file_path, 'w');
            fwrite(fid, result, 'char');
            fclose(fid);
        end
    end


  methods(Static)
        function obj = fromJsonString(jsonString)
            % Parse JSON text and build LvkParsedAlert.
            data = jsondecode(jsonString);
            obj = ultrasat.alerts_filters.lvk.models.LvkParsedAlert.fromStruct(data);
        end

        function obj = loadFromJsonFile(file_path)
            % Load parsed_alert.json from disk; supports slim, transitional, or legacy shape.
            txt = fileread(file_path);
            obj = ultrasat.alerts_filters.lvk.models.LvkParsedAlert.fromJsonString(txt);
        end

        function obj = fromStruct(s)
            % Map jsondecode struct to LvkParsedAlert; pick schema branch then hydrate.

            obj = ultrasat.alerts_filters.lvk.models.LvkParsedAlert();

            % Top-level identity (not inside alert node).
            obj.alert_id = string(getfield(s, "alert_id", ""));
            obj.superevent_id = string(getfield(s, "superevent_id", ""));
            obj.alert_type = string(getfield(s, "alert_type", ""));

            % Top-level times (MATLAB-friendly strings from Python dump).
            obj.time_created = parse_datetime(getfield(s, "time_created", []));
            obj.event_time = parse_datetime(getfield(s, "event_time", []));
            obj.parsed_time = parse_datetime(getfield(s, "parsed_time", []));

            % Embedded FITS header when skymap was decoded at parse time.
            if isfield(s, "fits_header") && isstruct(s.fits_header)
                obj.fits_header = s.fits_header;
            else
                obj.fits_header = struct();
            end

            % Branch A: slim parsed_alert (alert + derived).
            if isfield(s, "alert") && isstruct(s.alert)
                obj.alert = s.alert;
                if isfield(s, "derived") && isstruct(s.derived)
                    obj.derived = s.derived;
                else
                    obj.derived = struct();
                end
                obj = hydrateFromAlertAndDerived(obj);
                return;
            end

            % Branch B: transitional raw_fields.lvc_notice on older share files.
            if isfield(s, "raw_fields") && isstruct(s.raw_fields) && ...
                    isfield(s.raw_fields, "lvc_notice") && isstruct(s.raw_fields.lvc_notice)
                obj.alert = s.raw_fields.lvc_notice;
                obj.derived = buildDerivedFromRawFields(s);
                obj.raw_fields = s.raw_fields;
                obj = hydrateFromAlertAndDerived(obj);
                return;
            end

            % Branch C: legacy flat JSON (prob_bns, far_per_year, ... at top level).
            obj = hydrateLegacyFlatFields(obj, s);
        end
    end
end


% =================================================================
%                         Hydration helpers
% =================================================================

function obj = hydrateFromAlertAndDerived(obj)
    % Fill flat filter fields from obj.alert (LVC notice) and obj.derived (Python extras).
    % event.classification/properties live in alert; FAR and skymap areas prefer derived.

    % LVC event block (classification, far Hz, instruments, ...).
    event = struct();
    if isstruct(obj.alert) && isfield(obj.alert, "event") && isstruct(obj.alert.event)
        event = obj.alert.event;
    end

    % event.classification.* -> prob_* (json null -> NaN via scalarFinite).
    obj.prob_bns = classificationProb(event, "BNS");
    obj.prob_nsbh = classificationProb(event, "NSBH");
    obj.prob_bbh = classificationProb(event, "BBH");
    obj.prob_terrestrial = classificationProb(event, "Terrestrial");

    % event.properties.* -> has_* .
    obj.has_ns = propertyProb(event, "HasNS");
    obj.has_remnant = propertyProb(event, "HasRemnant");
    obj.has_mass_gap = propertyProb(event, "HasMassGap");

    % event metadata strings and significant flag.
    obj.significant = false;
    if isfield(event, "significant")
        val = event.significant;
        if islogical(val) && isscalar(val)
            obj.significant = val;
        elseif isnumeric(val) && isscalar(val) && isfinite(val)
            obj.significant = logical(val ~= 0);
        end
    end

    if isfield(event, "group") && ~isempty(event.group)
        obj.group = string(event.group);
    end

    if isfield(event, "instruments")
        obj.instruments = string(event.instruments);
    end
    if isfield(event, "pipeline")
        obj.pipeline = string(event.pipeline);
    end
    if isfield(event, "search")
        obj.search = string(event.search);
    end

    % FAR: derived.far_hz / derived.far_per_year, else event.far (Hz) converted.
    far_hz = NaN;
    if isfield(obj.derived, "far_hz")
        far_hz = scalarFinite(getfieldStruct(obj.derived, "far_hz"));
    elseif isfield(event, "far")
        far_hz = scalarFinite(getfieldStruct(event, "far"));
    end
    obj.far_hz = far_hz;

    far_per_year = NaN;
    if isfield(obj.derived, "far_per_year")
        far_per_year = scalarFinite(getfieldStruct(obj.derived, "far_per_year"));
    elseif isfinite(far_hz)
        far_per_year = far_hz * secondsPerYear();
    end
    obj.far_per_year = far_per_year;

    % Credible region size: derived.localization_area_deg2, else area_90 or area_50.
    loc = NaN;
    if isfield(obj.derived, "localization_area_deg2")
        loc = scalarFinite(getfieldStruct(obj.derived, "localization_area_deg2"));
    elseif isfield(obj.derived, "area_90")
        loc = scalarFinite(getfieldStruct(obj.derived, "area_90"));
    elseif isfield(obj.derived, "area_50")
        loc = scalarFinite(getfieldStruct(obj.derived, "area_50"));
    end
    obj.localization_area_deg2 = loc;

    % Prob map path for planner/filter flags (skymap.mat UNC in derived).
    skymap = "";
    if isfield(obj.derived, "skymap_path") && strlength(string(obj.derived.skymap_path)) > 0
        skymap = string(obj.derived.skymap_path);
    elseif isfield(obj.derived, "prob_map_file")
        skymap = string(obj.derived.prob_map_file);
    end
    obj.skymap_path = string(ultrasat.tools.pathForJson(skymap));
end


function derived = buildDerivedFromRawFields(s)
    % Assemble a derived-like struct from transitional JSON (raw_fields + flat tops).
    % Copies skymap areas, paths, and FAR from raw_fields and legacy top-level keys.

    derived = struct();
    rf = s.raw_fields;

    % Operational fields that lived under raw_fields before slim schema.
    keys = {"gracedb_event_id", "area_50", "area_80", "area_90", ...
        "prob_map_file", "stage_folder", "source_file"};
    for k = 1:numel(keys)
        name = keys{k};
        if isfield(rf, name)
            derived.(name) = rf.(name);
        end
    end

    % FAR and localization duplicated at top level on transitional files.
    if isfield(s, "localization_area_deg2")
        derived.localization_area_deg2 = s.localization_area_deg2;
    end
    if isfield(s, "far_hz")
        derived.far_hz = s.far_hz;
    end
    if isfield(s, "far_per_year")
        derived.far_per_year = s.far_per_year;
    end
    if isfield(s, "skymap_path")
        derived.skymap_path = s.skymap_path;
    end
end


function obj = hydrateLegacyFlatFields(obj, s)
    % Read flat top-level fields only; pre-slim schema — not written by current Python dump.

    % Classification and property probabilities at top level.
    obj.prob_bns = scalarFinite(getfield(s, "prob_bns", NaN));
    obj.prob_nsbh = scalarFinite(getfield(s, "prob_nsbh", NaN));
    obj.prob_bbh = scalarFinite(getfield(s, "prob_bbh", NaN));
    obj.prob_terrestrial = scalarFinite(getfield(s, "prob_terrestrial", NaN));

    obj.has_ns = scalarFinite(getfield(s, "has_ns", NaN));
    obj.has_remnant = scalarFinite(getfield(s, "has_remnant", NaN));
    obj.has_mass_gap = scalarFinite(getfield(s, "has_mass_gap", NaN));

    % Rates, localization, and remaining metadata.
    if isfield(s, "significant") && islogical(s.significant)
        obj.significant = s.significant;
    elseif isfield(s, "significant") && isnumeric(s.significant)
        obj.significant = logical(s.significant);
    end
    obj.group = string(getfield(s, "group", ""));

    obj.far_hz = scalarFinite(getfield(s, "far_hz", NaN));
    obj.far_per_year = scalarFinite(getfield(s, "far_per_year", NaN));

    obj.skymap_path = string(ultrasat.tools.pathForJson(getfield(s, "skymap_path", "")));
    obj.localization_area_deg2 = scalarFinite(getfield(s, "localization_area_deg2", NaN));

    obj.instruments = string(getfield(s, "instruments", strings(0,1)));
    obj.pipeline = string(getfield(s, "pipeline", ""));
    obj.search = string(getfield(s, "search", ""));
    obj.raw_fields = getfield(s, "raw_fields", struct());
end


function v = classificationProb(event, className)
    % Map event.classification.(className) to double; jsondecode null -> [] -> NaN.

    v = NaN;
    if ~isstruct(event) || ~isfield(event, "classification")
        return;
    end
    cls = event.classification;
    if ~isstruct(cls) || ~isfield(cls, className)
        return;
    end
    v = scalarFinite(getfieldStruct(cls, className));
end


function v = propertyProb(event, propName)
    % Map event.properties.(propName) to double; missing or null -> NaN.

    v = NaN;
    if ~isstruct(event) || ~isfield(event, "properties")
        return;
    end
    props = event.properties;
    if ~isstruct(props) || ~isfield(props, propName)
        return;
    end
    v = scalarFinite(getfieldStruct(props, propName));
end


function v = scalarFinite(x)
    % Coerce jsondecode scalars (including null -> []) to finite double or NaN.

    if isempty(x)
        v = NaN;
        return;
    end
    if isstring(x) || ischar(x)
        v = NaN;
        return;
    end
    if isnumeric(x) && isscalar(x) && isfinite(x)
        v = double(x);
        return;
    end
    v = NaN;
end


function y = secondsPerYear()
    % Seconds per year consistent with Python LVC parser constant.
    y = 31557600.0;
end


function v = getfieldStruct(s, name)
    % Read struct field without default when caller already checked isfield.
    v = s.(name);
end


function v = getfield(s, name, default)
    % Get struct field or default.

    if isfield(s, name)
        v = s.(name);
    else
        v = default;
    end
end


function dt = parse_datetime(x)
    % Parse MATLAB or ISO datetime strings as UTC.

    if isempty(x)
        dt = NaT('TimeZone', 'UTC');
    elseif isdatetime(x)
        dt = x;
        if isempty(dt.TimeZone)
            dt.TimeZone = 'UTC';
        end
    else
        dt = ultrasat.tools.parseUtcDatetime(x);
    end
end
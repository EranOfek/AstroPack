function debug_SkyExposureTracker()
    % Debug file for testing the SkyExposureTrackerClient.

    % Define API URL and create client
    client = ultrasat.api.future.SkyExposureTrackerClient();
    client.ApiUrl = 'http://127.0.0.1:8216'; % Update with the actual URL

    % Define table name for debugging
    table_name = 'mission.sky_exposure_debug_matlab';

    % HEALPix indices filter
    fprintf("Selecting rows by HEALPix indices...\n");
    response = client.select(table_name, [1,2,3], [], [], false);
    disp(response);


    % Run debug functions
    debugInitTable(client, table_name);
    debugUpdate(client, table_name);
    debugSelect(client, table_name);
    debugSelectAll(client, table_name);
end

% -------------------------------------------------------------------------

function debugInitTable(client, table_name)
    % Debug: Initialize a Sky Exposure Tracker table.

    fprintf("Debug: Initializing table '%s'...\n", table_name);

    % Parameters
    healpix_rows = 1000;
    healpix_level = 5;
    healpix_indices = 1:10;

    % Call the initTable function
    response = client.initTable(table_name, healpix_rows, healpix_level, healpix_indices);
    disp(response);

    % Check if the response is successful
    if response.ok
        fprintf("Table '%s' initialized successfully.\n", table_name);
    else
        fprintf("Failed to initialize table '%s'.\n", table_name);
    end
end

% -------------------------------------------------------------------------

function debugUpdate(client, table_name)
    % Debug: Update rows in the Sky Exposure Tracker table.

    fprintf("Debug: Updating rows in table '%s'...\n", table_name);

    % Parameters
    healpix_indices = 1:5; % Example HEALPix indices
    duration = 30.0;       % Duration in seconds
    timestamp = datetime('2024-12-26 15:14:34', 'TimeZone', 'UTC'); % Example timestamp

    % Call the update function
    response = client.update(table_name, healpix_indices, duration, timestamp);
    disp(response);

    % Check if the response is successful
    if response.ok
        fprintf("Rows updated successfully in table '%s'.\n", table_name);
    else
        fprintf("Failed to update rows in table '%s'.\n", table_name);
    end
end

% -------------------------------------------------------------------------

function debugSelect(client, table_name)
    % Debug: Select rows from the Sky Exposure Tracker table.

    fprintf("Debug: Selecting rows from table '%s'...\n", table_name);

    % HEALPix indices filter
    fprintf("Selecting rows by HEALPix indices...\n");
    response = client.select(table_name, 1:3, [], [], false);
    disp(response);

    % Timestamp range filter
    fprintf("Selecting rows by timestamp range...\n");
    start_timestamp = datetime('2024-12-26 12:00:00', 'TimeZone', 'UTC');
    end_timestamp = datetime('2026-12-26 14:00:00', 'TimeZone', 'UTC');
    response = client.select(table_name, [], start_timestamp, end_timestamp, false);
    disp(response);

    % Combined filter
    fprintf("Selecting rows by HEALPix and timestamp range...\n");
    response = client.select(table_name, 1:2, start_timestamp, end_timestamp, false);
    disp(response);
end

% -------------------------------------------------------------------------

function debugSelectAll(client, table_name)
    % Debug: Select all rows with filters from the Sky Exposure Tracker table.

    fprintf("Debug: Selecting all rows with filters from table '%s'...\n", table_name);

    % HEALPix indices filter
    fprintf("Selecting all rows by HEALPix indices...\n");
    response = client.select(table_name, 1:3, [], [], true);
    disp(response);

    % Timestamp range filter
    fprintf("Selecting all rows by timestamp range...\n");
    start_timestamp = datetime('2024-12-26 12:00:00', 'TimeZone', 'UTC');
    end_timestamp = datetime('2026-12-26 14:00:00', 'TimeZone', 'UTC');
    response = client.select(table_name, [], start_timestamp, end_timestamp, true);
    disp(response);

    % Combined filter
    fprintf("Selecting all rows by HEALPix and timestamp range...\n");
    response = client.select(table_name, 1:2, start_timestamp, end_timestamp, true);
    disp(response);
end

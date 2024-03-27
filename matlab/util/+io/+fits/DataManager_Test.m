% Create an instance of DataManager
dataManager = io.fits.DataManager();

% Example usage with dummy data and a 5-second duration before release
imageData = rand(100); % Example image data
headerData = {'Header1', 'Header2'}; % Example header data
dataKeeper = io.fits.DataKeeper(imageData, headerData, 5); % 5 seconds until data can be released

% Add the DataKeeper to the DataManager
dataManager = dataManager.addDataKeeper(dataKeeper);

% Periodically call scanAndRelease to check and release expired data
% This could be inside a timer loop or triggered by some event
while true
    pause(1); % Pause for 1 second before each scan to reduce CPU load
    dataManager.scanAndRelease();
end


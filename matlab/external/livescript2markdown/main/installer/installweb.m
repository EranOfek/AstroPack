TEMP_INST.uuid = 'c5ce6999-1e66-4758-81ee-567db8689a9b';
TEMP_INST.url = matlab.addons.repositories.FileExchangeRepository().getAddonURL(TEMP_INST.uuid);
TEMP_INST.fname = websave(tempname + ".mltbx", TEMP_INST.url);
open(TEMP_INST.fname);
clear TEMP_INST
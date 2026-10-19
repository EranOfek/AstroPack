function [AllStreaks] = saveProductStreak(AI, FN, Args)
    % Save streaks data stored in AstroImage object
    % Input  : - An AstroImage object.
    %          - An AstroFileName object with file name to save.
    %          * ...,key,val,... 
    %            'Save' - A logical indicating if to save the product.
    %                   Default is true.
    %            'KeyOrigSec' - Header keyword of the crop section in the
    %                   full calibrated image. Default is 'ORIGSEC'.
    %            'KeyLightSec' - Header keyword of the calibrated image
    %                   section in the raw image. Default is 'LIGHTSEC'.
    %            'KeyCropID' - Header keyword of the crop ID.
    %                   Default is 'CROPID'.
    % Output : - A structure array of streaks from all the images.
    %            Fields: Streaks, Epoch, Crop (column index in AI),
    %            and from the crop header: ORIGSEC, LIGHTSEC
    %            ([xmin xmax ymin ymax]; NaN(1,4) if missing) and CROPID.
    %            Full-frame row = Y + ORIGSEC(3) - 1;
    %            raw-file row = full-frame row + LIGHTSEC(3) - 1.
    % Author : Eran Ofek (2026 Apr) 
    % Example: imProc.io.saveProductStreak

    arguments
        AI
        FN
        Args.JD     = [];
        Args.Save   = true;
        Args.KeyOrigSec  = 'ORIGSEC';
        Args.KeyLightSec = 'LIGHTSEC';
        Args.KeyCropID   = 'CROPID';
    end

    [Nep, Nsub] = size(AI);
    K = 0;
    AllStreaks = struct('Streaks',[], 'Epoch',[], 'Crop',[], 'ORIGSEC',[], 'LIGHTSEC',[], 'CROPID',[]);
    for Isub=1:1:Nsub
        for Iep=1:1:Nep
            if ~isempty(AI(Iep, Isub).Streaks)
                K = K + 1;
                AllStreaks(K).Streaks = AI(Iep, Isub).Streaks;
                AllStreaks(K).Epoch  = Iep;
                AllStreaks(K).Crop   = Isub;
                % crop position in the full frame (issue #1415)
                Head = AI(Iep, Isub).HeaderData;
                AllStreaks(K).ORIGSEC  = readSec(Head, Args.KeyOrigSec);
                AllStreaks(K).LIGHTSEC = readSec(Head, Args.KeyLightSec);
                AllStreaks(K).CROPID   = Head.getVal(Args.KeyCropID, 'UseDict',false);
                % if isempty(Args.JD)
                %     AllStreaks(K).JD = AI(Iep, Isub).julday;
                % else
                %     AllStreaks(K).JD     = Args.JD(Iep, Isub);
                % end
            end
        end
    end

    if K>0 && Args.Save
        % save
        save('-v7.3', FN.genFull, "AllStreaks");
    end



end

%%% local function
function Sec = readSec(Head, Key)
    % Read a CCDSEC-like keyword as a 1x4 vector; NaN(1,4) if missing
    Sec = Head.getVal(Key, 'UseDict',false, 'ReadCCDSEC',true);
    if ~isnumeric(Sec) || numel(Sec)~=4
        Sec = NaN(1,4);
    end
    Sec = double(Sec(:).');
end

function JD = gaiaEpoch(Image, ApplyPM)
    % The epoch [JD] to move the Gaia sources to for an image (issue #1348).
    %   Moved from the local function of pipeline.last.pipes.pipelineII.
    % Input  : - An AstroImage.
    %          - Apply the proper motion (true) or not (false).
    % Output : - The image JD from its header; [] if ApplyPM is false or
    %            the header has no usable JD (no proper motion is then
    %            applied).
    % Author : Alexander Gioffe (Sep 2026)
    % Example: JD = imProc.cat.gaiaEpoch(AI, true)

    JD = [];
    if ApplyPM
        try
            Val = Image.julday;
            if ~isempty(Val) && isfinite(Val(1))
                JD = Val(1);
            end
        catch
            JD = [];   % no readable JD - no proper motion
        end
    end
end

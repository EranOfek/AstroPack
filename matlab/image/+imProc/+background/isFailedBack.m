function Result = isFailedBack(Obj)
    % Check which elements have a failed background estimation.
    %     imProc.background.backVar fills the Back and Var of an element whose
    %     estimation failed with NaN (issue #1223), and returns its index in
    %     its second output. That index is unusable by the pipeline callers,
    %     which pass subsets (Obj(Flag) = backVar(Obj(Flag),...)) or a single
    %     element inside parfor, so the failure is recognized here from the
    %     state of the object itself.
    %     The NaN fill covers the whole array, and the operations applied to
    %     Back/Var downstream are additive, so the marker survives the entire
    %     extraction and astrometry chain.
    % Input  : - An AstroImage object (multi elements supported).
    % Output : - An array of logicals, the size of the input object, which is
    %            true for the elements whose background estimation failed.
    %            An element which was never given a background (empty Back) is
    %            not a failure and is false.
    % Author : A.M. Krassilchtchikov (Aug 2026)
    % Example: Flag = imProc.background.isFailedBack(AllSI);

    arguments
        Obj AstroImage
    end

    Result = false(size(Obj));
    for Iobj=1:1:numel(Obj)
        Back = Obj(Iobj).BackData.Data;
        % The first element is checked before the whole array, so a healthy
        % image costs one comparison instead of a full scan of the image.
        if ~isempty(Back) && isnan(Back(1)) && all(isnan(Back), 'all')
            Result(Iobj) = true;
        end
    end
end

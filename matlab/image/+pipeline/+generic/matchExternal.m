function [Result] = matchExternal(Obj, Args)
    % Match sources in AstroImage-like objects to a suite of external catalogs
    %     Matching may include calling the following functions:
    %       imProc.match.matchStars
    %       imProc.match.match_catsHTM_multi
    %       imProc.match.match_catsHTMmerged
    %       imProc.match.match2solarSystem
    % Input  : - An AstroImage/AstroZOGY/AstroDiff object.
    %          * ...,key,val,... 
    %            'matchStars' - Logical indicating if to call the
    %                   imProc.match.matchStars function.
    %                   Default is true.
    %            'matchStarsArgs' - A cell array of additional arguments to
    %                   pass to the imProc.match.matchStars function.
    %                   Default is {}.
    %            'match_catsHTM_multi' - Logical indicating if to call the
    %                   imProc.match.match_catsHTM_multi function (match to
    %                   multiple catsHTM catalogs with flexible per-catalog criteria).
    %                   Default is true.
    %            'match_catsHTM_multiArgs' - A cell array of additional
    %                   arguments to pass to the imProc.match.match_catsHTM_multi
    %                   function. Default is {}.
    %            'match_catsHTMmerged' - Logical indicating if to call the
    %                   imProc.match.match_catsHTMmerged function (matching to
    %                   a merged/union view of catsHTM catalogs).
    %                   Default is true.
    %            'match_catsHTMmergedArgs' - A cell array of additional
    %                   arguments to pass to the imProc.match.match_catsHTMmerged
    %                   function. Default is {}.
    %            'match2solarSystem' - Logical indicating if to call the
    %                   imProc.match.match2solarSystem function (matching to
    %                   Solar-System ephemeris catalogs).
    %                   Default is true.
    %            'match2solarSystemArgs' - A cell array of additional
    %                   arguments to pass to the imProc.match.match2solarSystem
    %                   function. Default is {}.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of the input catalog that will be returned as
    %                   the output argument. Default is false.
    % Output : - The input object with the updated catalog with matched
    %            sources information.
    % Author : Eran Ofek (2025 Oct) 
    % Example: AI=imProc.match.matchExternal(AI);

    arguments
        Obj
        Args.matchStars              = true;
        Args.matchStarsArgs          = {};
        Args.match_catsHTM_multi     = true;
        Args.match_catsHTM_multiArgs = {};
        Args.match_catsHTMmerged     = true;
        Args.match_catsHTMmergedArgs = {};
        Args.match2solarSystem       = true;
        Args.match2solarSystemArgs   = {};
        Args.CreateNewObj            = false;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = copy;
    end

    if Args.matchStars
        Result = imProc.match.matchStars(Result, Args.matchStarsArgs{:});
    end

    if Args.match_catsHTM_multi
        Result = imProc.match.match_catsHTM_multi(Result, Args.match_catsHTM_multiArgs{:});
    end

    if Args.match_catsHTMmerged
        Result = imProc.match.match_catsHTMmerged(Result, Args.match_catsHTMmergedArgs{:});
    end

    if Args.match2solarSystem
        Result = imProc.match.match2solarSystem(Result, Args.match2solarSystemArgs{:});
    end

end

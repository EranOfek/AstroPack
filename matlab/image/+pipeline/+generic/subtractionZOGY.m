function [Result] = subtractionZOGY(AI, Args)
    % Image subtraction and transients finding
    %   Replacing: pipeline.last.transients.runTransientsPipe
    % Input  : - An AstroImage object
    %          * ...,key,val,... 
    %            'RefAI' - Optional AstroImage containing reference images.
    %                   This can be a scalar (one ref for all input
    %                   images), or a an array (ref per new image).
    %                   If empty, then will attempt to read ref image from
    %                   disk (see below).
    %                   Default is [].
    %            '
    %           
    % Output : - 
    % Author : Eran Ofek (2025 Oct) 
    % Example: 

    arguments
        AI
        Args.RefAI                 = [];
        Args.MinmalOverlapFraction = 0.1;
        Args.MinimalOverlapNpix    = 3e5;  % about 10% of the LAST cropped image
        Args.DoGabor               = true;
        Args.DoScorr               = true;
        Args.DoTranslient          = true;
        Args.DoDSDF                = true;
        Args.DoDelta               = true;
        Args.FindTransients        = true;
        Args.MatchExternal         = true;
        Args.LogObj                = [];
    end

    try
        % number of images
        Nai = numel(AI);
    
        if ~isempty(Args.RefAI)
            % user provided RefAI
            Nref = numel(Args.RefAI);
        end
    
        % allocate Info struct
        % The Info struct contains information on the sucess of different steps
        Info.RefImageExist = true(Nai,1);
        Info.NewRefOverlapNpix   = nan(Nai,1);
        Info.NoNewRefOverlapNpix = nan(Nai,1); % new w/no-overlap bit overlap with ref.
        Info.OverlapUsed         = nan(Nai,1); % like NoNewRefOverlapNpix but can be
                                               % set to 0 if no subtraction produced.
    
    
        for Iai=1:1:Nai
            % populate Args.RefAI
            if isempty(Args.RefAI)
                % Read reference image corresponding to current AI
                HERE: Args.RefAI = retrieveRefImage(...)
    
                % only one ref image
                Iref = 1;
            else
                % index of corresponding ref image
                Iref = min(Iai, Nref);
            end
           
            % Create AzstroZOGY object
            AD(Iai) = AstroZOGY;
    
            % check that Ref image exist
            if Args.RefAI(Iref).isemptyImage
                % Ref image doesn't exist
                Info.RefImageExist(Iai) = false; 
    
                % HERE: do we want to write to log?
            else
                % populate the AstroZOGY object
                AD(Iai).Ref = Args.RefAI(Iref);
                AD(Iai).New = AI(Iai);
        
                % registration
                AD(Iai).register;
        
                % Estimate area of overlap between new and ref
                % do it for both full overlap and overlap of regions with
                % Overlap=false bit.
                % The following info is kept:
                %   Info.NewRefOverlapNpix
                %   Info.NoNewRefOverlapNpix - new w/no-overlap bit overlap
                %                                   with ref.
                %   Info.OverlapUsed - like NoNewRefOverlapNpix but can be
                %                                   set to 0 if no subtraction produced.
                Here: Info.NewRefOverlapNpix(Iai) = ...
        
                %SizeNew = size(AI(Iai).ImageData.Image);
                MinimalOverlapNpix = Args.MinimalOverlapFraction.*numel(AI(Iai).ImageData.Image);

                % Check if mininmal overlap exist
                if OverlapInfo(Iai).NoNewRefOverlapNpix>MinimalOverlapNpix
                    % Estimate backround and variance of New and Ref
                    AD(Iai).estimateBackVar;
                    % Estimate zero points
                    AD(Iai).estimateFnFr;
    
                    % Check quality of FnFr 
                    HERE: 
    
                    % ----- Produce subtraction images -----
                    % Create proper subtraction image D
                    AD(Iai).subtractionD;
                    % Derive S stat image
                    AD(Iai).subtractionS;
                    if Args.DoGabor
                        % Derive Gabor stat image
                        AD(Iai).matchfilterGabor;
                    end
                    if Args.DoScorr
                        % Derive Scorr stat image
                        AD(Iai).subtractionScorr;
                    end
                    if Args.DoTranslient
                        % Derive Z2 stat image
                        AD(Iai).translient;
                    end
                    if Args.DoDelta
                        % score for delta functions
                        HERE:
                    end
                    if Args.DoDSDF
                        % dS/dF
                        HERE:
                    end
    
                    if Args.FindTransients
                        % Find transients
                        HERE: AD(Iai).findTransients;
                    end
                    if Args.MatchExternal
                        % Match with external sources
                        % Extranla catalogs, galaxies, stars, asteroids,...
                        HERE:
                        imProc.match.match2Galaxies(AD(Iai));
                        imProc.match.match2Stars(AD, StarCat); % consider supply StarCat from Args...
                    end
                    
        
                else
                    % not enough overlap pixels to continue with subtraction
                    OverlapInfo(Iai).OverlapUsed = 0;
    
                    HERE:
    
                end % if OverlapInfo(Iai).NoNewRefOverlapNpix<Args.MinimalOverlapNpix
            end % if Args.RefAI(Iref).isemptyImage
        end % for Iai=1:1:Nai

    catch ME
        if isempty(Args.LogObj)
            ME
            error('pipeline.generic.subtractionZOGY failed');
        else
            Msg='pipeline.generic.subtractionZOGY failed';
            Args.LogObj.writeMsg(Msg, LogLevel.Error);
            Args.LogObj.writeMsg(ME, LogLevel.Error);
        end

    end % try/catch

end

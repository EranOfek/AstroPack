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
        Args.RegisterRef           = true;
        % Overlap parameters
        Args.MinmalOverlapFraction = 0.1;
        Args.CheckBits             = {'NaN','Overlap','NearEdge'};
        Args.RefIsRegistered       = true;
        %Args.OutUnits              = 'deg';
        Args.UseMex                = false;

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
        Info = struct('RefImageExist',nan(Nai,1),...
                      'RefImageExist',true(Nai,1),...
                      'Npix',nan(Nai,1),...
                      'OverlapFraction',nan(Nai,1),...
                      'CornersX',nan(Nai,4),...
                      'CornersY',nan(Nai,4),...
                      'CornersRA',nan(Nai,4),...
                      'CornersDec',nan(Nai,4));
        
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
                % register ref to new
                % The ref will have the same size as the new
                AD(Iai).register('RegisterRef',Args.RegisterRef);
        
                % Estimate area of overlap between new and ref
                [Info.OverlapFraction(Iai), Info.Npix(Iai), Corners, CornersWCS] = AD(Iai).overlapArea('CheckBits',Args.CheckBits, 'RefIsRegistered',Args.RefIsRegistered', 'OutUnits','deg', 'UseMex',Args.UseMex);
                Info.CornersX(Iai,:)   = Corners(:,2).';
                Info.CornersY(Iai,:)   = Corners(:,1).';
                Info.CornersRA(Iai,:)  = CornersWCS(:,1).';
                Info.CornersDec(Iai,:) = CornersWCS(:,2).';

                % Check if mininmal overlap exist
                if OverlapInfo(Iai).OverlapFraction>Args.MinmalOverlapFraction
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
                        % Be careful dS/dF is not normalized like S!
                        % require normalization...
                        HERE: check sign of median - maybe can be used to refine Fn/Fr?
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

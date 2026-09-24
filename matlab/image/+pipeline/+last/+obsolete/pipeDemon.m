function pipeDemon(Args)
    %

    arguments
        Args.DataDir     = [];  % superceed CamNumber
        Args.CamNumber   = [];

    end


    if isempty(Args.DataDir) && isempty(Args.CamNumber)
        error('DataDir or CamNumber must be supplied')
    end

    if isempty(Args.CamNumber)
        if isempty(Args.DataDir)
            error('DataDir or CamNumber must be supplied');
        else
            % only DataDir is given

        end
    else
        if isempty(Args.DataDir)
            % only CamNumber is given
        else
            % both DataDir and CamNumber are given - use DataDir

        end
    end



end

function Edges = binEdges(Data, Args)
    % Construct histogram bin edges in data space (linear or logarithmic).
    % Description: Returns a row vector of bin edges in the natural units of
    %              the data. For LogX the edges are logarithmically spaced
    %              (logspace); the consumer is expected to draw the histogram
    %              on a log-scaled axis (see overlayHist). An explicit edge
    %              vector passed as 'Bins' is returned unchanged.
    % Input  : - Data - numeric array used to derive the data range when
    %            'XLim' is not supplied.
    %          * ...,key,val,...
    %            'Bins' - Scalar bin count, or an explicit edge vector
    %                     (returned as-is). Default 30.
    %            'XLim' - [lo hi] data-space limits. Default [] (data range).
    %            'LogX' - Logarithmic edge spacing over positive data.
    %                     Default false.
    % Output : - Edges - 1x(Bins+1) edge vector (or the passed-in vector).
    % Author : photCalib package refactor (2026-05)
    % Example: E = binEdges(V, 'Bins', 40, 'LogX', true);

    arguments
        Data
        Args.Bins         = 30
        Args.XLim         = []
        Args.LogX logical = false
    end

    if ~isscalar(Args.Bins)
        Edges = Args.Bins(:).';
        return;
    end

    V = Data(:);
    V = V(isfinite(V));
    if Args.LogX
        V = V(V > 0);
    end

    if isempty(Args.XLim)
        if isempty(V)
            Lo = 0; Hi = 1;
        else
            Lo = min(V); Hi = max(V);
        end
    else
        Lo = Args.XLim(1);
        Hi = Args.XLim(2);
    end
    if Hi <= Lo
        Hi = Lo + 1;
    end

    if Args.LogX
        Lo = max(Lo, realmin);
        Hi = max(Hi, Lo * (1 + eps));
        Edges = logspace(log10(Lo), log10(Hi), Args.Bins + 1);
    else
        Edges = linspace(Lo, Hi, Args.Bins + 1);
    end
end

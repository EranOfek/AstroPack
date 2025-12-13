function [Line, Mom] = lineProp(AS, Args)
    % Measure line properties in AstroSpec object.
    % Input  : - An AstroSpec object.
    %          * ...,key,val,... 
    %            'LineWave' - Central wavelength of line to measure.
    %            'LineHalfWidth' - [Lower, Upper] half width from line
    %                   center in which to maesure the line properties.
    %            'BackRange' - Ranges of background to use in background
    %                   fitting.
    %            'Method' - Fitting method. Either a function handle or
    %                   string. Allowd strings:
    %                   'fit' - fit a polynomials which orders are listed
    %                           in the 'PolyOrder argument.
    %                   'wmean' - Weighted mean. In this case the returned
    %                           error is the weighted error on the mean (and not
    %                           std).
    %                   Default is 'fit'.
    %            'PolyOrder' - List of polynomials order to fit.
    %                   Default is [0 1].
    %            'MethodArgs' - If method is function_handle, then this is
    %                   a cell array of additionl arguments to pass to the
    %                   function. Default is {}.
    %            'FunStd' - If Method is a function_handle, then this is a
    %                   function to use for calcaulting the background std.
    %                   Default is 'std'.
    %            'FunStdArgs' - A cell array of additional arguments to
    %                   pass to 'FunStd'. Default is {[],1,'omitnan'}.
    % Output : - A structure array of line properties, including:
    %            .LineLum
    %            .LineLumErr
    %            .BackLum
    %            .backLumErr
    %            .EW - defined negative for emission line
    %            .EWErr
    %          - A structure array of additional properties, including:
    %            .X1 - first central moment. calculate relative to range
    %                   center.
    %            .X2 - second central moment.
    % Author : Eran Ofek (2025 Dec) 
    % Example: [L,M]=imProc.spec.measure.lineProp(AS,'LineWave',7500,'BackRange',[7400 7450; 7550 7600]);

    arguments
        AS
        Args.LineWave          = [];
        Args.LineHalfWidth     = [5 5];
        Args.BackRange         = [];

        Args.Method            = 'fit';  % 'wmean' - return weighted error on the maen
        Args.PolyOrder         = [0 1];
        Args.MethodArgs        = {};
        Args.FunStd            = @std;
        Args.FunStdArgs        = {[],1,'omitnan'};

    end

    if isscalar(Args.LineHalfWidth)
        Args.LineHalfWidth = repmat(Args.LineHalfWidth, 1, 2); % Ensure LineHalfWidth is a 1x2 vector
    end
    LineWaveRange = Args.LineWave + [-1 1].*Args.LineHalfWidth;
    Nobj = numel(AS);
    for Iobj=1:1:Nobj
        % for each AstroSpec object
        
        [Line(Iobj), Mom(Iobj)] = imUtil.spec.measure.lineProp(AS(Iobj).Wave, AS(Iobj).Flux, AS(Iobj).FluxErr, LineWaveRange, ...
                                                     'BackWaveRange',Args.BackRange,...
                                                     'Method',Args.Method,...
                                                     'PolyOrder',Args.PolyOrder,...
                                                     'MethodArgs',Args.MethodArgs,...
                                                     'FunStd',Args.FunStd,...
                                                     'FunStdArgs',Args.FunStdArgs);


    end

end

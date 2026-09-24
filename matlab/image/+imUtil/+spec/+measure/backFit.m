function [Wave, Back, Std, Par, ParErr] = backFit(Wave, Flux, Err, Args)
    % Given a 1D spectrum, fit the background level in some range
    % Input  : - Wavelength vector.
    %          - Flux vector.
    %          - Error vector or scalar. If empty, then set to 1.
    %            Default is [].
    %          * ...,key,val,...
    %            'WaveRange' - Wavelength range in which to fit background.
    %                   If empty, then use all. Default is [].
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
    %            'AllWave' - A logical indicating if the return fitted
    %                   background vector for the entire input wave range (true), or only
    %                   for the fitted range given in BackWaverange (false).
    %                   Default is true.
    %
    % Output : - Vector of wavelength in which the background was fitted.
    %          - Vector (or scalar) of background levels per wavelength.
    %          - Vector (or scalar) of background std.
    %          - Par of polynomila fit.
    %          - ParErr of polynomila fit.
    % Author : Eran Ofek (2025 Dec) 
    % Example: [Wave, Back, Std, Par, ParErr] = imUtil.spec.measure.backFit([1:10],randn(1,10))

    arguments
        Wave
        Flux
        Err                    = [];
        Args.BackWaveRange     = [];
        Args.Method            = 'fit';  % 'wmean' - return weighted error on the maen
        Args.PolyOrder         = [0 1];
        Args.MethodArgs        = {};
        Args.FunStd            = @std;
        Args.FunStdArgs        = {[],1,'omitnan'};
        Args.AllWave           = true;
    end

    Wave = Wave(:);
    Flux = Flux(:);
    FluxAll = Flux;
    WaveAll = Wave;

    Nwave = numel(Wave);
    if isempty(Err)
        Err = 1;
    end
    if isscalar(Err)
        Err = Err.*ones(Nwave,1);
    else
        Err = Err(:);
    end
        


    if isempty(Args.BackWaveRange)
        % use all waves
    else
        Ind = tools.array.find_ranges(Wave, Args.BackWaveRange);
        Wave = Wave(Ind);
        Flux = Flux(Ind);
        Err  = Err(Ind);
    end

    Par    = [];
    ParErr = [];
    if isa(Args.Method, 'function_handle')
        Back = Args.Method(Flux, Args.MethodArgs{:});
        Std  = Args.FunStd(Flux, Args.FunStdArgs{:})./sqrt(numel(Wae));
    else

        switch lower(Args.Method)
            case 'fit'
                H = Wave.^Args.PolyOrder;
                [Par, ParErr] = lscov(H, Flux, (1./Err).^2);    

                if Args.AllWave
                    Ha = WaveAll.^Args.PolyOrder;
                    Back = Ha*Par;
                    BackB = H*Par;
                    Std  = std(Flux - BackB, [], 1, 'omitnan');
                else
                    Ha = H;
                    Back = Ha*Par;
                    Std  = std(Flux - Back, [], 1, 'omitnan');
                end
                

            case 'wmean'
                [Back, Std] = tools.math.stat.wmean([Flux, err]);
                
            otherwise
                error('Unknown Method option');
        end
    end

end

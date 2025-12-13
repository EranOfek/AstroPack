function [Result, Mom] = lineProp(Wave, Flux, Err, LineRange, Args)
    % Measure spectral line properties (equivalent width, lum, moments)
    % Input  : - Wavelength vector.
    %          - Flux vector.
    %          - Error vector or scalar. If empty, then set to 1.
    %          - Line range [Min, Max] in which to measure the line
    %            properties.
    %          * ...,key,val,...
    %            'SubBack' - Subtract background before measurments.
    %            'BackWaveRange' - Wavelength range in which to fit background.
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
    % Output : - A structure of line properties, including:
    %            .LineLum
    %            .LineLumErr
    %            .BackLum
    %            .backLumErr
    %            .EW - defined negative for emission line
    %            .EWErr
    %          - A structure of additional properties, including:
    %            .X1 - first central moment. calculate relative to range
    %                   center.
    %            .X2 - second central moment.
    % Author : Eran Ofek (2025 Dec) 
    % Example: Wave=(1:1:100); B=1; Err=0.1; Flux=B+exp(-(Wave-50).^2./8)+randn(size(Wave)).*Err;
    %          Wave=(1:1:100); B=2; Flux=B.*ones(size(Wave)); Flux(41:60)=B+1;
    %          [Result, Mom] = imUtil.spec.measure.lineProp(Wave, Flux, Err, [40 60])

    arguments
        Wave
        Flux
        Err   
        LineRange
        Args.SubBack           = true;

        Args.BackWaveRange     = [];
        Args.Method            = 'fit';  % 'wmean' - return weighted error on the maen
        Args.PolyOrder         = [0 1];
        Args.MethodArgs        = {};
        Args.FunStd            = @std;
        Args.FunStdArgs        = {[],1,'omitnan'};
       
    end

    Wave = Wave(:);
    Flux = Flux(:);

    Nwave = numel(Wave);
    if isempty(Err)
        Err = 1;
    end
    if isscalar(Err)
        Err = Err.*ones(Nwave,1);
    else
        Err = Err(:);
    end
        

    if Args.SubBack
        % measure back
        [~, Back, BackErr] = imUtil.spec.measure.backFit(Wave, Flux, Err,...
                                                    'BackWaveRange',Args.BackWaveRange,...
                                                    'Method',Args.Method,...
                                                    'PolyOrder',Args.PolyOrder,...
                                                    'MethodArgs',Args.MethodArgs,...
                                                    'FunStd',Args.FunStd,...
                                                    'FunStdArgs',Args.FunStdArgs,...
                                                    'AllWave',true);

    else
        Back    = 0;
        BackErr = 0;
    end

    % measure line properties
    IndW = find(Wave>=min(LineRange) & Wave<=max(LineRange));

    Wave = Wave(IndW);
    Flux = Flux(IndW);
    Back = Back(IndW);
    Err  = Err(IndW);


    [Result.LineLum, Result.LineLumErr] = tools.math.integral.trapzErr(Wave, Flux-Back, sqrt(Err.^2+BackErr.^2));
    [Result.BackLum, Result.BackLumErr] = tools.math.integral.trapzErr(Wave, Back, BackErr.*ones(size(Back)));
    FunEW     = (Back - Flux)./Back;
    FunEWErr  = (BackErr.^2.*((Back - Flux)./Back.^2 - 1./Back).^2 + Err.^2./Back.^2).^(1./2);
    [Result.EW, Result.EWErr] = tools.math.integral.trapzErr(Wave, FunEW, FunEWErr);

    if nargout>1
        % calculate moments
        MidWave = 0.5.*(min(Wave) + max(Wave));
        Wave    = Wave - MidWave;
        Mom.X1 = MidWave + sum(Wave.*Flux)./sum(Flux);
        Mom.X2 = sqrt(sum(Wave.^2.*Flux)./sum(Flux));  
        %Mom.X3 = sum(Wave.^3.*Flux)./sum(Flux);
    end


end

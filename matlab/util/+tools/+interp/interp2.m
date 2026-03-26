function [Zout] = interp2(Xin, Yin, Z, Xout, Yout, Method, ExtrapVal)
    % interp2 with additional methods, including fast mex interpolation
    %     This function can be used to call the matlab builtin interp2
    %     function, or additional interp2 mex function (controld via the
    %     Method argument).
    % Input  : - Input X grid (vector or matrix).
    %          - Input Y grid (vector or matrix).
    %          - 2D matrix to interpolate (any type).
    %          - Output X grid (vector or matrix).
    %          - Output Y grid (vector or matrix).
    %          - Method option:
    %            'nearest'|'linear'|'cubic'|'spline'|'makima' (will call
    %            the matlab builtin function interp2, with this method).
    %            'mex_bilinear' - tools.interp.mex.interp2_bilinear_mex
    %            'mex_cubic' - tools.interp.mex.interp2_cubic_mex
    %            'mex_nearest' - tools.interp.mex.interp2_nearest_mex
    %            'mex_lanczos2' - tools.interp.mex.interp2_lanczos2_mex 
    %            'mex_lanczos3' - tools.interp.mex.interp2_lanczos3_mex
    %          - (ExtrapVal) relevant for the interp2 options.
    %            Default is NaN.
    % Output : - Interpolated 2D matrix.
    % Author : Eran Ofek (2026 Feb) 
    % Example: Z=tools.interp.interp2(Xin,Yin,Z,Xout,Yout,'mex_lanczos3');

    arguments
        Xin
        Yin
        Z
        Xout
        Yout
        Method    = 'linear';
        ExtrapVal = NaN;
    end

    if strcmp(Method(1:3),'mex')
        % call astroPack mex functions
        switch Method
            case 'mex_bilinear'
                Zout = tools.interp.mex.interp2_bilinear_mex(Xin, Yin, Z, Xout, Yout);
            case 'mex_cubic'
                Zout = tools.interp.mex.interp2_cubic_mex(Xin, Yin, Z, Xout, Yout);
            case 'mex_nearest'
                Zout = tools.interp.mex.interp2_nearest_mex(Xin, Yin, Z, Xout, Yout);
            case 'mex_lanczos2'
                Zout = tools.interp.mex.interp2_lanczos2_mex(Xin, Yin, Z, Xout, Yout);
            case 'mex_lanczos3'
                Zout = tools.interp.mex.interp2_lanczos3_mex(Xin, Yin, Z, Xout, Yout);
            otherwise
                error('Uknown Method option');
        end

    else
        % call matlab built in interp2
        Zout = intrep2(Xin, Yin, Z, Xout, Yout, Method, ExtrapVal);
    end



end

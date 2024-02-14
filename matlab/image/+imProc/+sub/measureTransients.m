function TranCat = measureTransients(AD, Args)
    %{ 
    For each transient candidate measure further properties.
    These depend on the subtraction process, so a function is applied 
    that is determined by the object class.
    Input   : - An AstroDiff object in which CatData is populated.
                 * ...,key,val,...
              --- AstroZOGY ---
              'HalfSizeTS' - Half size of area on transients positions in 
                test statistic images S2 and Z2. Actual size will be  
                1+2*HalfSizeTS. Used to find peak S2 and Z2 values.
                Default is 5.
              'removeTranslients' - Bool on whether to remove
                transients candidates which score higher in Z2 than S2.
                Default is true.
    Output  : An AstroCatalog with the derived properties added to it.
                These properties may be the following;
                --- AstroZOGY ---
            .S2_TS - Peak value of the S2 test statistic within area
                defined by HalfSizeTS around transient position.
            .S2_Sig - S2_TS converted to Gaussian significance.
            .S2_AIC - S2_TS converted to a loglike value penalized for
                degrees of freedom (TS-2*d.o.f).
            .Z2_TS - Peak value of the Z2 test statistic within area
                defined by HalfSizeTS around transient position.
            .Z2_Sig - Z2_TS converted to Gaussian significance.
            .Z2_AIC - Z2_TS converted to a loglike value penalized for
                degrees of freedom (TS-2*d.o.f).
            .Translient - Binary value on whether transLient model scores
                higher than transient model. 1 for true and 0 for false.
    Author  : Ruslan Konno (Jan 2024)
    Example : imProc.sub.measureTransients(AD)
    %}

    arguments
        AD AstroDiff

        % AstroZOGY
        Args.HalfSizeTS = 5;
        Args.removeTranslients logical = true;
    end
    
    % Get object class and apply corresponding function.
    className = class(AD);

    switch className
        case 'AstroZOGY'
            TranCat = measureTransients_AstroZOGY(AD, ...
                'HalfSizeTS', Args.HalfSizeTS, ...
                'removeTranslients', Args.removeTranslients);
        otherwise
            % TODO: probably give Warning and return empty cat instead
            error('Class not supported.')
    end
    
end

function TranCat = measureTransients_AstroZOGY(AD, Args)
    %{ 
    For each transient candidate measure further properties given they are
    derived via AstroZOGY.
    Input   : - An AstroZOGY object in which CatData is populated.
                 * ...,key,val,...
              'HalfSizeTS' - Half size of area on transients positions in 
                test statistic images S2 and Z2. Actual size will be  
                1+2*HalfSizeTS. Used to find peak S2 and Z2 values.
                Default is 5.
              'removeTranslients' - Bool on whether to remove
                transients candidates which score higher in Z2 than S2.
                Default is true.
    Output  : An AstroCatalog with the derived properties added to it.
                These properties are the following;
            .S2_TS - Peak value of the S2 test statistic within area
                defined by HalfSizeTS around transient position.
            .S2_Sig - S2_TS converted to Gaussian significance.
            .S2_AIC - S2_TS converted to a loglike value penalized for
                degrees of freedom (TS-2*d.o.f).
            .Z2_TS - Peak value of the Z2 test statistic within area
                defined by HalfSizeTS around transient position.
            .Z2_Sig - Z2_TS converted to Gaussian significance.
            .Z2_AIC - Z2_TS converted to a loglike value penalized for
                degrees of freedom (TS-2*d.o.f).
            .Translient - Binary value on whether transLient model scores
                higher than transient model. 1 for true and 0 for false.
    Author  : Ruslan Konno (Jan 2024)
    %}
    arguments
        AD AstroZOGY

        Args.HalfSizeTS = 5;
        Args.removeTranslients logical = true;
    end

    % Get number of objects
    Nobj = numel(AD);

    % Get image (x,y) coordinates of transients candidates
    XPEAK = AD.CatData.Catalog.XPEAK;
    YPEAK = AD.CatData.Catalog.YPEAK;

    % reverse order to initiate Result array with proper size on first 
    % iteration
    for Iobj=Nobj:-1:1
    
        sizeCat = size(AD(Iobj).CatData.Catalog,1);
        
        % If catalog is empty, add empty columns and continue.
        if sizeCat == 0
            TranCat(Iobj) = AD(Iobj).CatData.insertCol([],'Score',...
                {'S2_TS','S2_Sig','S2_AIC','Z2_TS','Z2_Sig','Z2_AIC','Translient'},...
                {'','sig','','','sig','',''});
            continue
        end
        
        % find and save peak TS and corresponding gaussian significance for
        % S2 and Z2 statistics

        % set the S2 and/or Z2 values to NaN for all transients if the TS
        % maps do not exist or are empty, otherwise run process
        if isempty(AD(Iobj).S2)
            S2_TS = nan(Nsrc,1);
            S2_Sig = nan(Nsrc,1);
            S2_AIC = nan(Nsrc,1);
        else
            [S2_TS, S2_Sig, S2_AIC] = process_TS_map(AD(Iobj).S2, ...
                XPEAK, YPEAK, Args.HalfSizeTS, 1);
        end

        if isempty(AD(Iobj).Z2)
            Z2_TS = nan(Nsrc,1);
            Z2_Sig = nan(Nsrc,1);
            Z2_AIC = nan(Nsrc,1);
        else
            [Z2_TS, Z2_Sig, Z2_AIC] = process_TS_map(AD(Iobj).Z2, ...
                XPEAK, YPEAK, Args.HalfSizeTS, 2);
        end

        % See if translient model is preferred over transient model and set
        % bool value, true if translient preferred, false if transient
        % preferred.
        Translient = double(S2_AIC < Z2_AIC);

        % Insert derived properties into AD.CatData catalog
        TranCat(Iobj) = AD(Iobj).CatData.insertCol(...
            cell2mat({S2_TS, S2_Sig, S2_AIC, Z2_TS, Z2_Sig, Z2_AIC, Translient}), 'Score',...
            {'S2_TS','S2_Sig','S2_AIC','Z2_TS','Z2_Sig','Z2_AIC','Translient'}, ...
            {'','sig','','','sig','',''});

    end

end

function [TS, significance, AIC] = process_TS_map(TS_map, x_vec, y_vec, dist, dof)
    %{
    For a given set of (x,y) coordinates on a test static map, finds the
    peak values within a square centered on each pair of given coordinates.
    Then converts peak TS values into gaussian significance assuming
    the TS values follow chi2 distribution of a given degrees of freedom.

    Input :
        - TS_map (double matrix) Matrix containing the test statistic.
        - x_vec (double vector) Vector containing the x query coordinates.
        - y_vec (double vector) Vector containing the y query coordinates.
        - dist (int) Distance specifying the length of the search square in 
        which to search for the TS peak in. The side length of the square 
        equals to 2*dist+1.
        - dof (int) Degrees of freedom of the chi2 distribution attributed to
        the test statistic values.

    Output :
        - TS (double vector) Vector containing the TS peak values attributed
        to the (x,y) query coordinates.
        - significance (double vector) Vector containing the gaussian
        significance derived from the TS peak values.
    %}

    % check if TS map exists and is a matrix
    if isempty(TS_map)
        error("Test statistic map does not exist or is empty.");
    elseif ~ismatrix(TS_map)
        error("Test statistic map should be a matrix.");
    end
    
    num_trans = numel(x_vec);
    
    % return empty results if no transients are found
    if num_trans == 0
        TS = [];
        significance = [];
        AIC = [];
        return;
    end

    % pad TS map with zeros to account for on-edge transient queries
    TS_map = padarray(TS_map, [dist, dist]);

    % construct query indices relative to the search square center position
    % account for zero padded matrix by adding dist
    x_rel = 0:dist*2;
    y_rel = 0:dist*2;

    % construct and fill TS vector
    TS = zeros(num_trans,1);
    for n=1:num_trans
        % query all positions within search square centered on original 
        % (x,y) coordinates
        x_query = x_vec(n)+x_rel;
        y_query = y_vec(n)+y_rel;
        TS0 = TS_map(y_query,x_query);
        % take only the maximum TS value within search square
        TS(n) = max(TS0, [],'all');
    end
    
    % convert TS values to gaussian significance
    p_val = chi2cdf(TS,dof,'upper');
    significance = -norminv(p_val);
    significance(isinf(significance)) = nan;

    AIC = TS - 2*dof;
end
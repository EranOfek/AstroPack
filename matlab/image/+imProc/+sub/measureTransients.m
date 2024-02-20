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
    Output  : - An AstroCatalog with the derived properties added to it.
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
    Example : AD = AstroZOGY('LAST*.fits','LAST*1*.fits');
              AD.subtractionD;
              AD.subtractionS;
              AD.findTransients;
              imProc.sub.measureTransients(AD);
    %}

    arguments
        AD AstroDiff

        % AstroZOGY
        Args.HalfSizeTS = 5;
    end
    
    % Get object class and apply corresponding function.
    ClassName = class(AD);

    switch ClassName
        case 'AstroZOGY'
            TranCat = measureTransientsAstroZOGY(AD, ...
                'HalfSizeTS', Args.HalfSizeTS...
                );
        otherwise
            % TODO: probably give Warning and return empty cat instead
            error('Class not supported.')
    end
    
end

function TranCat = measureTransientsAstroZOGY(AD, Args)
    %{ 
    For each transient candidate measure further properties given they are
      derived via AstroZOGY.
    Input   : - An AstroZOGY object in which CatData is populated.
              * ...,key,val,...
                'HalfSizeTS' - Half size of area on transients positions in 
                       test statistic images S2 and Z2. Actual size will be  
                       1+2*HalfSizeTS. Used to find peak S2 and Z2 values.
                       Default is 5.
    Output  : - An AstroCatalog with the derived properties added to it.
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
    end

    % Get number of objects
    Nobj = numel(AD);

    % Get image (x,y) coordinates of transients candidates
    XY = AD.CatData.getXY;

    % reverse order to initiate Result array with proper size on first 
    % iteration
    for Iobj=Nobj:-1:1
    
        CatSize = size(AD(Iobj).CatData.Catalog,1);
        
        % If catalog is empty, add empty columns and continue.
        if CatSize == 0
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
            [S2_TS, S2_Sig, S2_AIC] = imUtil.properSub.processStatMap(AD(Iobj).S2, ...
                XY(:,1), XY(:,2), 1, 'HalfSizeTS', Args.HalfSizeTS);
        end

        if isempty(AD(Iobj).Z2)
            Z2_TS = nan(Nsrc,1);
            Z2_Sig = nan(Nsrc,1);
            Z2_AIC = nan(Nsrc,1);
        else
            [Z2_TS, Z2_Sig, Z2_AIC] = imUtil.properSub.processStatMap(AD(Iobj).Z2, ...
                XY(:,1), XY(:,2), 2, 'HalfSizeTS', Args.HalfSizeTS);
        end

        % See if translient model is preferred over transient model and set
        % bool value, true if translient preferred, false if transient
        % preferred.
        Translient = double(S2_AIC < Z2_AIC);

        % Insert derived properties into AD.CatData catalog
        TranCat(Iobj) = AD(Iobj).CatData.insertCol(...
            cell2mat({S2_TS, S2_Sig, S2_AIC, Z2_TS, Z2_Sig, Z2_AIC, Translient}), ...
            'Score',...
            {'S2_TS','S2_Sig','S2_AIC','Z2_TS','Z2_Sig','Z2_AIC','Translient'}, ...
            {'','sig','','','sig','',''});


    end

end
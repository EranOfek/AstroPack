function TranCat = measureTransients(AD, Args)
    %{ 
    For each transient candidate measure further properties.
      These depend on the subtraction process, so a function is applied 
      that is determined by the object class.
    Input   : - An AstroDiff object in which CatData is populated.
              * ...,key,val,...
                --- AstroZOGY ---
                'RadiusTS' - Radius of area on transients positions in 
                       test statistic images S2 and Z2. Used to find peak 
                       S2 and Z2 values. Default is 5.
    Output  : - An AstroCatalog with the derived properties added to it.
                These properties may be the following;
                --- AstroZOGY ---
                .S2_TS - Peak value of the S2 test statistic within area
                       defined by RadiusTS around transient position.
                .S2_Sig - S2_TS converted to Gaussian significance.
                .S2_AIC - S2_TS converted to a loglike value penalized for
                       degrees of freedom (TS-2*d.o.f).
                .Z2_TS - Peak value of the Z2 test statistic within area
                       defined by RadiusTS around transient position.
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
        Args.RadiusTS = 5;
    end
    
    % Get object class and apply corresponding function.
    ClassName = class(AD);

    switch ClassName
        case 'AstroZOGY'
            TranCat = measureTransientsAstroZOGY(AD, ...
                'RadiusTS', Args.RadiusTS...
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
                'RadiusTS' - Radius of area on transients positions in 
                       test statistic images S2 and Z2. Used to find peak 
                       S2 and Z2 values. Default is 5.
    Output  : - An AstroCatalog with the derived properties added to it.
                These properties are the following;
                .S2_TS - Peak value of the S2 test statistic within area
                       defined by RadiusTS around transient position.
                .S2_Sig - S2_TS converted to Gaussian significance.
                .S2_AIC - S2_TS converted to a loglike value penalized for
                       degrees of freedom (TS-2*d.o.f).
                .Z2_TS - Peak value of the Z2 test statistic within area
                       defined by RadiusTS around transient position.
                .Z2_Sig - Z2_TS converted to Gaussian significance.
                .Z2_AIC - Z2_TS converted to a loglike value penalized for
                       degrees of freedom (TS-2*d.o.f).
                .Translient - Binary value on whether transLient model scores
                       higher than transient model. 1 for true and 0 for false.
    Author  : Ruslan Konno (Jan 2024)
    %}
    arguments
        AD AstroZOGY

        Args.RadiusTS = 5;
        Args.UseFWHM logical = true;
        Args.MultipleFWHM = 2;
    end

    if Args.UseFWHM
        Args.RadiusTS = AD.PSFData.fwhm * Args.MultipleFWHM;
        Args.RadiusTS = floor(Args.RadiusTS);
        Args.RadiusTs = Args.RadiusTS + mod(Args.RadiusTS+1,2);
    end

    % Get number of objects
    Nobj = numel(AD);

    % Get image (x,y) coordinates of transients candidates
    XY = AD.CatData.getXY('ColX', 'XPEAK', 'ColY', 'YPEAK');

    % reverse order to initiate Result array with proper size on first 
    % iteration
    for Iobj=Nobj:-1:1
    
        CatSize = size(AD(Iobj).CatData.Catalog,1);
        
        % If catalog is empty, continue.
        if CatSize < 1
            TranCat = AD(Iobj).CatData.Catalog;
            continue
        end
        
        % Get Scorr value 

        if ~isempty(AD(Iobj).Scorr)
            Indices = sub2ind(size(AD(Iobj).Scorr), XY(:,2),XY(:,1));
            Scorr = AD(Iobj).Scorr(Indices);
        else
            Scorr = nan(Nsrc,1);
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
            [S2_TS, S2_Sig, S2_AIC] = imUtil.properSub.findNearestPeakSig(AD(Iobj).S2, ...
                XY(:,1), XY(:,2), 1, 'RadiusTS', Args.RadiusTS);
        end

        if isempty(AD(Iobj).Z2)
            Z2_TS = nan(Nsrc,1);
            Z2_Sig = nan(Nsrc,1);
            Z2_AIC = nan(Nsrc,1);
        else
            [Z2_TS, Z2_Sig, Z2_AIC] = imUtil.properSub.findNearestPeakSig(AD(Iobj).Z2, ...
                XY(:,1), XY(:,2), 2, 'RadiusTS', Args.RadiusTS);
        end

        % Change Gabor stat to near max value
        if ~isempty(AD(Iobj).GaborSN)
            [Gabor_max, ~, ~] = imUtil.properSub.findNearestPeakSig(AD(Iobj).GaborSN, ...
                XY(:,1), XY(:,2), 2, 'RadiusTS', Args.RadiusTS);
            AD(Iobj).CatData.replaceCol(Gabor_max,'GaborSN');
        end

        % Insert derived properties into AD.CatData catalog
        TranCat(Iobj) = AD(Iobj).CatData.insertCol(...
            cell2mat({cast(Scorr,'double'), cast(S2_TS,'double'), cast(Z2_TS,'double'), ...
            cast(S2_Sig,'double'), cast(Z2_Sig,'double'), cast(S2_AIC,'double'), ...
            cast(Z2_AIC,'double')}), 'Score',...
            {'Scorr','S2_TS','Z2_TS','S2_Sig','Z2_Sig','S2_AIC','Z2_AIC'}, ...
            {'sig','','','sig','sig','',''});


    end

end
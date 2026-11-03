function A=importImUtilAstroStreak(~,imUtilA)
    % importer - translator from imUtil.streaks.AstroStreak
    %  TODO: empty fields are to be handled gracefully

    % count how may streaks we have to deal with, first:
    N=0;
    for i=1:numel(imUtilA)
        N=N+numel(imUtilA(i).Flux);
    end

    k=0;
    A=AstroStreak.empty(0,N);
    for i=1:numel(imUtilA)
        imUtilA.setExtent; % if not already populated
        for j=1:numel(imUtilA(i).Flux)
            k=k+1;
            A(k).Xdetect = imUtilA(i).X(:,j)';
            A(k).Ydetect = imUtilA(i).Y(:,j)';
            try
                A(k).RAExtremes = imUtilA(i).RA(:,j)';
                A(k).DecExtremes = imUtilA(i).Dec(:,j)';
            catch
            end
            A(k).JD = imUtilA(i).JD;
            A(k).IsEdge = imUtilA(i).IsEdge(:,j)';
            A(k).Flux = imUtilA(i).Flux(j);
            A(k).FitPar = imUtilA(i).FitPar(:,j)';
            try
                A(k).ExtentIndices = imUtilA(i).Curve(j).ExtentIndices;
            catch
            end
            A(k).TransversePSF = imUtilA(i).Curve(j).TransversePSF;
            % ID - not yet decided what it is

            % all these should have the same size, as the number of slices, or
            %  be empty. Maybe enforce a consistency check
            A(k).X = imUtilA(i).Curve(j).X;
            A(k).Y = imUtilA(i).Curve(j).Y;
            try
                A(k).RA = imUtilA(i).Curve(j).RA;
                A(k).Dec = imUtilA(i).Curve(j).Dec;
            catch
            end
            A(k).SliceFlux = imUtilA(i).Curve(j).Flux;
            A(k).TransverseSigma = imUtilA(i).Curve(j).TransverseSigma;
            A(k).Hmean = imUtilA(i).Curve(j).Hmean;
            A(k).Acceptable = imUtilA(i).Curve(j).Acceptable;
        end
    end
end

function A=importImUtilAstroStreak(~,imUtilA)
    % importer - translator from imUtil.streaks.AstroStreak
    %  TODO: imUtilA can be an array, whose elements contain each a
    %   different number of curves;
    %  TODO: empty fields are to be handled gracefully
    % count how may streaks we have to deal with, first:
    N=0;
    for i=1:numel(imUtilA)
        N=N+numel(imUtilA(i).Flux);
    end

    k=0;
    A=AstroStreak.empty(0,N);
    for i=1:numel(imUtilA)
        for j=1:numel(imUtilA(i).Flux)
            k=k+1;
            A(k).Xdetect = imUtilA(i).X(:,j)';
            A(k).Ydetect = imUtilA(i).Y(:,j)';
            A(k).LightSec = [];
            A(k).RAExtremes = imUtilA(i).RA(:,j)';
            A(k).DecExtremes = imUtilA(i).Dec(:,j)';
            A(k).JD = [imUtilA(i).JD; imUtilA(i).JD];
            A(k).IsEdge = imUtilA(i).IsEdge(:,j)';
            A(k).Flux = imUtilA(i).Flux(j);
            try
                A(k).AngSpeed = imUtilA(i).AngSpeed(j);
            catch
            end
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
            A(k).RA = imUtilA(i).Curve(j).RA;
            A(k).Dec = imUtilA(i).Curve(j).Dec;
            A(k).SliceFlux = imUtilA(i).Curve(j).Flux;
            try
                A(k).Mag = imUtilA(i).Curve(j).Mag;
            catch
            end
            A(k).TransverseSigma = imUtilA(i).Curve(j).TransverseSigma;
            A(k).Hmean = imUtilA(i).Curve(j).Hmean;
            A(k).Acceptable = imUtilA(i).Curve(j).Acceptable;
        end
    end
end

function importImUtilAstroStreak(A,imUtilA)
    % importer - translator from imUtil.streaks.AstroStreak
    %  TODO: imUtilA can be an array, whose elements contain each a
    %   different number of curves;
    %  TODO: empty fields are to be handled gracefully
    A=repmat(A,1,numel(imUtilA.Flux));
    for i=1:numel(imUtilA.Curve)
        A(i).Xdetect = imUtilA.X(:,i)';
        A(i).Ydetect = imUtilA.Y(:,i)';
        A(i).LightSec = [];
        A(i).RAextremes = imUtilA.RA(:,i)';
        A(i).DecExtremes = imUtilA.Dec(:,i)';
        A(i).JD = [imUtilA.JD; imUtilA.JD];
        A(i).IsEdge = imUtilA.IsEdge(:,i)';
        A(i).Flux = imUtilA.Flux(i);
        try
            A(i).AngSpeed = imUtilA.AngSpeed(i);
        catch
        end
        A(i).FitPar = imUtilA.FitPar(:,i)';
        A(i).ExtentIndices = imUtilA.Curve(i).ExtentIndices;
        A(i).TransversePSF = imUtilA.Curve(i).TransversePSF;
        % ID - not yet decided what it is

        % all these should have the same size, as the number of slices, or
        %  be empty. Maybe enforce a consistency check
        A(i).X = imUtilA.Curve(i).X;
        A(i).Y = imUtilA.Curve(i).Y;
        A(i).RA = imUtilA.Curve(i).RA;
        A(i).Dec = imUtilA.Curve(i).Dec;
        A(i).SliceFlux = imUtilA.Curve(i).Flux;
        try
            A(i).Mag = imUtilA.Curve(i).Mag;
        catch
        end
        A(i).TransverseSigma = imUtilA.Curve(i).TransverseSigma;
        A(i).Hmean = imUtilA.Curve(i).Hmean;
        A(i).Acceptable = imUtilA.Curve(i).Acceptable;
    end
end

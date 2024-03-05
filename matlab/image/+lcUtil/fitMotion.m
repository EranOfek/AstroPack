function Result = fitMotion(Obj, Args)
    % fit Proper motion in RA/Dec to coordinates in MatchedSources object.
    %       Given a MatchedSources object with RA/Dec fields, fit proper
    %       motions using the celestial.pm.fitMultiProperMotion function.
    % Input  : - A MatchedSources Object.
    %          * ...,key,val,...
    %            'MinNobs' - minimum number of data points required for fit
    %                   (used only when the number of observations is not
    %                   the same for all sources).
    %                   Default is 3.
    %            'Prob' - Vector of probabilities for which to calculate
    %                   the probably difference between H1 and H0.
    %                   Default is [1e-3 1e-5].
    %            'Units' - Units of RA/Dec. This is used only for the the
    %                   checking that the RA data is not crossing zero.
    %                   Default is 'deg'.
    %            'RenormErr' - A logical flag inducating if to normalize
    %                   the chi2 for the H1 hypothesis to 1.
    %                   Default is true.
    % Output : - A structure with the following fields:
    %            [Note that RA fit is in time units (cos(Dec) factor) while Dec is in angular
    %            units]
    %            .MeanT - Mean epoch relative to which the fit is done.
    %            .RA.ParH1 - Parameters for H1 [pos; vel] for each source.
    %            .RA.ParH0 - Parameters for H0 [pos] for each source.
    %            .RA.Chi2_H1 - \chi^2 for H1
    %            .RA.Chi2_H0 - \chi^2 for H0
    %            .RA.Nobs - Number of observations for each source.
    %            .RA.DeltaChi2 - \Delta\chi^2 between H0 and H1.
    %            .RA.FlagH1 - A logical indicating if the source has a
    %                   prefreed H1 model over H0. each line, for each
    %                   requested proabability.
    %            .RA.StdResid_H1 - std of H1 residuals.
    %            .RA.StdResid_H0 - std of H0 residuals.
    %            .Dec. - the same as .RA, but for the Dec axis.
    % Author : Eran Ofek (Sep 2021)
    % Example: Time=(1:1:20)'; RA = randn(20,1e5)./(3600.*100); Dec=randn(20,1e5)./(3600.*200);
    %          RA(:,1) = (0.1:0.1:2)'./(3600);
    %          Obj = MatchedSources;
    %          Obj.addMatrix(RA,'RA');
    %          Obj.addMatrix(Dec,'DEC');
    %          Result = lcUtil.fitMotion(Obj);
    
    arguments
        Obj MatchedSources
        Args.MinNobs                = 3;
        
        Args.Units                  = 'deg';
        Args.UnitsErr               = 'deg';
        
        Args.Niter                  = 2;
        Args.SigmaClip              = [3 3];

        Args.Prob                   = [1e-3 1e-5];
        Args.RenormErr(1,1) logical = true;
    end
    
    
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        
        [MatRA, MatDec, MatErrRA, MatErrDec] = getLonLat(Obj(Iobj));
    
        if isempty(MatErrRA)
            MatErrRA = 1./(3600.*100);
        end
        if isempty(MatErrDec)
            MatErrDec = 1./(3600.*100);
        end
        Algo = 0;
        if Algo==0
            Result(Iobj) = celestial.pm.fitMultiProperMotion(Obj(Iobj).JD, MatRA, MatDec, MatErrRA, MatErrDec, 'MinNobs',Args.MinNobs,...
                                                                                'Prob',Args.Prob,...
                                                                                'Units',Args.Units,...
                                                                                'RenormErr',Args.RenormErr);
        else
            Result(Iobj) = celestial.pm.fitLinearProperMotion(Obj(Iobj).JD, MatRA, MatDec, 'MinNObs',Args.MinNobs,...
                                                                                       'Niter',Args.Niter,...
                                                                                       'SigmaClip',Args.SigmaClip,...
                                                                                       'Units',Args.Units,...
                                                                                       'UnitsErr',Args.UnitsErr);
        end
    end
end
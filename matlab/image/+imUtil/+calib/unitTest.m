function Answer=unitTest
    % unitTest for the imUtil.calib package
    
    
    % testing: imUtil.calib.calibDesignMatrix
    

    MagErr = 0.03;
    Nimage = 50;
    Nstar  = 300;
    Mag    = rand(Nstar,1).*10;
    ZP     = rand(Nimage,1).*2;

    InstMag = ZP + Mag.';
    InstMag = InstMag + MagErr.*randn(size(InstMag));

    H1=imUtil.calib.calibDesignMatrix(Nimage, Nstar,'Sparse',false);
    H=imUtil.calib.calibDesignMatrix(Nimage, Nstar,'Sparse',true);
    
    Par1 = H1\InstMag(:);
    Par = H\InstMag(:);
    
    ParZP = Par(1:Nimage);
    ParM  = Par(Nimage+1:end);
    std(ParZP - ZP)   % should be eq to MagErr/sqrt(Nimage)
    std(ParM  - Mag)  % should be eq to MagErr/sqrt(Nstar)

    Answer = true;

    % testing: transmissionModel - transmissionCostFun - transmissionFit
    % testing: imUtil.calib.transmissionModel
   
    % Define transmission functions as struct array
    TransFunList(1).name = 'Ozone';
    TransFunList(1).handle = '@astro.transmission.ozoneTransmission';
    TransFunList(1).handletype = 'named';
    TransFunList(1).params = [30, 300];
    TransFunList(1).paraminfo(1).name = 'ZenithAngle_deg';
    TransFunList(1).paraminfo(1).min = 0;
    TransFunList(1).paraminfo(1).max = 90;
    TransFunList(1).paraminfo(2).name = 'DobsonUnits';
    TransFunList(1).paraminfo(2).min = 200;
    TransFunList(1).paraminfo(2).max = 400;
    TransFunList(2).name = 'Aerosol';
    TransFunList(2).handle = '@astro.transmission.aerosolTransmission';
    TransFunList(2).handletype = 'named';
    TransFunList(2).params = [30, 0.05, 1.2];
    TransFunList(2).paraminfo(1).name = 'ZenithAngle_deg';
    TransFunList(2).paraminfo(1).min = 0;
    TransFunList(2).paraminfo(1).max = 90;
    TransFunList(2).paraminfo(2).name = 'TauAod500';
    TransFunList(2).paraminfo(2).min = 0.0;
    TransFunList(2).paraminfo(2).max = 0.5;
    TransFunList(2).paraminfo(3).name = 'Alpha';
    TransFunList(2).paraminfo(3).min = 0.5;
    TransFunList(2).paraminfo(3).max = 2.5;
    % Build model with metadata injection
    Model = imUtil.calib.transmissionModel(TransFunList, ...
        'Airmass', 1.2, 'Temperature', 15, 'Pressure_mbar', 965);


    % testing: imUtil.calib.transmissionCostFun
    % Reuse the Model and test data from transmissionModel test

    % Get parameter values from Model
    TransParams = Model.valuesAllPar();
    % Create test data (3 calibrators with realistic Gaia-like spectra)
    Lambda = linspace(336, 1020, 343)';
    Spec = [(5e-17) ./ (Lambda / 400).^2, ...      % Blue star
            (3e-17) ./ (Lambda / 550).^0.5, ...    % Solar-type star
            (2e-17) * (Lambda / 700).^1.5];        % Red star [343 x 3]
    SpecErr = 0.05 * Spec;  % 5% errors
    Flux = [5.1e4; 7.5e4; 6.3e4];  % Observed photons
    FluxErr = [5e3; 4e2; 3e2];
    X = [500; 1000; 1500];  % Pixel coordinates
    Y = [500; 1000; 1500];
    PolyFieldCorr = @(X, Y, P) telescope.optics.fieldCorrectionLAST([X(:), Y(:)], P);
    FieldParams = zeros(1, 10);
    % Option 1: Pass TransParams explicitly
    [Res, Cost, Pred] = imUtil.calib.transmissionCostFun(Lambda, Spec, SpecErr, ...
        Flux, FluxErr, Model, 'TransParams', TransParams, 'X', X, 'Y', Y, ...
        'PolyFieldCorr', PolyFieldCorr, 'FieldParams', FieldParams);
    % Option 2: Use parameters from Model (extracted automatically)
    % [Res, Cost, Pred] = imUtil.calib.transmissionCostFun(Lambda, Spec, SpecErr, ...
    %     Flux, FluxErr, Model, 'X', X, 'Y', Y, 'PolyFieldCorr', PolyFieldCorr, ...
    %     'FieldParams', FieldParams);



    % testing: imUtil.calib.transmissionFit
    % Reuse the same TransFunList and test data

    % Define 2-stage optimization sequence as struct array
    OptSeq(1).stagename = 'AerosolOpt';
    OptSeq(1).freeparams(1).function = 'Aerosol';
    OptSeq(1).freeparams(1).parameter = 'TauAod500';
    OptSeq(1).sigmaclip = true;
    OptSeq(1).sigmathresh = 3.0;
    OptSeq(1).sigmaiter = 3;
    OptSeq(1).description = 'Optimize aerosol optical depth';
    OptSeq(2).stagename = 'FieldCorr';
    OptSeq(2).freeparams = [];  % Empty for field correction
    OptSeq(2).sigmaclip = true;
    OptSeq(2).sigmathresh = 2.0;
    OptSeq(2).sigmaiter = 2;
    OptSeq(2).regularization = 1e-6;
    OptSeq(2).description = 'Field correction (always linear)';
    % Run fitting
    [Model, FieldParams, Results] = imUtil.calib.transmissionFit(...
        Lambda, Spec, SpecErr, Flux, FluxErr, 'X', X, 'Y', Y, ...
        'PolyFieldCorr', PolyFieldCorr, ...
        'TransmissionFunctions', TransFunList, 'OptimizationSequence', OptSeq);
    
 % Final transmission parameters:
 % [1] ZenithAngle_deg: 33.557310
 % [2] DobsonUnits: 300.000000
 % [3] TauAod500: 0.005454
 % [4] Alpha: 1.200000

%Final field correction parameters:
%{  
[1] kx0: 0.000129522473
[2] kx: 0.000862727726
[3] kx2: 0.001387635303
[4] kx3: -0.000271156095
[5] kx4: -0.002494055579
[6] ky: 0.000862727726
[7] ky2: 0.001387635304
[8] ky3: -0.000271156095
[9] ky4: -0.002494055580
[10] kxy: 0.000758578889
%}

%Final calibrators: 2
%Final RMS: 0.0000 mag   
end
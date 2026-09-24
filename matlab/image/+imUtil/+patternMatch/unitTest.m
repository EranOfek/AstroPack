function Result = unitTest()
    % unitTest for imUtil.patternMatch
    % Example: imUtil.patternMatch.unitTest

	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    

    %% imUtil.patternMatch.matchPatternPairs
    [Result, Tests] = unitTest_patternMatchPairs();


    %% histograms
    Xcat=rand(1e3,1).*1024; Ycat=rand(1e3,1).*1024; Xref=[Xcat+2;1]; Yref=[Ycat+1;2];
    FlipX=1; FlipY=1;
    RangeX=[-2000 2000]; 
    RangeY=[-1000 1000]; 
    StepX=400;
    StepY=400;

    Nsim = 10;
    tic;
    for i=1:Nsim
        Dx=Xcat-FlipX.*Xref.';
        Dy=Ycat-FlipY.*Yref.';
        %[H2] = histcounts2(Dy(:),Dx(:), (RangeY(1):StepY:RangeY(2)),(RangeX(1):StepX:RangeX(2)) );
        [H2] = histcounts2(Dx(:),Dy(:), (RangeX(1):StepX:RangeX(2)),(RangeY(1):StepY:RangeY(2)) );
    end
    toc
    % tic;
    % for i=1:Nsim
    %     %[H2b,VecYa,VecXa] = hist2d_VVtrans(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY);
    %     [H2b,VecXa,VecYa] = hist2d_VVtrans_fix(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY);
    % end
    % toc   
    % 
    % if max(abs(H2-H2b),[],'all')>0
    %     error('Problem with tools.hist.mex.hist2d_VVtrans');
    % end

    % histograms


    tic;
    for i=1:Nsim
        Dx=Xcat-FlipX.*Xref.';
        Dy=Ycat-FlipY.*Yref.';
        %[H2a,VecY,VecX] = tools.array.hist2d_fast(Dy(:),Dx(:),RangeY,RangeX,StepY,StepX); 
        [H2a,VecX,VecY] = tools.array.hist2d_fast(Dx(:),Dy(:),RangeX,RangeY,StepX,StepY); 
    end
    toc  
    

    tic;
    for i=1:Nsim
        Dx=Xcat-FlipX.*Xref.';
        Dy=Ycat-FlipY.*Yref.';
        H2c=tools.hist.histcounts2regular_mex(Dy(:),Dx(:),[RangeX, StepX],[RangeY, StepY], false);
    end
    toc

    
    % for i=1:Nsim
    %     Dx=Xcat-FlipX.*Xref.';
    %     Dy=Ycat-FlipY.*Yref.';
    %     H2d=hist2d_fast_mex(Dy(:),Dx(:), RangeX, RangeY ,StepX, StepY);
    % end
    % toc

    if sum(H2c~=H2a,'all')>0
        error('Problem with tools.hist.mex.hist2d_fast_mex');
    end
    
    %if max(abs(H2-single(H2c)),[],'all')>0
    %    error('Problem with tools.hist.histcounts2regular_mex');
    %end
    if max(abs(H2-H2a),[],'all')>0
        %tools.array.hist2d_fast is not fully consistent with histcounts2 but it is ok (edge effects)
        error('Problem with tools.array.hist2d_fast');
    end
    

    %% imUtil.patternMatch.mex.distAngPairs_mex

    N=1e3;
    CatX=rand(N,1).*1024;
    CatY=rand(N,1).*1024;
    MaxDist = 500;
    FlipX   = -1;
    FlipY   = 1;

    Nsim=1;
  
    tic;
    for i=1:Nsim
        CatXt = CatX.*FlipX;
        CatYt = CatY.*FlipY;
        CatDiffX = CatXt - CatXt.';
        CatDiffY = CatYt - CatYt.';
        % select withn Max Dist
        Fc = abs(CatDiffX(:))<MaxDist & abs(CatDiffY(:))<MaxDist;
        % find(Fc) is slower here...
        CatDiffX = CatDiffX(Fc);
        CatDiffY = CatDiffY(Fc);
        % all possible distances/angle between sources in Cat
        CatDist  = sqrt(CatDiffX.^2 + CatDiffY.^2);
        CatTan   = atan(CatDiffY./CatDiffX);
    
    end
    FF = CatDist<MaxDist;
    CatDist = CatDist(FF);
    CatTan  = CatTan(FF);
    T1=toc;

    tic;
    for i=1:Nsim
	    [a,b]=imUtil.patternMatch.mex.distAngPairs_mex(CatX,CatY,MaxDist, false, FlipX, FlipY);
    end
    T2=toc;

    [CatDist,SI] = sort(CatDist);
    CatTan = CatTan(SI);
    [a,SI] = sort(a);
    b      = b(SI);

    %size(a)
    %size(CatDist)

    if max(abs(a-CatDist))>1e-12
        error('Problem with ]=imUtil.patternMatch.mex.distAngPairs_mex CatDist output');
    end
    DiffTan = abs(b-CatTan);
    Frest = DiffTan>1e-12;
    if max(abs(DiffTan(Frest)-pi))>1e-12
        error('Problem with ]=imUtil.patternMatch.mex.distAngPairs_mex CatTan output');
    end

  
    %%

    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end


function [Result, Tests] = unitTest_patternMatchPairs()
    % unitTest for imUtil.patternMatch.matchPatternPairs
    %   Simulates reference catalogs, applies known shift/rotation/flip
    %   transformations plus noise and unmatched stars, and verifies that
    %   matchPatternPairs recovers the correct solution.
    % Output : - Result: true if all tests passed.
    %          - Tests: A structure array (one element per test) with fields:
    %            .Name      - Test description.
    %            .True      - Structure of true simulated parameters
    %                         (.Theta, .ShiftX, .ShiftY, .Flip), or [] for
    %                         tests with no true solution (negative tests).
    %            .Res       - Full Result structure returned by
    %                         matchPatternPairs (the best solution found).
    %            .DTheta    - Recovered-minus-true rotation [deg] (NaN if N/A).
    %            .DShiftX   - Recovered-minus-true X shift [pix] (NaN if N/A).
    %            .DShiftY   - Recovered-minus-true Y shift [pix] (NaN if N/A).
    %            .Passed    - Logical, this test passed.
    % Author : Claude (Jul 2026)
    % Example: [Result, Tests] = imUtil.patternMatch.unitTest;
    %          struct2table(rmfield(Tests, {'True','Res'}))

    rng(1);   % reproducibility

    Fun = @imUtil.patternMatch.matchPatternPairs;

    FieldSize = 2048;
    PosNoise  = 0.3;      % [pix] positional noise
    MagNoise  = 0.05;     % [mag] photometric noise

    Tests = struct('Name',{}, 'True',{}, 'Res',{}, ...
                   'DTheta',{}, 'DShiftX',{}, 'DShiftY',{}, 'Passed',{});

    %--- Test 1: pure shift, with magnitudes ---
    [Ref, Img, True] = simulateField(FieldSize, 300, 0, [120.5, -63.2], false, ...
                                     1.0, 0.0, PosNoise, MagNoise);
    Res = Fun(Ref, Img);
    Tests(end+1) = packTest('Pure shift', True, Res);
    assert(Res.Found, 'Test 1: solution not found');
    checkSolution(Res, True, 0.5, 0.5, 'Test 1');
    assert(~Res.Flip, 'Test 1: spurious flip');
    Tests(end).Passed = true;

    %--- Test 2: shift + rotation, with magnitudes ---
    [Ref, Img, True] = simulateField(FieldSize, 300, 33.7, [-210.3, 95.8], false, ...
                                     1.0, 0.0, PosNoise, MagNoise);
    Res = Fun(Ref, Img);
    Tests(end+1) = packTest('Shift+rotation', True, Res);
    assert(Res.Found, 'Test 2: solution not found');
    checkSolution(Res, True, 0.5, 0.5, 'Test 2');
    assert(abs(Res.Scale-1) < 1e-2, 'Test 2: scale deviates from unity');
    Tests(end).Passed = true;

    %--- Test 3: robustness - only 50 percent of stars in common ---
    [Ref, Img, True] = simulateField(FieldSize, 400, -71.2, [55.0, 300.1], false, ...
                                     0.5, 0.5, PosNoise, MagNoise);
    Res = Fun(Ref, Img);
    Tests(end+1) = packTest('50% overlap', True, Res);
    assert(Res.Found, 'Test 3: solution not found with 50% overlap');
    checkSolution(Res, True, 0.5, 1.0, 'Test 3');
    Tests(end).Passed = true;

    %--- Test 4: no magnitude information (2-column input) ---
    [Ref, Img, True] = simulateField(FieldSize, 300, 12.3, [40.0, -87.5], false, ...
                                     0.8, 0.2, PosNoise, MagNoise);
    Res = Fun(Ref(:,1:2), Img(:,1:2));
    Tests(end+1) = packTest('No magnitudes', True, Res);
    assert(Res.Found, 'Test 4: solution not found without magnitudes');
    checkSolution(Res, True, 0.5, 1.0, 'Test 4');
    assert(isnan(Res.ResidMag), 'Test 4: ResidMag should be NaN without magnitudes');
    Tests(end).Passed = true;

    %--- Test 5: flip, with TreatFlips=true ---
    [Ref, Img, True] = simulateField(FieldSize, 300, 20.0, [-15.2, 240.0], true, ...
                                     0.8, 0.2, PosNoise, MagNoise);
    Res = Fun(Ref, Img, 'TreatFlips',true);
    Tests(end+1) = packTest('Flip detected', True, Res);
    assert(Res.Found, 'Test 5: flipped solution not found');
    assert(Res.Flip,  'Test 5: flip not detected');
    checkSolution(Res, True, 0.5, 1.0, 'Test 5');
    Tests(end).Passed = true;

    %--- Test 6: flip present but TreatFlips=false -> must NOT lock falsely ---
    Res = Fun(Ref, Img, 'TreatFlips',false);
    Tests(end+1) = packTest('Flip ignored (negative)', [], Res);
    assert(~Res.Found, 'Test 6: false positive on flipped field with TreatFlips=false');
    Tests(end).Passed = true;

    %--- Test 7: TreatFlips=true on a non-flipped field -> must pick no-flip ---
    [Ref, Img, True] = simulateField(FieldSize, 300, -5.5, [10.0, 10.0], false, ...
                                     0.9, 0.1, PosNoise, MagNoise);
    Res = Fun(Ref, Img, 'TreatFlips',true);
    Tests(end+1) = packTest('Parity arbitration', True, Res);
    assert(Res.Found && ~Res.Flip, 'Test 7: wrong parity selected');
    checkSolution(Res, True, 0.5, 0.5, 'Test 7');
    Tests(end).Passed = true;

    %--- Test 8: ThetaRange restriction (true angle inside range) ---
    [Ref, Img, True] = simulateField(FieldSize, 300, 2.1, [-300.0, 150.0], false, ...
                                     0.8, 0.2, PosNoise, MagNoise);
    Res = Fun(Ref, Img, 'ThetaRange',[-5 5]);
    Tests(end+1) = packTest('ThetaRange inside', True, Res);
    assert(Res.Found, 'Test 8: solution not found within ThetaRange');
    checkSolution(Res, True, 0.5, 0.5, 'Test 8');
    Tests(end).Passed = true;

    %--- Test 9: ThetaRange restriction (true angle outside range) ---
    [Ref, Img, ~] = simulateField(FieldSize, 300, 45.0, [50.0, 50.0], false, ...
                                  0.9, 0.1, PosNoise, MagNoise);
    Res = Fun(Ref, Img, 'ThetaRange',[-10 10]);
    Tests(end+1) = packTest('ThetaRange outside (negative)', [], Res);
    assert(~Res.Found, 'Test 9: false positive outside allowed ThetaRange');
    Tests(end).Passed = true;

    %--- Test 10: unrelated catalogs -> no solution ---
    RefA = [rand(200,2).*FieldSize, 15+3.*rand(200,1)];
    RefB = [rand(200,2).*FieldSize, 15+3.*rand(200,1)];
    Res  = Fun(RefA, RefB, 'TreatFlips',true);
    Tests(end+1) = packTest('Unrelated catalogs (negative)', [], Res);
    assert(~Res.Found, 'Test 10: false positive on unrelated catalogs');
    Tests(end).Passed = true;

    %--- Test 11: identity transformation ---
    [Ref, Img, True] = simulateField(FieldSize, 300, 0, [0 0], false, ...
                                     1.0, 0.0, PosNoise, MagNoise);
    Res = Fun(Ref, Img);
    Tests(end+1) = packTest('Identity', True, Res);
    assert(Res.Found, 'Test 11: identity solution not found');
    checkSolution(Res, True, 0.5, 0.5, 'Test 11');
    Tests(end).Passed = true;

    %--- Test 12: consistency of matched indices and transformation ---
    [Ref, Img, True] = simulateField(FieldSize, 300, 62.0, [77.0, -13.0], false, ...
                                     0.8, 0.2, PosNoise, MagNoise);
    Res = Fun(Ref, Img);
    Tests(end+1) = packTest('Tran/RMS consistency', True, Res);
    assert(Res.Found, 'Test 12: solution not found');
    assert(numel(Res.MatchedRefInd)==Res.Nmatch && ...
           numel(Res.MatchedImgInd)==Res.Nmatch, 'Test 12: index count mismatch');
    XYt = (Res.Tran(:,1:2)*Img(Res.MatchedImgInd,1:2).').' + Res.Tran(:,3).';
    Rms = sqrt(mean(sum((Ref(Res.MatchedRefInd,1:2)-XYt).^2, 2)));
    assert(abs(Rms - Res.RMS) < 1e-6, 'Test 12: Tran inconsistent with reported RMS');
    assert(Res.RMS < 5.*PosNoise, 'Test 12: RMS too large');
    assert(Res.ResidMag < 5.*MagNoise.*sqrt(2), 'Test 12: magnitude residuals too large');
    Tests(end).Passed = true;

    %--- Test 13: dense field ---
    [Ref, Img, True] = simulateField(FieldSize, 2000, 15.0, [30.0, -40.0], false, ...
                                     0.7, 0.3, PosNoise, MagNoise);
    Res = Fun(Ref, Img, 'MaxStars',80);
    Tests(end+1) = packTest('Dense field', True, Res);
    assert(Res.Found, 'Test 13: solution not found in dense field');
    checkSolution(Res, True, 0.5, 1.0, 'Test 13');
    Tests(end).Passed = true;

    %--- Test 14: S/N sanity (on the dense-field solution) ---
    assert(Res.SN_Theta >= 5 && Res.SN_Shift >= 5, 'Test 14: reported S/N below threshold');

    fprintf('matchPatternPairs.unitTest passed (%d tests)\n', numel(Tests));
    Result = true;
end

% ======================= local functions =======================

function T = packTest(Name, True, Res)
    % Pack test name, true parameters, solution, and truth-vs-recovered diffs
    T.Name = Name;
    T.True = True;
    T.Res  = Res;
    if ~isempty(True) && Res.Found
        T.DTheta  = mod(Res.Theta - True.Theta + 180, 360) - 180;
        T.DShiftX = Res.ShiftX - True.ShiftX;
        T.DShiftY = Res.ShiftY - True.ShiftY;
    else
        T.DTheta  = NaN;
        T.DShiftX = NaN;
        T.DShiftY = NaN;
    end
    T.Passed = false;   % set true after this test's asserts pass
end

function [Ref, Img, True] = simulateField(FieldSize, Nstar, Theta, Shift, UseFlip, ...
                                          FracCommon, FracExtra, PosNoise, MagNoise)
    % Simulate a reference catalog and a transformed image catalog.
    %   Model applied: Img is generated s.t. Ref = R(Theta)*P*Img + Shift
    %   FracCommon - fraction of Ref stars present in Img.
    %   FracExtra  - fraction (of Nstar) of spurious stars added to Img only.
    True.Theta  = Theta;
    True.ShiftX = Shift(1);
    True.ShiftY = Shift(2);
    True.Flip   = UseFlip;

    Ref = [rand(Nstar,2).*FieldSize, 15 + 3.*rand(Nstar,1)];

    P = eye(2);
    if UseFlip
        P = [-1 0; 0 1];
    end
    R = [cosd(Theta) -sind(Theta); sind(Theta) cosd(Theta)];
    M = R*P;                                   % Ref = M*Img + Shift
    Ncom = round(FracCommon.*Nstar);
    Ind  = randperm(Nstar, Ncom);
    XYi  = (M \ (Ref(Ind,1:2) - Shift).').';   % invert exactly
    Img  = [XYi + PosNoise.*randn(Ncom,2), Ref(Ind,3) + MagNoise.*randn(Ncom,1)];
    % add positional noise to Ref as well
    Ref(:,1:2) = Ref(:,1:2) + PosNoise.*randn(Nstar,2);

    % spurious stars in Img only
    Next = round(FracExtra.*Nstar);
    Img  = [Img; rand(Next,2).*FieldSize, 15 + 3.*rand(Next,1)];
    Img  = Img(randperm(size(Img,1)), :);      % shuffle rows
end

function checkSolution(Res, True, TolTheta, TolShift, Msg)
    % Verify recovered parameters against truth
    DTheta = mod(Res.Theta - True.Theta + 180, 360) - 180;
    assert(abs(DTheta) < TolTheta, ...
        '%s: Theta error %.3f deg (true %.2f, got %.2f)', Msg, DTheta, True.Theta, Res.Theta);
    assert(abs(Res.ShiftX - True.ShiftX) < TolShift, ...
        '%s: ShiftX error %.3f pix', Msg, Res.ShiftX - True.ShiftX);
    assert(abs(Res.ShiftY - True.ShiftY) < TolShift, ...
        '%s: ShiftY error %.3f pix', Msg, Res.ShiftY - True.ShiftY);
    assert(Res.Flip == True.Flip, '%s: parity mismatch', Msg);
end
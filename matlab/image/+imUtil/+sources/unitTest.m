function Result = unitTest
    % unitTest for imUtil.sources
    % Example: Result = imUtil.sources.unitTest
    
    
    % imUtil.sources.findSources
    Image = randn(1700, 1700);
    Result = imUtil.sources.findSources(Image,'Threshold',6);
    if numel(Result.XPEAK)>0
        error('Problem with imUtil.sources.findSources');
    end
    
    
    % imUtil.sources.aperPhotCube
    % simple case
    Cube = randn(17,17,4000); X=ones(4000,1).*9; Y=X;
    imUtil.sources.aperPhotCube(Cube, X, Y);
    PSF  = imUtil.kernel2.gauss([1.5;2],[17 17]);
    imUtil.sources.aperPhotCube(Cube, X, Y, 'PSF',PSF);
    imUtil.sources.aperPhotCube(Cube, X, Y, 'PSF',PSF,'SubPixShift','fft');
    % PSF case w/o noise
    Nsrc  = 10;
    Flux  = rand(Nsrc, 1).*100;
    Cube  = imUtil.kernel2.gauss(1.5.*ones(Nsrc,1),[17 17]);
    Cube  = Cube.*permute(Flux,[3 2 1]);
    Ratio = squeeze(sum(Cube,[1 2]))./Flux;
    if abs(Ratio-1)>1e-6
        error('Problem with imUtil.sources.aperPhotCube simulations');
    end
    Result = imUtil.sources.aperPhotCube(Cube, 9.*ones(Nsrc,1), 9.*ones(Nsrc,1), 'AperRad',[2 4 5 6]);

    % imUtil.sources.moments - first moment on noise-free Gaussian sources (issue #1275)
    % A bright source 0.4 pix off the stamp center must be recovered to <1%
    % (the 3.75-pix support disc used to compress it to 0.82 of the offset),
    % and a faint one (SN=6) 0.6 pix off must not freeze on the 0.4 rail.
    SigmaPSF = 1.57;
    [XX,YY]  = meshgrid(1:25, 1:25);
    Stamp    = @(Dx) 1e4.*exp(-((XX-13-Dx).^2 + (YY-13).^2)./(2.*SigmaPSF.^2))./(2.*pi.*SigmaPSF.^2);
    Cube     = cat(3, Stamp(0.4), Stamp(0.6));
    M1 = imUtil.sources.moments(Cube, 'X',[13;13], 'Y',[13;13], 'SN',[1000; 6], 'Annulus',[10 12]);
    if abs(M1.StampX1(1) - 0.4)>0.01 || abs(M1.StampY1(1))>0.01
        error('Problem with imUtil.sources.moments: bright-source first moment compressed toward the stamp center');
    end
    if abs(M1.StampX1(2) - 0.6)>0.03
        error('Problem with imUtil.sources.moments: faint-source first moment frozen by the step clamp');
    end
    % cube input without X/Y: full-image coordinates are empty, stamp ones are not (issue #1292)
    M1 = imUtil.sources.moments(Cube, 'SN',[1000; 6], 'Annulus',[10 12]);
    if ~isempty(M1.X) || ~isempty(M1.Y) || numel(M1.StampX1)~=2 || abs(M1.StampX1(1) - 0.4)>0.01
        error('Problem with imUtil.sources.moments: cube input without X/Y');
    end
    % maximal relative flux error (without noise)
    max(abs(([Result.AperPhot(:,4) - Flux]./Flux)))
    
    % PSF case with noise
    Nsrc  = 10;
    Flux  = rand(Nsrc, 1).*1e6;
    Cube  = imUtil.kernel2.gauss(1.5.*ones(Nsrc,1),[25 25]);
    Cube  = Cube.*permute(Flux,[3 2 1]) + randn(25,25,Nsrc).*0.1;    
    Result = imUtil.sources.aperPhotCube(Cube, 13.*ones(Nsrc,1), 13.*ones(Nsrc,1), 'AperRad',[2 4 5 6]);
    % maximal relative flux error (without noise)
    [Flux, abs(([Result.AperPhot(:,4) - Flux]./Flux))]
    if min(abs([Result.AperPhot(:,4) - Flux]./Flux))>0.001
        error('Problem with imUtil.sources.aperPhotCube');
    end
    
    % PSF photometry with Poisson noise
    PSF = imUtil.kernel2.gauss(1.5,[25 25]);
    Nsrc  = 10;
    Flux  = rand(Nsrc, 1).*1e6;
    Cube  = imUtil.kernel2.gauss(1.5.*ones(Nsrc,1),[25 25]);
    Cube  = Cube.*permute(Flux,[3 2 1]); % + randn(25,25,Nsrc).*0.1;
    Cube  = poissrnd(Cube) + randn(25,25,Nsrc).*0.1;
    Result = imUtil.sources.aperPhotCube(Cube, 13.*ones(Nsrc,1), 13.*ones(Nsrc,1), 'AperRad',[2 4 5 6],'PSF',PSF);
    % maximal relative flux error (without noise)
    [Flux, abs(([Result.PsfPhot(:,1) - Flux]./Flux))]
    if max(abs([Result.PsfPhot(:,1) - Flux]./Flux))>0.02
        error('Problem with imUtil.sources.aperPhotCube');
    end

    % with error in PSF
    PSF = imUtil.kernel2.gauss(1.6,[25 25]);
    Nsrc  = 10;
    Flux  = rand(Nsrc, 1).*1e6;
    Cube  = imUtil.kernel2.gauss(1.5.*ones(Nsrc,1),[25 25]);
    Cube  = Cube.*permute(Flux,[3 2 1]); % + randn(25,25,Nsrc).*0.1;
    Cube  = poissrnd(Cube) + randn(25,25,Nsrc).*0.1;
    Result = imUtil.sources.aperPhotCube(Cube, 13.*ones(Nsrc,1), 13.*ones(Nsrc,1), 'AperRad',[2 4 5 6],'PSF',PSF);
    % maximal relative flux error (without noise)
    [Flux, abs(([Result.PsfPhot(:,1) - Flux]./Flux))]
    if max(abs([Result.PsfPhot(:,1) - Flux]./Flux))>0.1
        error('Problem with imUtil.sources.aperPhotCube');
    end
    
    
    % now with sub-pix shifts / no noise
    PSF = imUtil.kernel2.gauss(1.5,[25 25]);
    Nsrc  = 10;
    Flux  = rand(Nsrc, 1).*1e6;
    Cube  = imUtil.kernel2.gauss(1.5.*ones(Nsrc,1),[25 25]);
    % shift the Cube
    DX = rand(Nsrc,1).*4;
    DY = rand(Nsrc,1).*4;
    ShiftedCube = zeros(size(Cube));
    for Isrc=1:1:Nsrc
        [ShiftedCube(:,:,Isrc)]=imUtil.trans.shift_fft(Cube(:,:,Isrc),DX(Isrc),DY(Isrc));
        %[ShiftedCube(:,:,Isrc)]=imUtil.trans.shift_lanczos(Cube(:,:,Isrc),[DX(Isrc),DY(Isrc)]);
    end
    ShiftedCube  = ShiftedCube.*permute(Flux,[3 2 1]); % + randn(25,25,Nsrc).*0.1;
    %ShiftedCube  = poissrnd(ShiftedCube) + randn(25,25,Nsrc).*0.1;
    Result = imUtil.sources.aperPhotCube(ShiftedCube, 13.*ones(Nsrc,1)+DX, 13.*ones(Nsrc,1)+DY, 'AperRad',[2 4 5 6],'PSF',PSF,'SubPixShift','fft');
    [Flux, abs(([Result.PsfPhot(:,1) - Flux]./Flux))]   % fantastic with fft shift!
    % 10% error in PSF width -> 10% bias in photometry
    
    ErrorShift = logspace(-5,0,10);
    for I=1:1:numel(ErrorShift)
        Result = imUtil.sources.aperPhotCube(ShiftedCube, 13.*ones(Nsrc,1)+DX+ErrorShift(I), 13.*ones(Nsrc,1)+DY+ErrorShift(I), 'AperRad',[2 4 5 6],'PSF',PSF,'SubPixShift','fft');
        Mean(I) = mean(abs(([Result.PsfPhot(:,1) - Flux]./Flux)));
    end
    %loglog(ErrorShift.*sqrt(2),Mean)  
    Par = polyfit(log10(ErrorShift.*sqrt(2)),log10(Mean),1);
    % The photometric error as a function of positional shift is: ~0.1 x (TotalShift./1)^2

    % imUtil.sources.psfPhotCube
    % single object
    P=imUtil.kernel2.gauss;
    Ps=imUtil.trans.shift_fft(P,0.4,0.7);
    imUtil.sources.psfPhotCube(Ps, 'PSF',P)
    P=imUtil.kernel2.gauss(1.5.*ones(4,1));
    % multiple objects
    Ps=imUtil.trans.shift_fft(P,[0.4;0.7;-1.1;3.6],[0.7;-0.2;-0.9;-2.6]);
    Ps = Ps.*permute([100 110 200 300],[1 3 2]) + randn(15,15);
    Result = imUtil.sources.psfPhotCube(Ps, 'PSF',P(:,:,1));
    
    % test results
    Nsrc = 4000;
    ShiftXY = rand(Nsrc,2).*6 - 3;
    P  = imUtil.kernel2.gauss;
    Ps=imUtil.trans.shift_fft(P,ShiftXY(:,1), ShiftXY(:,2));
    Flux = rand(Nsrc,1).*100;
    Ps = Ps.*permute(Flux,[3 2 1]);
    [Result, CubePS] = imUtil.sources.psfPhotCube(Ps, 'PSF',P);
%     if max(max(abs([Result.DX, Result.DY]-ShiftXY)))>1e-8
%         error('Problem with imUtil.sources.psfPhotCube position finiding');
%     end
%     if max(abs(Result.Flux-Flux))>1e-6
%        error('Problem with imUtil.sources.psfPhotCube flux finiding');
%     end 
    
    % with noise
    Nsrc = 4000;
    ShiftXY = rand(Nsrc,2).*6 - 3;
    P  = imUtil.kernel2.gauss;
    Ps=imUtil.trans.shift_fft(P,ShiftXY(:,1), ShiftXY(:,2));
    Flux = rand(Nsrc,1).*1000;
    Ps = Ps.*permute(Flux,[3 2 1]);
    Ps = Ps + randn(15,15,Nsrc);
    [Result, CubePS] = imUtil.sources.psfPhotCube(Ps, 'PSF',P);
    
    ResidPos = sum(([Result.DX, Result.DY]-ShiftXY).^2,2);
    % error in poistions
    %loglog(Flux, ResidPos, '.')
    % error in flux
    %semilogx(Flux, (Result.Flux - Flux)./Flux,'.')
    
    
    
    Image = randn(1600,1600,5);
    [Pos1, BW, MaxIsn] = imUtil.sources.findLocalMax(Image,'Variance',1,'Threshold',3, 'Algo','findlocal');
    [Pos2, BW, MaxIsn] = imUtil.sources.findLocalMax(Image,'Variance',1,'Threshold',3, 'Algo','findlocalmex');
    [Pos3, BW, MaxIsn] = imUtil.sources.findLocalMax(Image,'Variance',1,'Threshold',3, 'Algo','imregionalmax');
    
    
    
    % Test mex_find_local_max_single
	% Author: Uri, 02/2022
	
	%z=[0 0 0;0 10 0;0 0 0;0 0 0];
	%[a,b,c,d] = imUtil.sources.mex_find_local_max_single(z,2.3,4,0.025)
	%dsfsdf
	Conn=4;
	z2 = randn(5000);
	z3 = zeros(size(z2)+2);
	size(z3)
	z3(2:(size(z2,1)+1),2:(size(z2,2)+1))= z2;
	%z3=z2;
	Thresh = 2.1;
	AllocateFrac = 0.02;
	%tic
	%[a1,b1,c1,d1] = imUtil.sources.mex_find_local_max_single(z3,Thresh,Conn,AllocateFrac);
	%[a1,b1,c1,d1] = imUtil.sources.mex.findLocalMaxAboveThreshold_mex_double(z3,Thresh,Conn,AllocateFrac);
	%toc

	% Run MEX version
	%tic
	%[a2,b2,c2,d2] = imUtil.sources.mex_find_local_max_single2(z3,Thresh,Conn,AllocateFrac);
	%toc

	% Run MATLAB version
	tic
	[a3,b3,c3,d3] = imUtil.sources.findLocalMaxAboveThreshold(z3,Thresh,Conn,AllocateFrac);
	toc

% 	fprintf('comparison 1st Mex \n') 
% 	all(a1(1:min(length(a1),length(a3)))==a3(1:min(length(a1),length(a3))))
% 	all(b1(1:min(length(b1),length(b3)))==b3(1:min(length(b1),length(b3))))
% 	all(c1(1:min(length(c1),length(c3)))==c3(1:min(length(c1),length(c3))))
% 	all(all(d1==d3))
% 	fprintf('comparison 1st Mex \n')
% 	all(a2(1:min(length(a2),length(a3)))==a3(1:min(length(a1),length(a3))))
% 	all(b2(1:min(length(b2),length(b3)))==b3(1:min(length(b1),length(b3))))
% 	all(c2(1:min(length(c2),length(c3)))==c3(1:min(length(c1),length(c3))))
% 	all(all(d2==d3))    
    
    Result = true;
end

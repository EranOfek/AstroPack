function tests = test_sparse2full
    % Unit tests for imUtil.image.sparse2full and its 'ExtrapMethod' option,
    % and for the 'ExtrapMethod' option of imUtil.background.backVar.
    %
    % 'nearest' (default) repeats the edge values, so the image is flat
    % between the outermost sampled points and the image edges. 'linear'
    % extrapolates the gradient between the last two points to the edges.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testDefaultIsNearest(testCase)
    % The default is identical to 'nearest' and to the original algorithm.

    % Arrange
    [S, VecX, VecY, SizeIJ] = makeGrid(@(X,Y) 100 + 0.01*X - 0.02*Y + 1e-5*X.*Y);
    S(2,2) = S(2,2) + 3;   % not a plane

    % Act
    R0 = imUtil.image.sparse2full(S, VecX, VecY, SizeIJ);
    R1 = imUtil.image.sparse2full(S, VecX, VecY, SizeIJ, 'ExtrapMethod','nearest');
    Sp = [S(:,1), S, S(:,end)];
    Sp = [Sp(1,:); Sp; Sp(end,:)];
    Ref = interp2([1; VecX(:); SizeIJ(2)], [1; VecY(:); SizeIJ(1)], Sp, (1:SizeIJ(2)), (1:SizeIJ(1)).', 'linear');

    % Assert
    testCase.verifyEqual(R0, R1, 'The default differs from ExtrapMethod=nearest.');
    testCase.verifyEqual(R0, Ref, 'nearest differs from the original algorithm.');
    testCase.verifyEqual(R0(1:VecY(1),VecX(2)), repmat(S(1,2), VecY(1), 1), 'nearest is not flat below the first block center.');
end

function testLinearReproducesBilinearSurface(testCase)
    % 'linear' reproduces a+bx+cy+dxy exactly everywhere, including the edges.

    % Arrange
    Fun = @(X,Y) 100 + 0.01*X - 0.02*Y + 1e-5*X.*Y;
    [S, VecX, VecY, SizeIJ] = makeGrid(Fun);
    [MatX, MatY] = meshgrid(1:SizeIJ(2), 1:SizeIJ(1));

    % Act
    R = imUtil.image.sparse2full(S, VecX, VecY, SizeIJ, 'ExtrapMethod','linear');

    % Assert
    testCase.verifyEqual(size(R), SizeIJ, 'Wrong output size.');
    testCase.verifyEqual(R, Fun(MatX, MatY), 'AbsTol',1e-9, 'linear does not reproduce a bilinear surface.');
end

function testLinearInterpolatesLikeNearestInside(testCase)
    % Between the outermost block centers both methods give the same image.

    % Arrange
    [S, VecX, VecY, SizeIJ] = makeGrid(@(X,Y) 100 + 0.01*X);
    S(2,3) = S(2,3) + 5;

    % Act
    Rn = imUtil.image.sparse2full(S, VecX, VecY, SizeIJ, 'ExtrapMethod','nearest');
    Rl = imUtil.image.sparse2full(S, VecX, VecY, SizeIJ, 'ExtrapMethod','linear');

    % Assert
    In = {VecY(1):VecY(end), VecX(1):VecX(end)};
    testCase.verifyEqual(Rl(In{:}), Rn(In{:}), 'AbsTol',1e-9, 'linear and nearest differ between the block centers.');
end

function testLinearSingleRowFallsBackToNearest(testCase)
    % With a single sampled row, 'linear' falls back to 'nearest'.

    % Arrange
    S = [10 20 30]; VecX = [100 300 500]; VecY = 50; SizeIJ = [100 600];

    % Act
    Rn = imUtil.image.sparse2full(S, VecX, VecY, SizeIJ, 'ExtrapMethod','nearest');
    Rl = imUtil.image.sparse2full(S, VecX, VecY, SizeIJ, 'ExtrapMethod','linear');

    % Assert
    testCase.verifyEqual(Rl, Rn, 'A single row should fall back to nearest.');
end

function testLinearKeepsSingle(testCase)
    % A single-precision input gives a single-precision output.

    % Arrange
    [S, VecX, VecY, SizeIJ] = makeGrid(@(X,Y) 100 + 0.01*X);

    % Act
    R = imUtil.image.sparse2full(single(S), VecX, VecY, SizeIJ, 'ExtrapMethod','linear');

    % Assert
    testCase.verifyClass(R, 'single', 'The output class should follow the input.');
end

function testUnknownExtrapMethodErrors(testCase)
    % An unknown ExtrapMethod raises an error.

    % Arrange
    [S, VecX, VecY, SizeIJ] = makeGrid(@(X,Y) 100 + 0*X);

    % Act / Assert
    testCase.verifyError(@() imUtil.image.sparse2full(S, VecX, VecY, SizeIJ, 'ExtrapMethod','cubic'), ?MException, 'An unknown ExtrapMethod should error.');
end

function testBackVarLinearFollowsSlope(testCase)
    % On a sloping sky, backVar with 'linear' follows the slope to the edges;
    % 'nearest' is off there by about the slope times the distance to the
    % last block center. The variance is floored at the lowest block value.

    % Arrange
    rng(1);
    N = 1716; Slope = 0.2;   % ADU/px, ~340 ADU across the image
    [MatX, MatY] = meshgrid(1:N, 1:N);
    Sky = 700 + Slope*MatY;
    Image = Sky + sqrt(Sky).*randn(N, N);
    BVArgs = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[512 512], 'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}}};

    % Act
    [Bn] = imUtil.background.backVar(Image, BVArgs{:});
    [Bl, Vl, ~, VarSmall] = imUtil.background.backVar(Image, BVArgs{:}, 'ExtrapMethod','linear');

    % Assert
    Edge = [1:100, N-99:N];
    ErrN = median(abs(Bn(Edge,:) - Sky(Edge,:)), 'all');
    ErrL = median(abs(Bl(Edge,:) - Sky(Edge,:)), 'all');
    testCase.verifyGreaterThan(ErrN, 40, 'nearest should be off by ~Slope*300 px at the edges.');
    testCase.verifyLessThan(ErrL, 5, 'linear should follow the slope at the edges.');
    testCase.verifyGreaterThanOrEqual(min(Vl, [], 'all'), min(VarSmall, [], 'all'), 'The variance should be floored at the lowest block value.');
end

%% Helpers

function [S, VecX, VecY, SizeIJ] = makeGrid(Fun)
    % 3x3 block centers of a 1716x1716 LAST crop with 512 px blocks.
    SizeIJ = [1716 1716];
    VecX   = [319; 858; 1398];
    VecY   = [319; 858; 1398];
    [GX, GY] = meshgrid(VecX, VecY);
    S = Fun(GX, GY);
end

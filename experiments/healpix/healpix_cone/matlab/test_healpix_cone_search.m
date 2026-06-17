% ***************************************************************************
% Project     : AstroPack
% Filename    : test_healpix_cone_search.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : Comprehensive matlab.unittest suite for HealpixConeSearch.m
%               (mirrors test_healpix_cone_search.py).
% ***************************************************************************
classdef test_healpix_cone_search < matlab.unittest.TestCase
    % test_healpix_cone_search  Comprehensive test suite (mirrors pytest).
    %
    %   Run:
    %       addpath('experiments/healpix/healpix_cone/matlab');
    %       runtests('test_healpix_cone_search')

    methods (Test)

        % -------------------------------------------------------------------
        % 1. bestNsideForRadius
        % -------------------------------------------------------------------

        function testBestNsideReturnsPowerOfTwo(testCase)
            Radii = [0.001, 0.01, 0.1, 0.5, 1.0, 5.0, 30.0];
            for R = Radii
                Ns = HealpixConeSearch.bestNsideForRadius(R);
                testCase.verifyGreaterThan(Ns, 0);
                testCase.verifyEqual(bitand(Ns, Ns - 1), 0, ...
                    sprintf('NSide=%d is not a power of 2', Ns));
            end
        end

        function testBestNsideNeverExceedsNsideCat(testCase)
            for R = [1e-6, 1e-5, 0.0001]
                Ns = HealpixConeSearch.bestNsideForRadius(R);
                testCase.verifyLessThanOrEqual(Ns, HealpixConeSearch.NSIDE_CAT);
            end
        end

        function testBestNsideMinimumIsOne(testCase)
            Ns = HealpixConeSearch.bestNsideForRadius(89.0);   % huge radius
            testCase.verifyGreaterThanOrEqual(Ns, 1);
        end

        function testBestNsidePixelSizeCoversRadius(testCase)
            % Conservative mode: pixel size (1/NSide) must be >= search radius.
            for RDeg = [0.01, 0.1, 0.5, 1.0, 2.0, 5.0]
                Ns = HealpixConeSearch.bestNsideForRadius(RDeg, "conservative");
                RRad = deg2rad(RDeg);
                PixSizeRad = 1.0 / Ns;
                testCase.verifyGreaterThanOrEqual(PixSizeRad, RRad, ...
                    sprintf('radius=%g° NSide=%d', RDeg, Ns));
            end
        end

        function testBestNsideRaisesOnZeroRadius(testCase)
            testCase.verifyError(@() HealpixConeSearch.bestNsideForRadius(0.0), ...
                'MATLAB:validator:mustBePositive');
        end

        function testBestNsideRaisesOnNegativeRadius(testCase)
            testCase.verifyError(@() HealpixConeSearch.bestNsideForRadius(-1.0), ...
                'MATLAB:validator:mustBePositive');
        end

        function testBestNsideKnownValuesConservative(testCase)
            % R=1° = 0.017453 rad -> 1/r = 57.3 -> floor(log2) = 5 -> NSide = 32
            Ns = HealpixConeSearch.bestNsideForRadius(1.0, "conservative");
            testCase.verifyEqual(Ns, 32);

            % R=0.1° = 0.001745 rad -> 1/r = 572.9 -> floor(log2) = 9 -> 512
            Ns = HealpixConeSearch.bestNsideForRadius(0.1, "conservative");
            testCase.verifyEqual(Ns, 512);
        end

        function testBestNsideKnownValuesArea(testCase)
            Ns = HealpixConeSearch.bestNsideForRadius(1.0, "area");
            testCase.verifyEqual(Ns, 32);
        end

        function testBestNsideKnownValuesCircumradius(testCase)
            Ns = HealpixConeSearch.bestNsideForRadius(1.0, "circumradius");
            testCase.verifyEqual(Ns, 64);
        end

        function testBestNsideCircumradiusFinerThanConservative(testCase)
            for R = [0.01, 0.1, 0.5, 1.0, 5.0]
                NsCons = HealpixConeSearch.bestNsideForRadius(R, "conservative");
                NsCirc = HealpixConeSearch.bestNsideForRadius(R, "circumradius");
                testCase.verifyGreaterThanOrEqual(NsCirc, NsCons);
            end
        end

        function testBestNsideAreaCoarsest(testCase)
            for R = [0.01, 0.1, 0.5, 1.0, 5.0]
                NsCons = HealpixConeSearch.bestNsideForRadius(R, "conservative");
                NsArea = HealpixConeSearch.bestNsideForRadius(R, "area");
                testCase.verifyLessThanOrEqual(NsArea, NsCons);
            end
        end

        function testBestNsideInvalidMode(testCase)
            testCase.verifyError( ...
                @() HealpixConeSearch.bestNsideForRadius(1.0, "badmode"), ...
                'HealpixConeSearch:UnknownMode');
        end

        % -------------------------------------------------------------------
        % 2. pixelsToRanges
        % -------------------------------------------------------------------

        function testPixelsToRangesSinglePixelNsideCat(testCase)
            Pix = int64(42);
            Ranges = HealpixConeSearch.pixelsToRanges(Pix, HealpixConeSearch.NSIDE_CAT);
            testCase.verifyEqual(Ranges, int64([42, 42]));
        end

        function testPixelsToRangesContiguousPixelsMerge(testCase)
            NSideS = HealpixConeSearch.NSIDE_CAT / 2;
            Pix = int64([0; 1]);
            Ranges = HealpixConeSearch.pixelsToRanges(Pix, NSideS);
            testCase.verifyEqual(size(Ranges, 1), 1);
            testCase.verifyEqual(Ranges, int64([0, 7]));
        end

        function testPixelsToRangesNonContiguousPixelsSeparate(testCase)
            NSideS = HealpixConeSearch.NSIDE_CAT / 2;
            Pix = int64([0; 10]);
            Ranges = HealpixConeSearch.pixelsToRanges(Pix, NSideS);
            testCase.verifyEqual(size(Ranges, 1), 2);
        end

        function testPixelsToRangesRangeWidthCorrect(testCase)
            NSideS = HealpixConeSearch.NSIDE_CAT / 4;
            Pix = int64(5);
            Ranges = HealpixConeSearch.pixelsToRanges(Pix, NSideS);
            Lo = Ranges(1, 1);
            Hi = Ranges(1, 2);
            testCase.verifyEqual(Hi - Lo + 1, 16);
            testCase.verifyEqual(Lo, 5 * 16);
        end

        function testPixelsToRangesAllIdsWithinBounds(testCase)
            NSideS = 64;
            NPix = 12 * 64 * 64;
            Pix = int64((0:min(99, NPix-1))');
            Ranges = HealpixConeSearch.pixelsToRanges(Pix, NSideS);
            for I = 1:size(Ranges, 1)
                testCase.verifyGreaterThanOrEqual(Ranges(I, 1), 0);
                testCase.verifyLessThanOrEqual(Ranges(I, 2), HealpixConeSearch.MAX_PIX_ID);
            end
        end

        function testPixelsToRangesDuplicatesHandled(testCase)
            Pix = int64([5; 5; 5]);
            Ranges = HealpixConeSearch.pixelsToRanges(Pix, HealpixConeSearch.NSIDE_CAT);
            testCase.verifyEqual(size(Ranges, 1), 1);
            testCase.verifyEqual(Ranges, int64([5, 5]));
        end

        function testPixelsToRangesAreSorted(testCase)
            Pix = int64([100; 10; 50; 1]);
            Ranges = HealpixConeSearch.pixelsToRanges(Pix, HealpixConeSearch.NSIDE_CAT);
            Los = Ranges(:, 1);
            testCase.verifyEqual(Los, sort(Los));
        end

        function testPixelsToRangesNonOverlapping(testCase)
            Pix = int64((0:199)');
            Ranges = HealpixConeSearch.pixelsToRanges(Pix, HealpixConeSearch.NSIDE_CAT / 8);
            for I = 1:size(Ranges, 1) - 1
                testCase.verifyLessThan(Ranges(I, 2), Ranges(I+1, 1), ...
                    'Ranges overlap!');
            end
        end

        % -------------------------------------------------------------------
        % 3. coneToPixelRanges — NEIGHBOR
        % -------------------------------------------------------------------

        function testNeighborReturnsPixelRangesObject(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(0.0, 0.0, 1.0, Algo.NEIGHBOR);
            testCase.verifyClass(Pr, 'PixelRanges');
        end

        function testNeighborAtMost9Ranges(testCase)
            Cases = [
                0.0,   0.0,  1.0;
                254.0, 64.0, 1.0;
                180.0, 89.9, 0.5;
                90.0, -89.9, 0.5;
                45.0,  45.0,  0.1;
            ];
            for I = 1:size(Cases, 1)
                Pr = HealpixConeSearch.coneToPixelRanges( ...
                    Cases(I,1), Cases(I,2), Cases(I,3), Algo.NEIGHBOR);
                testCase.verifyLessThanOrEqual(Pr.NRanges, 9, ...
                    sprintf('RA=%g Dec=%g R=%g: got %d ranges', ...
                    Cases(I,1), Cases(I,2), Cases(I,3), Pr.NRanges));
            end
        end

        function testNeighborAlgoStoredCorrectly(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(0.0, 0.0, 1.0, Algo.NEIGHBOR);
            testCase.verifyEqual(Pr.Algo, Algo.NEIGHBOR);
        end

        function testNeighborNsideSearchCorrect(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(0.0, 0.0, 1.0, Algo.NEIGHBOR);
            Expected = HealpixConeSearch.bestNsideForRadius(1.0, "conservative");
            testCase.verifyEqual(Pr.NSideSearch, Expected);
        end

        function testNeighborAllRangeIdsValid(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(45.0, 30.0, 1.0, Algo.NEIGHBOR);
            for I = 1:Pr.NRanges
                Lo = Pr.Ranges(I, 1);
                Hi = Pr.Ranges(I, 2);
                testCase.verifyGreaterThanOrEqual(Lo, 0);
                testCase.verifyLessThanOrEqual(Hi, HealpixConeSearch.MAX_PIX_ID);
                testCase.verifyLessThanOrEqual(Lo, Hi);
            end
        end

        function testNeighborRangesNonOverlapping(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(45.0, 30.0, 1.0, Algo.NEIGHBOR);
            for I = 1:Pr.NRanges - 1
                testCase.verifyLessThan(Pr.Ranges(I, 2), Pr.Ranges(I+1, 1));
            end
        end

        function testNeighborCenterPixelCovered(testCase)
            Backend = Backend.getBackend();
            Ra = 254.0; Dec = 64.0; R = 1.0;
            Pr = HealpixConeSearch.coneToPixelRanges(Ra, Dec, R, Algo.NEIGHBOR);
            CenterPix = Backend.ang2pixNested(HealpixConeSearch.NSIDE_CAT, Ra, Dec);
            testCase.verifyTrue(rangesCoverPixel(Pr.Ranges, CenterPix), ...
                sprintf('Center pixel %d not in ranges', CenterPix));
        end

        % -------------------------------------------------------------------
        % 4. coneToPixelRanges — CONE
        % -------------------------------------------------------------------

        function testConeReturnsPixelRangesObject(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(0.0, 0.0, 1.0, Algo.CONE);
            testCase.verifyClass(Pr, 'PixelRanges');
        end

        function testConeAlgoStoredCorrectly(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(0.0, 0.0, 1.0, Algo.CONE);
            testCase.verifyEqual(Pr.Algo, Algo.CONE);
        end

        function testConeAllRangeIdsValid(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(45.0, 30.0, 1.0, Algo.CONE);
            for I = 1:Pr.NRanges
                Lo = Pr.Ranges(I, 1);
                Hi = Pr.Ranges(I, 2);
                testCase.verifyGreaterThanOrEqual(Lo, 0);
                testCase.verifyLessThanOrEqual(Hi, HealpixConeSearch.MAX_PIX_ID);
            end
        end

        function testConeCenterPixelCovered(testCase)
            Backend = Backend.getBackend();
            Ra = 254.0; Dec = 64.0; R = 1.0;
            Pr = HealpixConeSearch.coneToPixelRanges(Ra, Dec, R, Algo.CONE);
            CenterPix = Backend.ang2pixNested(HealpixConeSearch.NSIDE_CAT, Ra, Dec);
            testCase.verifyTrue(rangesCoverPixel(Pr.Ranges, CenterPix));
        end

        function testConeFewerRangesThanNeighbor(testCase)
            Ra = 254.0; Dec = 64.0; R = 1.0;
            PrN = HealpixConeSearch.coneToPixelRanges(Ra, Dec, R, Algo.NEIGHBOR);
            PrC = HealpixConeSearch.coneToPixelRanges(Ra, Dec, R, Algo.CONE);
            testCase.verifyLessThanOrEqual(PrC.NRanges, PrN.NRanges, ...
                sprintf('CONE (%d) > NEIGHBOR (%d)', PrC.NRanges, PrN.NRanges));
        end

        function testConeFewerTotalPixelsThanNeighbor(testCase)
            Ra = 254.0; Dec = 64.0; R = 1.0;
            PrN = HealpixConeSearch.coneToPixelRanges(Ra, Dec, R, Algo.NEIGHBOR);
            PrC = HealpixConeSearch.coneToPixelRanges(Ra, Dec, R, Algo.CONE);
            TotalN = sum(PrN.Ranges(:, 2) - PrN.Ranges(:, 1) + 1);
            TotalC = sum(PrC.Ranges(:, 2) - PrC.Ranges(:, 1) + 1);
            testCase.verifyLessThanOrEqual(TotalC, TotalN);
        end

        function testConeNoFalseNegativesInsideCone(testCase)
            Ra0 = 100.0; Dec0 = 20.0; R = 1.0;
            Pr = HealpixConeSearch.coneToPixelRanges(Ra0, Dec0, R, Algo.CONE);

            AllPix = [];
            for I = 1:Pr.NRanges
                Lo = Pr.Ranges(I, 1);
                Hi = Pr.Ranges(I, 2);
                AllPix = [AllPix, Lo:min(Hi, Lo + 19)]; %#ok<AGROW>
            end
            if isempty(AllPix)
                testCase.assumeFail('no pixels to sample');
            end

            InnerR = R * 0.6;
            Missed = 0;
            Checked = 0;
            for P = AllPix(1:min(200, numel(AllPix)))
                [PRa, PDec] = pixCenter(HealpixConeSearch.NSIDE_CAT, P);
                D = sphereDistDeg(Ra0, Dec0, PRa, PDec);
                if D <= InnerR
                    if ~rangesCoverPixel(Pr.Ranges, P)
                        Missed = Missed + 1;
                    end
                    Checked = Checked + 1;
                end
            end
            testCase.verifyEqual(Missed, 0, ...
                sprintf('%d/%d inner-cone pixels not covered', Missed, Checked));
        end

        function testConeSubPixelRadiusReturnsAtLeastOneRange(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(45.0, 30.0, 0.001, Algo.CONE);
            testCase.verifyGreaterThanOrEqual(Pr.NRanges, 1);
        end

        function testConeRangesNonOverlapping(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(254.0, 64.0, 1.0, Algo.CONE);
            for I = 1:Pr.NRanges - 1
                testCase.verifyLessThan(Pr.Ranges(I, 2), Pr.Ranges(I+1, 1));
            end
        end

        % -------------------------------------------------------------------
        % 5. Input validation
        % -------------------------------------------------------------------

        function testInvalidRa(testCase)
            for Ra = [-1.0, 360.0, 400.0]
                testCase.verifyError( ...
                    @() HealpixConeSearch.coneToPixelRanges(Ra, 0.0, 1.0), ...
                    'HealpixConeSearch:InvalidRa');
            end
        end

        function testInvalidDec(testCase)
            for Dec = [-91.0, 91.0, 180.0]
                testCase.verifyError( ...
                    @() HealpixConeSearch.coneToPixelRanges(0.0, Dec, 1.0), ...
                    'HealpixConeSearch:InvalidDec');
            end
        end

        function testInvalidRadius(testCase)
            for R = [0.0, -1.0]
                testCase.verifyError( ...
                    @() HealpixConeSearch.coneToPixelRanges(0.0, 0.0, R), ...
                    'HealpixConeSearch:InvalidRadius');
            end
        end

        % -------------------------------------------------------------------
        % 6. Pixel ID bounds
        % -------------------------------------------------------------------

        function testMaxPixelIdConstant(testCase)
            testCase.verifyEqual(HealpixConeSearch.MAX_PIX_ID, ...
                int64(12) * int64(HealpixConeSearch.NSIDE_CAT)^2 - 1);
        end

        function testNsideCat(testCase)
            testCase.verifyEqual(HealpixConeSearch.NSIDE_CAT, 65536);
        end

        function testMaxIdExceedsUint32(testCase)
            testCase.verifyGreaterThan(HealpixConeSearch.MAX_PIX_ID, int64(2^32 - 1));
        end

        function testMaxIdFitsUint64(testCase)
            testCase.verifyLessThan(HealpixConeSearch.MAX_PIX_ID, int64(2^64 - 1));
        end

        function testAllRangesWithinBounds(testCase)
            Cases = [0.0, 0.0, 1.0; 254.0, 64.0, 1.0; 180.0, 89.0, 0.5];
            for I = 1:size(Cases, 1)
                for AlgoVal = [Algo.CONE, Algo.NEIGHBOR]
                    Pr = HealpixConeSearch.coneToPixelRanges( ...
                        Cases(I,1), Cases(I,2), Cases(I,3), AlgoVal);
                    for J = 1:Pr.NRanges
                        testCase.verifyGreaterThanOrEqual(Pr.Ranges(J, 1), 0);
                        testCase.verifyLessThanOrEqual(Pr.Ranges(J, 2), ...
                            HealpixConeSearch.MAX_PIX_ID);
                    end
                end
            end
        end

        % -------------------------------------------------------------------
        % 7. Direction cosines
        % -------------------------------------------------------------------

        function testDirectionCosinesUnitVector(testCase)
            Pairs = [0, 0; 90, 0; 0, 90; 45, 45; 254, 64];
            for I = 1:size(Pairs, 1)
                [Cx, Cy, Cz] = HealpixConeSearch.directionCosines(Pairs(I,1), Pairs(I,2));
                Norm = sqrt(Cx^2 + Cy^2 + Cz^2);
                testCase.verifyEqual(Norm, 1.0, 'AbsTol', 1e-12, ...
                    sprintf('Not unit vector at RA=%g Dec=%g', Pairs(I,1), Pairs(I,2)));
            end
        end

        function testDirectionCosinesKnownValues(testCase)
            [Cx, Cy, Cz] = HealpixConeSearch.directionCosines(0.0, 0.0);
            testCase.verifyEqual(Cx, 1.0, 'AbsTol', 1e-12);
            testCase.verifyEqual(Cy, 0.0, 'AbsTol', 1e-12);
            testCase.verifyEqual(Cz, 0.0, 'AbsTol', 1e-12);
        end

        function testDirectionCosinesNorthPole(testCase)
            [~, ~, Cz] = HealpixConeSearch.directionCosines(0.0, 90.0);
            testCase.verifyEqual(Cz, 1.0, 'AbsTol', 1e-12);
        end

        function testDirectionCosinesSouthPole(testCase)
            [~, ~, Cz] = HealpixConeSearch.directionCosines(0.0, -90.0);
            testCase.verifyEqual(Cz, -1.0, 'AbsTol', 1e-12);
        end

        function testDirectionCosinesDotProductIsCosDistance(testCase)
            Pairs = [0, 0, 1, 0; 45, 30, 50, 35; 254, 64, 256, 65];
            for I = 1:size(Pairs, 1)
                C1 = HealpixConeSearch.directionCosines(Pairs(I,1), Pairs(I,2));
                C2 = HealpixConeSearch.directionCosines(Pairs(I,3), Pairs(I,4));
                Dot = C1(1)*C2(1) + C1(2)*C2(2) + C1(3)*C2(3);
                D = sphereDistDeg(Pairs(I,1), Pairs(I,2), Pairs(I,3), Pairs(I,4));
                testCase.verifyEqual(Dot, cos(deg2rad(D)), 'AbsTol', 1e-10);
            end
        end

        % -------------------------------------------------------------------
        % 8. SQL generation
        % -------------------------------------------------------------------

        function testSqlReturnsTuple(testCase)
            [Sql, Pf] = sqlAndPf(testCase);
            testCase.verifyNotEmpty(Sql);
            testCase.verifyClass(Sql, 'string');
            testCase.verifyClass(Pf, 'string');
        end

        function testSqlContainsSelect(testCase)
            [Sql, ~] = sqlAndPf(testCase);
            testCase.verifyTrue(startsWith(upper(strtrim(Sql)), "SELECT"));
        end

        function testSqlContainsTableName(testCase)
            [Sql, ~] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
                'my_catalog', 'upix_high');
            testCase.verifyTrue(contains(Sql, 'my_catalog'));
        end

        function testSqlContainsColumnName(testCase)
            [Sql, ~] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
                'proc_src', 'healpix_id');
            testCase.verifyTrue(contains(Sql, 'healpix_id'));
        end

        function testSqlContainsBetween(testCase)
            [Sql, ~] = sqlAndPf(testCase);
            testCase.verifyTrue(contains(upper(Sql), 'BETWEEN'));
        end

        function testSqlRangeValuesAreIntegers(testCase)
            [Sql, ~] = sqlAndPf(testCase);
            Tokens = regexp(char(Sql), 'BETWEEN\s+(\d+)\s+AND\s+(\d+)', 'tokens');
            testCase.verifyNotEmpty(Tokens);
            for I = 1:numel(Tokens)
                Lo = str2double(Tokens{I}{1});
                Hi = str2double(Tokens{I}{2});
                testCase.verifyLessThanOrEqual(Lo, Hi);
                testCase.verifyGreaterThanOrEqual(Lo, 0);
                testCase.verifyLessThanOrEqual(Hi, HealpixConeSearch.MAX_PIX_ID);
            end
        end

        function testPostFilterNoneWhenDisabled(testCase)
            [~, Pf] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
                't', 'c', 'PostFilter', false);
            testCase.verifyEqual(Pf, '');
        end

        function testPostFilterCosineContainsAnd(testCase)
            [~, Pf] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
                't', 'c', 'PostFilter', true, 'PostFilterMode', 'cosine');
            testCase.verifyTrue(contains(upper(Pf), 'AND'));
        end

        function testPostFilterCosineHasThreeTerms(testCase)
            [~, Pf] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
                't', 'c', 'PostFilter', true, 'PostFilterMode', 'cosine');
            testCase.verifyGreaterThanOrEqual(count(Pf, '*'), 3);
        end

        function testPostFilterGreatcircleContainsGreatcircleangle(testCase)
            [~, Pf] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
                't', 'c', 'PostFilter', true, 'PostFilterMode', 'greatcircle');
            testCase.verifyTrue(contains(Pf, 'greatCircleAngle'));
        end

        function testPostFilterInvalidMode(testCase)
            testCase.verifyError(@() HealpixConeSearch.coneSearchSql( ...
                0.0, 0.0, 1.0, 't', 'c', ...
                'PostFilter', true, 'PostFilterMode', 'bad'), ...
                'HealpixConeSearch:UnknownPostFilterMode');
        end

        function testExtraColumnsInSelect(testCase)
            [Sql, ~] = HealpixConeSearch.coneSearchSql(0.0, 0.0, 1.0, ...
                't', 'c', 'ExtraColumns', 'id, ra, dec');
            testCase.verifyTrue(contains(Sql, 'id, ra, dec'));
        end

        function testBothAlgosProduceValidSql(testCase)
            for AlgoVal = [Algo.CONE, Algo.NEIGHBOR]
                [Sql, ~] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
                    't', 'c', 'Algo', AlgoVal);
                testCase.verifyTrue(contains(upper(Sql), 'SELECT'));
                testCase.verifyTrue(contains(upper(Sql), 'BETWEEN'));
            end
        end

        function testSqlFullContainsAndFilter(testCase)
            Sql = HealpixConeSearch.coneSearchSqlFull(254.0, 64.0, 1.0, 't', 'c');
            testCase.verifyTrue(contains(Sql, 'AND'));
        end

        function testCustomTableAndColumn(testCase)
            [Sql, ~] = HealpixConeSearch.coneSearchSql(1.0, 1.0, 0.5, ...
                'my_survey.detections', 'hpx_nested');
            testCase.verifyTrue(contains(Sql, 'my_survey.detections'));
            testCase.verifyTrue(contains(Sql, 'hpx_nested'));
        end

        function testRangesInSqlMatchPixelRanges(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(254.0, 64.0, 1.0, Algo.CONE);
            [Sql, ~] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
                't', 'c', 'Algo', Algo.CONE);
            Tokens = regexp(char(Sql), 'BETWEEN\s+(\d+)\s+AND\s+(\d+)', 'tokens');
            SqlRanges = zeros(numel(Tokens), 2, 'int64');
            for I = 1:numel(Tokens)
                SqlRanges(I, 1) = str2double(Tokens{I}{1});
                SqlRanges(I, 2) = str2double(Tokens{I}{2});
            end
            testCase.verifyEqual(SqlRanges, Pr.Ranges);
        end

        % -------------------------------------------------------------------
        % 9. Edge cases / special sky positions
        % -------------------------------------------------------------------

        function testNoExceptionAtSpecialPositions(testCase)
            Cases = [
                0.0,   90.0, 0.5;
                0.0,  -90.0, 0.5;
                0.0,    0.0, 1.0;
                359.9,  0.0, 1.0;
                0.1,    0.0, 1.0;
            ];
            for I = 1:size(Cases, 1)
                for AlgoVal = [Algo.CONE, Algo.NEIGHBOR]
                    Pr = HealpixConeSearch.coneToPixelRanges( ...
                        Cases(I,1), Cases(I,2), Cases(I,3), AlgoVal);
                    testCase.verifyGreaterThanOrEqual(Pr.NRanges, 1);
                end
            end
        end

        function testVerySmallRadius(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(45.0, 30.0, 1e-4, Algo.CONE);
            testCase.verifyGreaterThanOrEqual(Pr.NRanges, 1);
            for I = 1:Pr.NRanges
                testCase.verifyGreaterThanOrEqual(Pr.Ranges(I, 1), 0);
                testCase.verifyLessThanOrEqual(Pr.Ranges(I, 2), HealpixConeSearch.MAX_PIX_ID);
            end
        end

        function testLargeRadius(testCase)
            Pr = HealpixConeSearch.coneToPixelRanges(45.0, 30.0, 10.0, Algo.CONE);
            testCase.verifyGreaterThanOrEqual(Pr.NRanges, 1);
        end

        function testRaZeroAnd359SimilarCoverage(testCase)
            Pr1 = HealpixConeSearch.coneToPixelRanges(0.5,   0.0, 1.0, Algo.CONE);
            Pr2 = HealpixConeSearch.coneToPixelRanges(359.5, 0.0, 1.0, Algo.CONE);
            T1 = sum(Pr1.Ranges(:, 2) - Pr1.Ranges(:, 1) + 1);
            T2 = sum(Pr2.Ranges(:, 2) - Pr2.Ranges(:, 1) + 1);
            Ratio = max(T1, T2) / max(min(T1, T2), 1);
            testCase.verifyLessThan(Ratio, 1.5, ...
                sprintf('RA wrap asymmetry: t1=%d t2=%d', T1, T2));
        end

        % -------------------------------------------------------------------
        % 10. Backend availability
        % -------------------------------------------------------------------

        function testBackendLoads(testCase)
            B = Backend.getBackend();
            testCase.verifyNotEmpty(B);
            testCase.verifyEqual(B.Name, 'celestial.healpix');
        end

        function testBackendAng2pixRoundTrip(testCase)
            B = Backend.getBackend();
            Ra = 123.456; Dec = -34.567;
            Pix = B.ang2pixNested(HealpixConeSearch.NSIDE_CAT, Ra, Dec);
            [Ra2, Dec2] = B.pix2angNested(HealpixConeSearch.NSIDE_CAT, Pix);
            testCase.verifyLessThan(sphereDistDeg(Ra, Dec, Ra2, Dec2), 0.01);
        end

        function testBackendNeighboursReturnsArray(testCase)
            B = Backend.getBackend();
            Pix = B.ang2pixNested(64, 45.0, 30.0);
            Nb = B.neighboursNested(64, Pix);
            testCase.verifyGreaterThanOrEqual(numel(Nb), 1);
        end

        function testBackendQueryDiscReturnsArray(testCase)
            B = Backend.getBackend();
            Pix = B.queryDiscNested(64, 45.0, 30.0, 1.0);
            testCase.verifyNotEmpty(Pix);
        end

        % -------------------------------------------------------------------
        % 11. Reproducibility
        % -------------------------------------------------------------------

        function testSameInputSameOutput(testCase)
            Pr1 = HealpixConeSearch.coneToPixelRanges(123.0, -20.0, 0.5, Algo.CONE);
            Pr2 = HealpixConeSearch.coneToPixelRanges(123.0, -20.0, 0.5, Algo.CONE);
            testCase.verifyEqual(Pr1.Ranges, Pr2.Ranges);
        end

        function testSqlDeterministic(testCase)
            [Sql1, ~] = HealpixConeSearch.coneSearchSql(45.0, 10.0, 1.0, 't', 'c');
            [Sql2, ~] = HealpixConeSearch.coneSearchSql(45.0, 10.0, 1.0, 't', 'c');
            testCase.verifyEqual(Sql1, Sql2);
        end
    end
end

% ---------------------------------------------------------------------------
% Helpers (mirrors Python test helpers)
% ---------------------------------------------------------------------------

function [Sql, Pf] = sqlAndPf(~)
    [Sql, Pf] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
        'proc_src', 'upix_high');
end

function D = sphereDistDeg(Ra1, Dec1, Ra2, Dec2)
    % Great-circle distance in degrees (haversine).
    R1 = deg2rad(Ra1); D1 = deg2rad(Dec1);
    R2 = deg2rad(Ra2); D2 = deg2rad(Dec2);
    Dlat = D2 - D1;
    Dlon = R2 - R1;
    A = sin(Dlat/2)^2 + cos(D1)*cos(D2)*sin(Dlon/2)^2;
    D = rad2deg(2 * asin(sqrt(A)));
end

function [RaDeg, DecDeg] = pixCenter(Nside, Pix)
    B = Backend.getBackend();
    [RaDeg, DecDeg] = B.pix2angNested(Nside, int64(Pix));
end

function Flag = rangesCoverPixel(Ranges, PixId)
    Flag = any(Ranges(:, 1) <= PixId & PixId <= Ranges(:, 2));
end

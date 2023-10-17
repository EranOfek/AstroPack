function Chi2cuts=cornerPlot4cube(Chi2, Args)
    % Cut a cube to all possible 2D cuts, and plot
    % Input  : - A cube containing e.g., chi^2.
    %          * ...,key,val,...
    %            'AxesVecs' - A cell array containing vectors of axis per
    %                   each dimension. Default is {}.
    %            'AxesCenter' - A vector of axes center indices.
    %                   If empty, use central index in each dimension.
    %                   Default is [].
    %            'AxesNames' - A cell array of axes names.
    %            'Plot' - LOgical indicating if to plot data.
    %                   Default is true.
    %            'Levels' - Contour levels in sigmas, assuming input matrix
    %                   is in \chi^2 units.
    %                   Default is [1 2 3]
    %            'DOF' - Degrees of freedom for countor levels calc.
    %                   If empty use number of dimensions.
    %                   Default is [].
    %            'FontSize' - Default is 18.
    %            'Interpreter' - Default is 'latex'
    % Output : - 
    % Author : Eran Ofek (Oct 2023)
    % Example: plot.cornerPlot4cube(rand(13,11,9,11,9))

    arguments
        Chi2
        Args.AxesVecs cell  = [];
        Args.AxesCenter     = [];
        Args.AxesNames cell = {};
        Args.Plot logical   = true;
        Args.Levels         = [1 2 3];
        Args.DOF            = [];

        Args.FontSize       = 18;
        Args.Interpreter    = 'latex';
        Args.XLim           = [0.1 0.95];
        Args.YLim           = [0.1 0.95];
        Args.Gaps           = [0.03];
        Args.XTickLabel     = 'margin';  % 'all'|'margin'|'none'
        Args.YTickLabel     = 'margin';  % 'all'|'margin'|'none'
    end

    Ndim     = ndims(Chi2);
    SizeChi2 = size(Chi2);

    if isempty(Args.AxesCenter)
        Args.AxesCenter = (SizeChi2 + 1).*0.5;
    end

    VecDim = (1:1:Ndim);
    
    for Idim1=1:1:Ndim
        for Idim2=1:1:Ndim
            if Idim2>Idim1
                Flag = ~(VecDim==Idim1 | VecDim==Idim2);
                VecDimClean = VecDim(Flag);
                PermutedChi2 = permute(Chi2, [Idim1 Idim2 VecDimClean]);
    
                DimC = num2cell(Args.AxesCenter(VecDimClean));
                Chi2cuts{Idim1, Idim2} = PermutedChi2(:,:,DimC{:});

                

            end
        end
    end

    % Plot section
    if Args.Plot
        if isempty(Args.DOF)
            Args.DOF = Ndim;
        end

        if numel(Args.Gaps)
            Args.Gaps = [Args.Gaps, Args.Gaps];
        end

        RangeXLim = range(Args.XLim)+Args.Gaps(1);
        RangeYLim = range(Args.YLim)+Args.Gaps(2);
        IdimVec   = (1:1:Ndim);
        Xstart    = Args.XLim(1) + (IdimVec-1).*RangeXLim./Ndim;
        Xend      = Args.XLim(1) + IdimVec    .*RangeXLim./Ndim - Args.Gaps(1);
        Ystart    = Args.YLim(1) + (IdimVec-1).*RangeYLim./Ndim;
        Yend      = Args.YLim(1) + IdimVec    .*RangeYLim./Ndim - Args.Gaps(2);
        DX        = Xend - Xstart;
        DY        = Yend - Ystart;

        Prob = 1-normcdf(Args.Levels,0,1,'upper').*2;
        ContourLevels = chi2inv(Prob, Args.DOF);


        HF = figure;
        for Idim1=1:1:Ndim
            for Idim2=1:1:Ndim
                if Idim2>Idim1
                    


                    %figure;
                    I1 = Idim1; % Ndim-Idim1 + 1;
                    I2 = Ndim-Idim2 + 1;
                    [I1 I2]
                    HA(Idim1,Idim2) = axes(HF, 'Position',[Xstart(I2) Ystart(I1) DX(I2) DY(I1)]);
                    switch lower(Args.XTickLabel)
                        case 'all'
                            % do nothing
                        case 'margin'
                            if I1~=1
                                HA(Idim1,Idim2).XTickLabel = [];
                            end
                        case 'none'
                            HA(Idim1,Idim2).XTickLabel = [];
                        otherwise
                            error('Unknown XTickLabel option');
                    end

                    switch lower(Args.YTickLabel)
                        case 'all'
                            % do nothing
                        case 'margin'
                            if I2~=1
                                HA(Idim1,Idim2).YTickLabel = [];
                            end
                        case 'none'
                            HA(Idim1,Idim2).YTickLabel = [];
                        otherwise
                            error('Unknown YTickLabel option');
                    end

                    if isempty(Args.AxesVecs)
                        contour(Chi2cuts{Idim1,Idim2}, ContourLevels)
                    else
                        contour(Args.AxesVecs{Idim1}, Args.AxesVecs{Idim2}, Chi2cuts{Idim1,Idim2}, ContourLevels)
                    end
                    if ~isempty(Args.AxesNames)
                        H = xlabel(Args.AxesNames{Idim2});
                        H.FontSize = Args.FontSize;
                        H.Interpreter = Args.Interpreter;
                
                        H = ylabel(Args.AxesNames{Idim1});
                        H.FontSize = Args.FontSize;
                        H.Interpreter = Args.Interpreter;
                
                    end
                end
            end
        end

    end


end

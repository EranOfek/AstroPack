function [HA,Chi2cuts]=cornerPlot4cube(Chi2, Args)
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
    % Output : - An array of axes handles, for each axes in the plot.
    %          - A 2D cell array in which each element contains a 2D
    %            matrix. Each cell element corresponds to a 2-D cut of the
    %            cube. For example, 2,3 corresponds to dim 2 vs dim 3.
    %            Generated by tools.array.arrayAllCuts
    % Author : Eran Ofek (Oct 2023)
    % Example: plot.cornerPlot4cube(rand(13,11,9,11,9))

    arguments
        Chi2
        Args.AxesVecs cell   = [];
        Args.AxesCenter      = [];
        Args.AxesNames cell  = {};
        Args.Plot logical    = true;
        Args.Levels          = [1 2 3];
        Args.DOF             = [];

        Args.AddHist logical = true;
        Args.FontSize        = 18;
        Args.Interpreter     = 'latex';
        Args.XLim            = [0.1 0.95];
        Args.YLim            = [0.1 0.95];
        Args.Gaps            = [0.03];
        Args.XTickLabel      = 'margin';  % 'all'|'margin'|'none'
        Args.YTickLabel      = 'margin';  % 'all'|'margin'|'none'
    end

    Ndim     = ndims(Chi2);
    %SizeChi2 = size(Chi2);

    Chi2cuts = tools.array.arrayAllCuts(Chi2, 'AxesCenter',Args.AxesCenter);
    
    % Plot section
    if Args.Plot
        if isempty(Args.DOF)
            Args.DOF = Ndim;
        end

        if numel(Args.Gaps)
            Args.Gaps = [Args.Gaps, Args.Gaps];
        end

        if Args.AddHist
            NdimM = Ndim;
        else
            NdimM = Ndim - 1;
        end
        RangeXLim = range(Args.XLim)+Args.Gaps(1);
        RangeYLim = range(Args.YLim)+Args.Gaps(2);
        IdimVec   = (1:1:Ndim);
        Xstart    = Args.XLim(1) + (IdimVec-1).*RangeXLim./NdimM;
        Xend      = Args.XLim(1) + IdimVec    .*RangeXLim./NdimM - Args.Gaps(1);
        Ystart    = Args.YLim(1) + (IdimVec-1).*RangeYLim./NdimM;
        Yend      = Args.YLim(1) + IdimVec    .*RangeYLim./NdimM - Args.Gaps(2);
        DX        = Xend - Xstart;
        DY        = Yend - Ystart;

        Prob = 1-normcdf(Args.Levels,0,1,'upper').*2;
        ContourLevels = chi2inv(Prob, Args.DOF);


        HF = figure;
        for Idim1=1:1:Ndim
            for Idim2=1:1:Ndim
%                 if Args.AddHist && Idim1==Idim2
%                     I1 = Idim1; % Ndim-Idim1 + 1;
%                     I2 = Ndim-Idim2 + 1;
%                     [I1 I2]
%                     [Idim1 Idim2]
%                     HA(Idim1,Idim2) = axes(HF, 'Position',[Xstart(I2) Ystart(I1) DX(I2) DY(I1)]);
%                 
%                     PP = exp(Chi2cuts{Idim1,Idim2});
%                     PP = PP./sum(PP, 'all');
%                     
%                     N=histcounts(sum(PP,1));
%                     stairs(N)
%                 end

                if Idim2>Idim1
                    
                    %figure;
                    I1 = Idim1; % Ndim-Idim1 + 1;
                    I2 = Ndim-Idim2 + 1;
                    %[I1 I2]
                    HA(Idim1,Idim2) = axes(HF, 'Position',[Xstart(I2) Ystart(I1) DX(I2) DY(I1)]);
                    

                    if isempty(Args.AxesVecs)
                        contour(Chi2cuts{Idim1,Idim2}, ContourLevels)
                    else
                        contour(Args.AxesVecs{Idim1}, Args.AxesVecs{Idim2}, Chi2cuts{Idim1,Idim2}, ContourLevels)
                    end
                    if ~isempty(Args.AxesNames)

                        if I1==1
                            H = xlabel(Args.AxesNames{Idim2});
                            H.FontSize = Args.FontSize;
                            H.Interpreter = Args.Interpreter;
                        end
                        if I2==1
                            H = ylabel(Args.AxesNames{Idim1});
                            H.FontSize = Args.FontSize;
                            H.Interpreter = Args.Interpreter;
                        end
                
                    end

                    % XTick/YTick
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

                end
            end
        end

    end


end

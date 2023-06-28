function Res=collapse_stat(Image,Args) 
% Collapse an image on one dimension and calc line statistics
% Package: @imUtil.background
% Description: Collapse an image on one dimension and calculate the line
%              statistics including smooth background level, and std
%              estimate.
%              The matrix is collapsed and the output is save in the 'Line'
%              field of the output.
%              The 'Line' is filtered (smotthed) and the result is in
%              'FiltLine', while the std of the line is in 'StdLine'.
% Input  : - An array.
%          * Pairs of ...,key,val,... arguments. Options are:
%            'Dim' - Dimension along which to collapse the image.
%                   Default is 1.
%            'CollapseFun' - Collapse function:
%                   'median' | 'mean' | 'sum' | 'std' | 'var' (ignore
%                   nans).
%                   Default is 'median'.
%            'FilterCollapse' - Method by which to filter the collapsed
%                   image: 
%                   'medfilt1' - use the medfilt1 function.
%                   'sgolay' - use the sgolay function.
%                   'hampel' - use the hampel function.
%                   'movavg' - use the movavg function.
%                   Default is 'medfilt1'.
%            'FilterCollapsePar' - A cell array of additional parameters to
%                   pass to the FilterCollapse function.
%                   Default is {10}.
%            'StdCollapse' - Method by which to estimate the std of the
%                   line.
%                   'std' - use the std function.
%                   'rstd' - use the imUtil.background.rstd function.
%                   Default is 'rstd'.
%            'StdCollapsePar' - A cell array of additional parameters to
%                   pass to the StdCollapse function.
%                   Default is {}.
% Output : - A structure with fields:
%            .Line - The collapse image.
%            .FiltLine - The filtered line.
%            .StdLine - - The line std.
%      By: Eran O. Ofek                       May 2020             
% Example: Image = rand(100,150); Image(23,:) = Image(23,:).*3;
%          Res=imUtil.background.collapse_stat(Image,'Dim',2) 
%          Res=imUtil.background.collapse_stat(Image,'Dim',2,'StdCollapse','rstd','StdCollapsePar',{})

arguments
    Image
    Args.Dim                        = 1;
    Args.CollapseFun                = 'median';
    Args.FilterCollapse             = 'medfilt1';
    Args.FilterCollapsePar cell     = {10};
    Args.StdCollapse                = 'rstd';
    Args.StdCollapsePar cell        = {}; % for std use: [],'all','omitnan'};
end


% collapse
switch lower(Args.CollapseFun)
    case 'median'
        Line = median(Image, Args.Dim, 'omitnan');
    case 'mean'
        Line = mean(Image, Args.Dim, 'omitnan');
    case 'sum'
        Line = sum(Image, Args.Dim, 'omitnan');
    case 'std'
        Line = std(Image, [], Args.Dim, 'omitnan');
    case 'var'
        Line = var(Image, [], Args.Dim, 'omitnan');
    otherwise
        error('Unknown CollapseFun option');
end
        
Line = Line(:);


switch lower(Args.FilterCollapse)
    case 'medfilt1'
        FiltLine = medfilt1(Line,Args.FilterCollapsePar{:});
    case 'sgolay'
        FiltLine = sgolay(Line,Args.FilterCollapsePar{:});
    case 'emission'
        % emission filter -remove spectral lines
        % construct a two horn filter
        error('not implemented')
        
%         X      = (-Args.FilterCollapsePar{1}:1:Args.FilterCollapsePar{1}).';
%         Filter = ones(size(X));
%         Filter(abs(X)<=Args.FilterCollapsePar{2}) = 0;
%         Filter = Filter./sum(Filter);
%         Filter = [ifftshift(Filter); zeros(numel(Line)-numel(Filter),1)];
%         % apply filter
%         FiltLine = ifft(fft(Line).*conj(fft(Filter)));
    case 'hampel'
        FiltLine = hampel(Line,Args.FilterCollapsePar{:});
    case 'movavg'
        FiltLine = movavg(Line,Args.FilterCollapsePar{:});
    otherwise
        error('Unknown FilterCollapse option');
end


switch lower(Args.StdCollapse)
    case 'std'
        StdLine = std(Line, Args.StdCollapsePar{:});
    case 'rstd'
        StdLine = imUtil.background.rstd(Line, Args.StdCollapsePar{:});
    case 'stdfilt'
        error('not implemented')
        
%         X      = (-Args.FilterCollapsePar{1}:1:Args.FilterCollapsePar{1}).';
%         Filter = ones(size(X));
%         Filter = Filter./sum(Filter);
%         
%         % apply filter
%         FiltL = ifft(fft(Line).*conj(fft(Filter)));
%         FiltL2 = ifft(fft(Line.^2).*conj(fft(Filter)));
%         
%         StdLine = sqrt(FiltL2 - FiltL.^2);
%         
    otherwise
        error('Unknown StdCollapse option');
end

Res.Line     = Line;
Res.FiltLine = FiltLine;
Res.StdLine  = StdLine;

        
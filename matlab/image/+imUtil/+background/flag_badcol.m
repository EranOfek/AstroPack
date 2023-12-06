function [Flag,Res]=flag_badcol(Image,Args) 
% Flag a bad column/row in an image
% Package: @imUtil.background
% Description: Flag a column/row which background/noise level is high.
% Input  : - An array.
%          * Pairs of ...,key,val,... arguments. Options are:
%            'Dim' - Dimension along to collapse the image.
%                   Default is 1.
%            'Threshold' - A threshold in units of sigma, for bad coumn/row
%                   flagging.
%                   Default is 10.
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
% Output : - A logical image indicating bad coumn/rows (true).
%          - The structure output of imUtil.background.collapse_stat
%      By: Eran O. Ofek                       May 2020             
% Example: Image = rand(100,150); Image(23,:) = Image(23,:).*5;
%          Flag=imUtil.background.flag_badcol(Image,'Dim',2) 

arguments
    Image
    Args.Dim                    = 1;
    Args.Threshold              = 10;
    Args.CollapseFun            = 'median';
    Args.FilterCollapse         = 'medfilt1';
    Args.FilterCollapsePar      = {10};
    Args.StdCollapse            = 'rstd';
    Args.StdCollapsePar         = {};
end


Res = imUtil.background.collapse_stat(Image,'Dim',Args.Dim,...
                                            'CollapseFun',Args.CollapseFun,...
                                            'FilterCollapse',Args.FilterCollapse,...
                                            'FilterCollapsePar',Args.FilterCollapsePar,...
                                            'StdCollapse',Args.StdCollapse,...
                                            'StdCollapsePar',Args.StdCollapsePar);

Stat = (Res.Line - Res.FiltLine)./Res.StdLine;
Flag = Stat>Args.Threshold;

if Args.Dim==1
    N       = size(Image,1);
    Flag    = repmat(Flag',N,1);
elseif Args.Dim==2
    N       = size(Image,2);
    Flag    = repmat(Flag,1,N);
else
    error('Dim must be 1 or 2');
end



function Result=collapse(Image, Args)
    %
    % Example: R=randn(100,100); R(30,:)=R(30,:)+1;
    %          Res=specUtil.trace.collapse(R,'Fun','mean');
    
    arguments
        Image
        Args.Threshold   = 5;
        Args.PreConv     = [];  % filter to convolve with prior to collapse
        Args.PreConvArgs = {};
        Args.PostFilter  = 2;  % sigma-width
        Args.DimWave     = 2;  % i.e., collapse-dim
        Args.Fun         = 'median';
        Args.Quant       = 0.95;
    end
    
    if ~isempty(Args.PreConv)
        if isa(Args.PreConv, 'function_handle')
            Conv = Args.PreConv(Args.PreConvArgs{:});
        else
            Conv = Args.PreConv;
        end
        Image = imUtil.filter.conv2_fast(Image, Conv);
    end
    
    % collapse image in the wave-dir
    switch Args.Fun
        case 'mean'
            Result = mean(Image, Args.DimWave, 'omitnan');
        case 'median'
            Result = median(Image, Args.DimWave, 'omitnan');
        case 'std'
            Result = std(Image, [], Args.DimWave, 'omitnan');
        case 'quantile'
            Result = quantile(Image, Args.Quant, Args.DimWave);
        case 'max'
            Result = max(Image,[],Args.DimWave);
        otherwise
            error('Unknown Fun option');
    end
           
    % post filter
    Std = tools.math.stat.rstd(Result);
    
    if ~isempty(Args.PostFilter)
        FiltSize = ceil(Args.PostFilter.*4);
        Filter   = normpdf((-FiltSize:1:FiltSize),0,Args.PostFilter);
        Result   = conv(Result, Filter', 'same');
        Norm     = sqrt(sum(Filter.^2));
    else
        Norm     = 1;
        
    end
    
    Std = Std.*Norm;
    %Std = tools.math.stat.rstd(Result);
    
    SN = Result./Std;
    
   Flag= logical((SN > Args.Threshold).*islocalmax(Result));
   SN(Flag)
   
    
    'a'
    
    
    
    
    
end

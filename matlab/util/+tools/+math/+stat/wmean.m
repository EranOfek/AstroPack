function [WMean,WErr,WStd]=wmean(Vec,Err,Dim,IgnoreNaN)
    % Calculate the weighted mean of a sample.
    % Input  : - Either a two column matrix [Val, Err] or a matrix of values,
    %            while the errors are in the second argument.
    %          - Optional matrix of errors. If given, then the first input
    %            argument is treated as values.
    %            (Note - Err most be nonzero, otherwise 0/0 and Inf/Inf=NaN
    %             result)
    %          - If the first two input arguments are provided then this is the
    %            dimension along to calculate the weighted mean.
    %            Default is 1.
    %          - Ignore rows or columns in which at least one element is a NaN,
    %            and don't produce output for them. Default is true. (this is
    %            different than using nansum, nanmean)
    % Output : - Weighted mean.
    %          - Weighted error on weighted mean.
    %          - Weighted standard deviation.
    % Tested : Matlab 7.0
    %     By : Eran O. Ofek                    Jun 1998
    %          Enrico Segre                    Sep 2023
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: [M,E,S]=tools.math.stat.wmean([1;2;3;4],[1;1;1;50]);
    % Reliable: 2
    %--------------------------------------------------------------------------
    
    arguments
        Vec
        Err                 = [];
        Dim                 = 1;
        IgnoreNaN logical   = true;
    end
    
    Algo = 1;
    if Algo==1
        W = 1 ./ (Err.^2);
        Mask = ~isnan(Vec) & ~isnan(Err) & Err > 0;
        Val(~Mask) = NaN;
        W(~Mask)   = NaN;
        
        SumW  = sum(W, Dim, 'omitmissing');
        WMean = sum(W .* Vec, Dim, 'omitmissing') ./ SumW;
        
        Diff = Vec - WMean;
        
        WStd = sqrt( sum(W .* Diff.^2, Dim, 'omitmissing') ./ SumW );
        
        WErr = sqrt( sum(W.^2 .* Diff.^2, Dim, 'omitmissing') ) ./ SumW;
    else
        
        %Old code
        ColVal  = 1;
        ColErr  = 2;
        
        
        if isempty(Err)
            % Vec is a two coumn vector
            Err = Vec(:,ColErr);
            Vec = Vec(:,ColVal);
            Dim = 1;
        else
            % Err is given in second argument
            % if (nargin==2)
            %     % automatic Dim for row or column vectors
            %     if any(size(Vec)==1)
            %         Dim = find(size(Vec)>1);
            %     end
            % end
        end
        
        % Remove rows or columns which include at least one NaN in Vec or in Err
        if IgnoreNaN
            Flag = ~(any(isnan(Vec),Dim) & any(isnan(Err),Dim));
            if Dim==2
                Vec  = Vec(Flag,:);
                Err  = Err(Flag,:);
            else
                Vec  = Vec(:,Flag);
                Err  = Err(:,Flag);
            end
        end
        
        W = 1./double(Err.^2);  % weight (double to prevent Inf if small singles)
        % TODO: -could use nansum, if the aim is to ignore single NaN entries
        %        without obliterating rows;
        %       -if some Err are 0, could use alternative code which computes sums
        %        using only the Vec elements for which Err=0. Maybe, equivalently,
        %        setting W=1 where Err=0 and W=0 where Err>0. By row or by column
        WS = sum(W,Dim);
        WM = sum(Vec.*W,Dim);
        WMean = WM./WS;
        WErr = sqrt(1./WS);
        WStd = sqrt( (sum(W.*Vec.^2,Dim).*WS - WM.^2) ./ (WS.^2 - sum(W.^2,Dim)) );
    end
end

function [Result] = matchedFilter(Obj, Args)
    % Matched filtering with template bank for equally spaced time series.
    % Input  : - A Matched sources object.
    %          * ...,key,val,...
    %            'FieldName' - Field name on which to run the
    %                   matched filter. Default is 'MAG'.
    %            'Templates' - Template bank (column-wise), with
    %                   the same length (epochs) as the matced
    %                   sources matrix.
    %            'rmsMagArgs' - Additional arguments to pass to
    %                   MatchedSources/rmsMag. Default is {}.
    %            'Back' - Back value to subtract from the data.
    %                   Alternatively, a function_handle for calculating
    %                   the background.
    %                   If empty, then do not subtract background.
    %                   Default is [].
    %            'BackArgs' - A cell array of additional arguments to pass
    %                   to the background calc. function. Default is {}.
    %            'Std' - An std of the data scalar, vector, or matrix.
    %                   Alternatively, this can be a function handle that
    %                   will be used to calculate the std.
    %                   The second argument of the function must be dim.
    %                   If empty, then will ise rmsMag to estimate
    %                   the std for each star according to its
    %                   magnitude.
    %                   Default is @tools.math.stat.rstd
    %            'StdArgs' - A cell array of additional arguments to pass
    %                   to the background std calc. function after the data
    %                   and dim parameters. Default is {}.
    %            'IsTemplateFFT' - Is template is FFTed and complex
    %                   conjugate of template. Default is false.
    % Output : A structure array, in which the number of elements
    %          equal to the number of elemenst in the MatchedSources object.
    %          each element contains a structure array .Temp that
    %          contains, the results foe each template (number of
    %          elements equal to the number of elements).
    %          The following fields are available:
    %          .S - Matrix of the detection statistics.
    %               The size of the matrix is like the size of the
    %               data. Columns corresponds to sources, and lines
    %               to epochs. The statistics is given in units of
    %               the std.
    %          .DataStd - The Std per source (column) that was used
    %               for the normaliztion.
    %          .MaxS - The maximum over each column in S.
    %          .IndMaxS - The index of the maximum over each column
    %               in S.
    %          .MinS - The minimum over each column in S.
    %          .IndMinS - The index of the minimum over each column
    %               in S.
    % Author : Eran Ofek (Feb 2022)
    % Example: MS = MatchedSources;
    %          MS.addMatrix(randn(100,200).*10,'MAG')
    %          T = [0 1 2 3 2 1 0].'.*100;
    %          MS.Data.MAG(1:numel(T),2) = T;
    %          [Result] = lcUtil.matchedFilter(MS, 'Templates', [T, T, T])


    arguments
        Obj
        Args.FieldName              = 'MAG';
        Args.Templates              = [];

        Args.Back                   = [];
        Args.BackArgs cell          = {};
        Args.Std                    = @tools.math.stat.rstd; % additional argument must be Dim
        Args.StdArgs cell           = {};
        Args.PadVal                 = 0;
        Args.IsTemplateFFT logical  = false;

        Args.rmsMagArgs             = {};
    end

    Ntemp = size(Args.Templates, 2);

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        [FieldName] = getFieldNameDic(Obj(Iobj), Args.FieldName);
        Matrix = getMatrix(Obj(Iobj), FieldName);
        [Nep, NsrcSel] = size(Matrix);

        if isempty(Args.Std)
            [ResRMS] = rmsMag(Obj, 'MagField',Args.FieldName,...
                                   'ParField',Args.FieldName,...
                                   Args.rmsMagArgs{:});

            % use ResRMS to estimate std
            Std = ResRMS.EstimatedStdPar;
        else
            Std = Args.Std;
        end

        for Itemp=1:1:Ntemp                    
            [Result(Iobj).Temp(Itemp).S, Result(Iobj).Temp(Itemp).DataStd] = tools.math.filter.filter1(Matrix, Args.Templates(:,Itemp), 1,...
                                                                                'Back',Args.Back,...
                                                                                'BackArgs',Args.BackArgs,...
                                                                                'Std',Std,...
                                                                                'StdArgs',Args.StdArgs,...
                                                                                'IsTemplateFFT',Args.IsTemplateFFT);

            [Result(Iobj).Temp(Itemp).MaxS, Result(Iobj).Temp(Itemp).IndMaxS] = max(Result(Iobj).Temp(Itemp).S);
            [Result(Iobj).Temp(Itemp).MinS, Result(Iobj).Temp(Itemp).IndMinS] = min(Result(Iobj).Temp(Itemp).S);
        end
    end
end
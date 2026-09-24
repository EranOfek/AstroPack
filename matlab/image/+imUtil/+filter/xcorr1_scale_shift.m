function [Info] = xcorr1_scale_shift(Data, Template, Args)
    % Find best shift and scale between two time series using cross-correlation
    %       The function first resample the data and template unfirmally.
    %       Next a new version of the template with possible stretches is
    %       generated. All the template-stretches are cross correlated with
    %       the data to find the best shift and stretch.
    % Input  : - An equally spaced data time series [T, X]
    %          - An equally spaced template time series [T, X]
    %          * ...,key,val,... 
    %            'ScaleVec' - Vector of scales to test:
    %                   Default is logspace(-0.3,0.3,1000)
    %            'uniformResampleArgs' - A cell array of arguments to pass
    %                   to uniformResample. Default is {}.
    %            See code for additional arguments.
    % Output : - A structure containing the following fields:
    %            .BestCol - The column index out of all scaled templates
    %                   with the highest correlation.
    %            .Shift - Best shift index.
    %            .Corr - Best correlation.
    %            .Best2dr - 2nd derivative of correlation at max point.
    %            .BestScale - Best scale used (corresponding to BestCol)
    %            .ScaledTemplate - The template time series scaled and
    %                   interpolated to the Data time series.
    % Author : Eran Ofek (2023 Dec) 
    % Example: Scale    = 1.1;
    %          Shift    = 10;
    %          Template = AS(1).Flux;
    %          Data     = Template;
    %          Data     = [(1:Scale:numel(Data).*Scale).', Data];
    %          Template = [(1:1:numel(Template)+Shift).', [zeros(Shift,1); Template]];
    %          Info     = imUtil.filter.xcorr1_scale_shift(Data,Template);


    arguments
        Data
        Template
        
        Args.ScaleVec          = logspace(-0.3,0.3,1000);
        
        Args.uniformResampleArgs cell   = {};
        
        Args.MaxRange          = 1e-8;
        Args.InterpMethod      = 'linear';
        Args.Back              = 'median';
        Args.subtract_back1dArgs cell = {};

        Args.ColX              = 1;
        Args.ColY              = 2;
        
    end
    
    %error('still some bugs');
    

    % make ScaleVec a row vector
    Args.ScaleVec = Args.ScaleVec(:).';
    
    % check if Data series is equally space
    [IntVal,NewT]=timeSeries.interp.uniformResample(Data(:,Args.ColX), Data(:,Args.ColY), Args.uniformResampleArgs{:});
    Data = [NewT, IntVal];
    
    [IntVal,NewT]=timeSeries.interp.uniformResample(Template(:,Args.ColX), Template(:,Args.ColY), Args.uniformResampleArgs{:});
    Template = [NewT, IntVal];

    % build a version of the template with all possible stretches
    Ndata = size(Data,1);
    Ntemp = size(Template,1);
    DataI = (1:1:Ndata).';
    TempI = (1:1:Ntemp).';

    StTemplateX = bsxfun(@times,TempI,Args.ScaleVec);
    StTemplateY = interp1(TempI,Template(:,Args.ColY),StTemplateX,Args.InterpMethod,'extrap');

    [~,~,Info]         = imUtil.filter.xcorr1_fft_multi(StTemplateY, Data(:,Args.ColY), 'Back',Args.Back, 'subtract_back1dArgs',Args.subtract_back1dArgs);

    Info.BestScale     = Args.ScaleVec(Info.BestCol);
    Info.ScaleData     = mean(diff(Data(:,Args.ColX)));
    Info.ScaleTemplate = mean(diff(Template(:,Args.ColX)));

    %Info.DataTemp = (DataI + Info.BestShift).*Info.BestScale.*Info.ScaleTemplate+min(Template(:,Args.ColX));

    XX = (Template(:,1)./Info.BestScale) - Info.BestShift;
    
    YY = interp1(XX, Template(:,2), Data(:,1));
    Info.ScaledTemplate = [Data(:,1), YY];

    % plot(Info.DataTemp,Data(:,2));
    % hold on;
    % plot(Template(:,1),Template(:,2),'r-');


end

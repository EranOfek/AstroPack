function [Info] = xcorr1_scale_shift(Data, Template, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2023 Dec) 
    % Example: S=AstroSpec.specGalQSO('Gal_E');
    %          Template = [S.Wave, S.Flux];
    % 
%          Data = interp1(Template(:,1),Template(:,2),(3300:0.7:5400).');
%          Data = [(1:1:length(Data)).'.*0.75+10,Data];
%          Info=xcorr_scale_shift(Data,Template)
%          X1=(1:1.3:100).'; Y1=zeros(size(X1)); Y1(10)=1; Y1(20)=1;
%          X2=(1:0.9:100).'; Y2=zeros(size(X2)); Y2(12)=1; Y2(30)=1;
%          Data = [X1,Y1]; Template = [X2, Y2];
%          Info=xcorr_scale_shift(Data,Template)

    arguments
        Data
        Template
        
        Args.uniformResample cell   = {};
        
        Args.MaxRange          = 1e-8;
        Args.InterpMethod      = 'linear';
        Args.ScaleVec          = logspace(-1,1,1000);
        Args.Back              = 'median';
        Args.BackPar           = {};

        Args.ColX              = 1;
        Args.ColY              = 2;
        
    end

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

    [~,~,Info]         = xcorr_fft_multi(StTemplateY,Data(:,Args.ColY),Args.Back,Args.BackPar);

    Info.BestScale     = Args.ScaleVec(Info.BestCol);
    Info.ScaleData     = mean(diff(Data(:,Args.ColX)));
    Info.ScaleTemplate = mean(diff(Template(:,Args.ColX)));

    Info.DataTemp = (DataI + Info.BestShift).*Info.BestScale.*Info.ScaleTemplate+min(Template(:,Args.ColX));


    % plot(Info.DataTemp,Data(:,2));
    % hold on;
    % plot(Template(:,1),Template(:,2),'r-');


end

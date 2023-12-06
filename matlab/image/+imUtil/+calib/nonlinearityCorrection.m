function [Image,CorrFactor] = nonlinearityCorrection(Image, Correction, Args)
    % Apply a flux non-linearity correction to a matrix image.
    % Input  : - An image in matrix format.
    %          - A correction table [Flux, CorrectionFactor]
    %            or a structure with .Flux and .Corr fields.
    %            The correction factor is either multiplicative or by
    %            division (see 'Operator' argument).
    %          * ...,key,val,...
    %            'InterpMethod' - Default is 'linear'.
    %            'Operator' - function handle operator to apply on the correction factor.
    %                   Default is @rdivide .e., divide image by factor).
    % Output : - A corrected image.
    % Author : Eran Ofek (Jul 2022)
    % Example: Corr = [0 1; 1000 1.1;20000 0.9; 70000 0.8]; 
    %          Image1 = imUtil.calib.nonlinearityCorrection(rand(1000,1000).*50000, Corr);
   
    arguments
        Image
        Correction   % [ADU, Corr]
        Args.InterpMethod    = 'linear';
        Args.Operator        = @rdivide;
    end
    
    
    if isstruct(Correction)
        CorrTable = [Correction.Flux, Correction.Corr];
    else
        CorrTable = Correction;
    end
    
    CorrFactor = interp1(CorrTable(:,1), CorrTable(:,2), Image, Args.InterpMethod);
    
    Image = Args.Operator(Image, CorrFactor);
        
end

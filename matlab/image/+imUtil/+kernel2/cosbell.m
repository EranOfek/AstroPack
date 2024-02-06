function Result=cosbell(Pars, SizeXY, PosXY, IsNormPeak)
    % create a matrix or a cube of 2D cosine bell functions
    % Package: +imUtil.kernel2
    % Input  : - [Inner radius, Outer radius] of the cosine bell function.
    %            Default is [4 7]
    %          - Stamp size [X,Y]. Default is [15 15].
    %          - [X,Y] Position of the Gaussian center in the stamp.
    %            Default is the ceil(stamp_size/2).
    %          - A logical indicating if to normlize peak to 1 (true),
    %            or bu=y sum to 1 (false).
    %            Default is true.
    % Output : - A matrix or a cube with the 2D cosine-bell.
    %            If a cube, the third dimension corresponds to the template
    %            index.
    % Author : Eran Ofek (Jun 2023)
    % Example: K=imUtil.kernel2.cosbell
    %          K=imUtil.kernel2.cosbell([4 7; 3 8]); 

    arguments
        Pars     = [4 7]
        SizeXY   = [15 15];
        PosXY    = [];
        IsNormPeak logical = false;
    end

    if isempty(PosXY)
        PosXY = ceil(SizeXY.*0.5);
    end
    
    Npars = size(Pars,1);
    
    %[MatX,MatY] = meshgrid( (1:1:SizeXY(1))-PosXY(1), (1:1:SizeXY(2))-PosXY(2) );
    %MatR        = sqrt(MatX.^2 + MatY.^2);
    VecX = (1:1:SizeXY(1))-PosXY(1);
    VecY = (1:1:SizeXY(2)).'-PosXY(2);
    %MatR = sqrt(VecX.^2 + VecY.^2);
    % faster
    MatR = hypot(VecX, VecY);
    
    Result      = ones(SizeXY(2), SizeXY(1), Npars);
    for Ipars=1:1:Npars
        Val    = ones(SizeXY(2), SizeXY(1));
        
        %Val(MatR>Pars(Ipars,2))  = 0;
        % faster
        Val = Val.*(MatR<Pars(Ipars,2));
        SpanQ = Pars(Ipars,2) - Pars(Ipars,1);
        Ind = find(MatR>Pars(Ipars,1) & MatR<=Pars(Ipars,2));
        Val(Ind) = 0.5.*(1+cos( pi.*(MatR(Ind) - Pars(Ipars,1))./SpanQ ));
        Result (:,:,Ipars) = Val;
    end
    
    if ~IsNormPeak
        Sum = sum(Result,[1 2]);
        Result = Result./Sum;
    end
    
end
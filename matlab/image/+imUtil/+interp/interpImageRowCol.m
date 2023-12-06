function Result = interpImageRowCol(Image)
    % Interpolate an image over NaN values using the col/row method.
    %   In the col/row method we use linear interpolation over rows and
    %   columns seperatly, and average the result.
    %   This function is about x3 times faster than inpaint_nans.
    %   FEATURES: In some cases some remaining NaNs. Not recomended.
    % Input  : - A 2D matrix.
    % Output : - The 2D matrix interpolated over NaNs.
    % Author : Eran Ofek (Nov 2021)
    % Example: [MatX, MatY] = meshgrid((1:1000),(1:1000));
    %          Image = MatX.*MatY + randn(1000,1000).*0.01;
    %          Image(10,10) = NaN; Image(20,11) = NaN; Image(50,60) = NaN;
    %          Image(1,30) = NaN; Image(21,11) = NaN;
    %          Result = imUtil.interp.interpImageRowCol(Image);
    
    
    
    Args.InterpMethod    = 'linear';
    
    SizeIm = size(Image);
    VecX   = (1:1:SizeIm(2));
    VecY   = (1:1:SizeIm(1)).';
    
    %IsN = isnan(Image);
    %[XN, YN] = imUtil.image.ind2sub_fast(SizeIm, IsN);
    
    ResultX = Image;
    ResultY = Image;
    for Icol=1:1:SizeIm(2)
        IsN = isnan(Image(:,Icol));
        if any(IsN)
            if all(IsN)
                if Icol>1
                    ResultX(IsN,Icol) = mean(Image(:,Icol-1));
                else
                    ResultX(IsN,Icol) = mean(Image(:,Icol+1),1,'omitnan');
                end
            else
                ResultX(IsN,Icol) = interp1(VecY(~IsN), Image(~IsN,Icol), VecY(IsN), Args.InterpMethod,'extrap'); 
            end
        end
    end
    for Irow=1:1:SizeIm(1)
        IsN = isnan(Image(Irow,:));
        if any(IsN)
            if all(IsN)
                if Irow>1
                    ResultX(Irow,IsN) = mean(Image(Irow-1,:));
                else
                    ResultX(Irow,IsN) = mean(Image(Irow+1,:),1,'omitnan');
                end
            else
                ResultY(Irow,IsN) = interp1(VecX(~IsN), Image(Irow,~IsN),VecY(IsN), Args.InterpMethod,'extrap');
            end
        end
    end
    Result = 0.5.*(ResultX + ResultY);
    
end

function [Image, Scale]=crop(Image, CCDSEC, Args)
    % Display image and crop
    % Input  : - An image matrix.
    %          - Optional CCDSEC to crop [Xmin Xmax Ymin Ymax]
    %            If not given, or empty, then the use can click on two
    %            points that defins the rectangular crop region.
    % Output : - Cropped image.
    % Author : Eran Ofek (Apr 2023)
   
    arguments
        Image
        CCDSEC = [];
        Args.GetX = false;
        Args.GetY = false;
    end
     
    if isempty(CCDSEC)
        imshow(Image);
        
        if Args.GetX
            fprintf('Click on first X point for scale\n')
            [Xs(1)] = ginput(1);
            Xact(1) = input('Write X value of first X point\n');
            
            fprintf('Click on second X point for scale\n')
            [Xs(2)] = ginput(1);
            Xact(2) = input('Write X value of second X point\n');
        else
            Xact = [NaN NaN];
            Xs   = [NaN NaN];
        end
        
        if Args.GetY
            fprintf('Click on first Y point for scale\n')
            [~,Ys(1)] = ginput(1);
            Yact(1) = input('Write Y value of first Y point\n');
            
            fprintf('Click on second Y point for scale\n')
            [~,Ys(2)] = ginput(1);
            Yact(2) = input('Write Y value of second X point\n');
        else
            Yact = [NaN NaN];
            Ys   = [NaN NaN];
        end
        
        fprintf('Click on 2 points to crop');
        [X,Y]=ginput(2);
        
        X = ceil(X);
        Y = ceil(Y);
        
        CCDSEC = [min(X), max(X), min(Y), max(Y)];
        
        Ys = Ys - min(Y);
        Xs = Xs - min(X);
        
        Scale.Xs   = Xs;
        Scale.Ys   = Ys;
        Scale.Xact = Xact;
        Scale.Yact = Yact;
    else
        Scale = [];
    
    end
    
    Image = Image(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));
    
    
end
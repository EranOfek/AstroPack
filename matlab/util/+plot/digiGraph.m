function [DX,DY]=digiGraph(Image)
    %
    % Example: plot.digiGraph('col.png');

    if ischar(Image)
        Image = imread(Image);
    end
    imagesc(Image);

    Npa = 4;
    X  = nan(Npa,1);
    Y  = nan(Npa,1);
    MX = nan(Npa,1);
    MY = nan(Npa,1);
    for I=1:1:Npa
        fprintf('Select point along the X/Y axes:\n')
        [MX(I),MY(I)]=ginput(1);
        X(I) = str2double(input('Type X value: ','s'));
        Y(I) = str2double(input('Type Y value: ','s'));
    end

    % fit transformation
    % MX = A + B*X + C*Y
    % MY = D + E*X + F*Y

    HX = [ones(Npa,1), MX, MY];
    HY = [ones(Npa,1), MX, MY];

    ParX = HX\X;
    ParY = HY\Y;


    fprintf('Use left click to select data points, right click to terminate selection\n')
    Button = 1;
    I = 0;
    while Button==1
        I = I + 1;
        [MDX(I), MDY(I), Button] = ginput(1);
    end
    Np = numel(MDX);

    DX = ParX(1) + ParX(2).*MDX(:) + ParX(3).*MDY(:);
    DX = ParY(1) + ParY(2).*MDX(:) + ParY(3).*MDY(:);



end

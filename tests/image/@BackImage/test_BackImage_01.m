function Result = unitTest()
    % unitTest for BackImage class

    B = BackImage;
    B = BackImage(B);
    B = BackImage([2 2]);

    Result = true;
end

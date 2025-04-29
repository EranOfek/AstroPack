function [AD] = properSubtraction(New, Ref, Args)
    % Perform ZOGY proper subtraction
    %   Wrapper for all the image subtraction commands in AstroZOGY.
    %   Subtract pairs of New and Ref image (one to many,
    %   many to one, or many to many). Register, subtract, and calculate
    %   statistics including Scorr and Translient.
    % Input  : - AstroImage vector of New images.
    %            Length must be 1, or equal to that of the Ref vector.
    %          - AstroImage vector of Ref images.
    %            Length must be 1, or equal to that of the New vector.
    %          * ...,key,val,... 
    %            'ReBack' - A logical indicating if to re-estimate the
    %                   background and variance. Default is true.
    %            'RefIsBackSub' - True if reference image is background
    %                   subtracted. Default is true.
    %            'Register' - Register images. Default is true.
    %            'GenScorr' - Generate Scorr. Default is true.
    %            'GenTranslient' - Generate Translient Z^2 image.
    %            'RenormS_ExcludeBits' - Renormalize S by removing pixels
    %                   with these bit mask value. Default is 'NearEdge'.
    %            'RenormS_StdFun' - Function to use for the std
    %                   renomalization of S.
    %                   Default is @tools.math.stat.rstd
    % Output : - An AstroZOGY object with all the subtraction images. 
    % Author : Eran Ofek (2025 Apr) 
    % Example: AD=imProc.sub.properSubtraction(New,Ref);
    % Author : Eran Ofek (Apr 2025)


    arguments
        New
        Ref
        Args.ReBack              = true;
        Args.RefIsBackSub        = true;
        Args.Register            = true;
        Args.GenScorr            = true;
        Args.GenTranslient       = true;
        Args.RenormS_ExcludeBits = 'NearEdge';
        Args.RenormS_StdFun      = @tools.math.stat.rstd;
    end

    Nnew = numel(New);
    Nref = numel(Ref);

    if ~(Nnew==1 || Nref==1 || Nnew==Nref)
        error('Number of New and REf images must be either 1, or equal to each other');
    end

    N = max(Nnew, Nref);
    for I=1:1:N
        Inew = min(Nnew, I);
        Iref = min(Nref, I);
        
        % subtract image
        AD(I) = AstroZOGY;
        AD(I).Ref = Ref(Iref);
        AD(I).New = New(Inew);

        if Args.ReBack
            AD(I).Ref.Back = [];
            AD(I).Ref.Var  = [];
        end
        AD(I).RefIsBackgroundSubtracted = Args.RefIsBackSub;

        if Args.Register
            AD(I).register;
        end

        AD(I).subtractionD;
        AD(I).subtractionS;
        if Args.GenScorr
            AD(I).subtractionScorr;
        end
        if Args.GenTranslient
            AD(I).translient;
        end

        % EO: I added this in order to deal with the edges of the reference
        % images that are based on low number of coadded images and they are
        % biasing the statistics
        % In principle this should be done by the AstroZOGY class...
        if ~isempty(Args.RenormS_ExcludeBits)
            FlagNearEdge = AD.MaskData.findBit(Args.RenormS_ExcludeBits);
            AD(I).S = AD(I).S./Args.RenormS_StdFun(AD(I).S(~FlagNearEdge(:)));
        end

    end

end

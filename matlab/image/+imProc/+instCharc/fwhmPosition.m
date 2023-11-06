function Result=fwhmPosition(Obj, Args)
    % Measure the FWHM as a function of position in AstroImage object
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'BlockSize' - Block size in which to calculate the FWHM.
    %                   Default is [1204 1024].
    % Output : - A structure array (element per AstroImage element) with
    %            the following fields:
    %            .ListCenters - [X, Y] of center
    %            .FWHM        - Vector of FWHM per sub image.
    %            .Nst         - Number of stars in each sub image.
    % Author : Eran Ofek (Nov 2023)
    % Example: R=imProc.instCharc.fwhmPosition(AI);

    arguments
        Obj AstroImage
        Args.BlockSize   = [1024 1024];

    end

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        [SubAI, InfoCCDSEC] = imProc.image.image2subimages(Obj(Iobj), Args.BlockSize);
        [F,N]  = imProc.psf.measureFWHM(SubAI);
        Result(Iobj).ListCenters = InfoCCDSEC.ListCenters;
        Result(Iobj).FWHM = F(:);
        Result(Iobj).Nst  = N(:);
    end
    

end

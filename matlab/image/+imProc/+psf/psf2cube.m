function Result = psf2cube(Obj, Args)
    % Construct a cube of PSF stamps from AstroImage
    % Input  : - An AstroImage object.
    %            * ...,key,val,...
    %            'StampSize' - PSF Stamp size. Empty for default. Default is empty.
    %            'DataPSF' - see AstroPSF/getPSF for details.
    %            'FunPSF' - see AstroPSF/getPSF for details.
    %            'ArgVals' - see AstroPSF/getPSF for details.
    %            'ArgNames' - see AstroPSF/getPSF for details.
    % Output : - A cube of PSFs, in which the PSF index is along the 3rd
    %            dimension.
    % Author : Eran Ofek (Jun 2021)
    % Example: NOT TESTED
    
    arguments
        Obj AstroImage
        Args.StampSize           = [];
        Args.DataPSF             = [];
        Args.FunPSF              = [];
        Args.ArgVals             = [];
        Args.ArgNames            = [];
    end
   
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        PSF = getPSF(Obj(Iobj), Args.DataPSF, Args.FunPSF, Args.StampSize, Args.ArgVals, Args.ArgNames);
        if Iobj==1
            Size   = size(PSF);
            Result = zeros(Size(1), Size(2), Nobj);
        end
        Result(:,:,Iobj) = PSF;
    end
    
    
end
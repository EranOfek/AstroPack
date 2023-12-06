function Result = coaddProperCore(Obj, Args)
    % Proper coaddition of images in AstroImage object
    % Input  : -
    % Output : -
    % Author : Eran Ofek (Jun 2021)
    % Example: UNDER CONSTRUCTION
    
    arguments
        Obj AstroImage
        Args.CCDSEC            = [];
        Args.F
        Args.Var
        Args.PsfType           = 'center';
        Args.Norm 
        
    end
   
    CubeImage = imProc.image.images2cube(Obj, 'CCDSEC',Args.CCDSEC);
    CubePSF   = imProc.psf.psf2cube(Obj);
    
    [R,PR,R_f,PR_f] = imUtil.properCoadd.combine_proper(CubeImage, CubePSF, Args)
    
end
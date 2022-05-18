function Result = isSuccessWCS(Obj)
    % Check if WCS is sucessful (true) on a list of AstroImage
    % Input  : - An AstroImage array.
    % Output : - An array of logical indicating the WCS.Success in each
    %            element of the input array.
    % Author : Eran Ofek (May 2022)
    % Example: imProc.astrometry.isSuccessWCS(AI)

    arguments
        Obj AstroImage
    end
   
    Nobj = numel(Obj);
    Result = false(size(Obj));
    for Iobj=1:1:Nobj
        Result(Iobj) = Obj(Iobj).WCS.Success;
    end
    
end
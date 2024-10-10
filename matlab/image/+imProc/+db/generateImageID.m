function [Result] = generateImageID(Obj, Args)
    % Generate Image 'unique' ID and store it in header
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Oct) 
    % Example: imProc.db.generateImageID

    arguments
        Obj
        Args.BitNum            = [5 4  5 5 3 6  36]
        Args.TypeCol           = "IMTYPE";
        Args.LevelCol          = "LEVEL";
        Args.InstCol           = ["NODENUB", "MOUNTNUM", "CAMNUM", "CROPID"];
        Args.TimeCol           = "MIDJD";
        

        Args.TimeFun           = @(jd) uint64((jd-2451545.5).*86400.*10);  % number of 0.1s since J2000
        Args.LevelOptions      = AstroFileName.ListLevel;
        Args.TypeOptions       = AstroFileName.ListType;
    end

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isempty(Args.TypeCol)
            Type = 0;
            BitType = 
        else
            Type = Obj(Iobj).getKey(Args.TypeCol)

        St = Obj(Iobj).getStructKey([Args.TypeCol, Args.LevelCol, Args.TimeCol]);
        St.(Args.TypeCol)

end

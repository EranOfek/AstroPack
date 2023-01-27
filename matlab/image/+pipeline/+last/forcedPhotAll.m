function MS=forcedPhotAll(Args)
    % Execute forcedPhot on all images found in some directory structure.
    %   Search for all images in dir struct recursively, and perform forced
    %   photometry on a stationary object or a moving object.
    % Input  : * ...,key,val,...
    %            See code
    % Output : - A Matched sources object. Element per directory.
   
    arguments
        Args.BasePath       = @pipeline.last.constructCamDir;  % if empty use pwd
        Args.DataDir        = 1;
        Args.BasePathArgs   = {'SubDir','2023'};
        Args.FileTemp       = 'LAST*_sci_proc_Image_*.fits';
        
        Args.Coo                        % [RA, Dec], for moving use EphemTable
        Args.CooUnits       = 'deg';
        Args.EphemTable     = [];  % if given then Moving=true
        
    end
    
    if isempty(Args.BasePath)
        Args.BasePath = pwd;
    end
    
    if isa(Args.BasePath, 'function_handle')
        BasePath = Args.BasePath(Args.DataDir, Args.BasePathArgs{:});
    else
        BasePath = Args.BasePath;
    end
    
    PWD = pwd;
    cd(BasePath);
    
    % select directories
    D = io.files.rdir(Args.FileTemp);
    D = D([D.isdir]);
    
    if isempty(Args.EphemTable)
        % stationary object
        Coo = Args.Coo;
    else
        Coo = [];
    end
    Moving   = false;
    CooUnits = Args.CooUnits;
    
    
    Ndir = numel(D);
    for Idir=1:1:Ndir
        cd(D(Idir).folder);
        
        % get all files
        FN = FileNames(Args.FileTemp);
        AI = AstroImage.readFileNames(FN);
        JD = AI.julday;
        
        if isempty(Coo)
            % moving source
            Moving = true;
            
            InterpTable = interp1(Args.Coo, 'JD',{'RA','Dec'}, JD);
            Coo         = InterpTable.Catalog(:,[2 3]);
            CooUnits    = InterpTable.ColUnits{2};
        end
            
        MeanCoo = mean(Coo);
        
        ResIn  = AI.isSkyCooInImage(MeanCoo(1), MeanCoo(2),'UNIQSEC',CooUnits);
        FlagIn = [ResIn.InImage];
        AI     = AI(FlagIn);
        JD     = JD(FlagIn);
        if Moving
            Coo = Coo(FlagIn,:);
        end
            
        MS(Idir) = imProc.sources.forcedPhot(AI, 'Coo',Coo, 'CooUnits',CooUnits, 'Moving',Moving);
        
    end
    
    % go back to orig dir
    cd(PWD);
end

function MS=forcedPhotAll(Args)
    % Execute forcedPhot on all images found in some directory structure.
    %   Search for all images in dir struct recursively, and perform forced
    %   photometry on a stationary object or a moving object.
    % Input  : * ...,key,val,...
    %            See code
    % Output : - A Matched sources object. Element per directory.
    % Author : Eran Ofek (Jan 2023)
    % Example:
    % MS=pipeline.last.forcedPhotAll('Coo',[50.0452635724,+8.748787337]);
   
    arguments
        Args.BasePath       = @pipeline.last.constructCamDir;  % if empty use pwd
        Args.DataDir        = 1;
        Args.BasePathArgs   = {'SubDir','2023/03/'};
        Args.FileTemp       = 'LAST*_089+48_*_sci_proc_Image_*.fits';
        
        Args.Coo                        % [RA, Dec], for moving use EphemTable
        Args.CooUnits       = 'deg';
        Args.EphemTable     = [];  % if given then Moving=true
        
        
        Args.MaxIter        = 1;
        
        Args.Verbose logical = true;
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
    UniqueDir = unique({D.folder});
    
    
    if isempty(Args.EphemTable)
        % stationary object
        Coo = Args.Coo;
    else
        Coo = [];
    end
    Moving   = false;
    CooUnits = Args.CooUnits;
    
    
    Ndir = numel(UniqueDir);
    for Idir=1:1:Ndir
        if Args.Verbose
            fprintf('Dir %d out of %d\n',Idir,Ndir);
        end
        cd(UniqueDir{Idir});
        
        % get all files
        FN = FileNames(Args.FileTemp);
        AI = AstroImage.readFileNamesObj(FN,'Path',sprintf('.%s',filesep));
        JD = AI.julday;
        
        if ~isempty(Args.EphemTable)
            % moving source
            Moving = true;
            
            InterpTable = interp1(Args.EphemTable, 'JD',{'RA','Dec'}, JD(:));
            Coo         = InterpTable.Catalog(:,[2 3]);
            if isempty(InterpTable.ColUnits)
               CooUnits = Args.CooUnits;
            else
               CooUnits    = InterpTable.ColUnits{2};
            end            
        end
            
        MeanCoo = mean(Coo,1);
        
        ResIn  = AI.isSkyCooInImage(MeanCoo(1), MeanCoo(2),'UNIQSEC',CooUnits);
        FlagIn = [ResIn.InImage];
        
        AI     = AI(FlagIn);
        JD     = JD(FlagIn);
        if Moving
            Coo = Coo(FlagIn,:);
        end
        
        if ~isempty(AI)
            MS(Idir) = imProc.sources.forcedPhot(AI, 'Coo',Coo, 'CooUnits',CooUnits, 'Moving',Moving,'MaxIter',Args.MaxIter);
            MS(Idir).UserData.FlagIn = FlagIn;
            MS(Idir).UserData.FN     = FN;
        
        end
        cd(BasePath);
    end
    
    % go back to orig dir
    cd(PWD);
end

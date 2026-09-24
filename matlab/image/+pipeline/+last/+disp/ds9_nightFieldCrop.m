function [D9,Files] = ds9_nightFieldCrop(Mount, Cam, Date, Field, Crop, Args)
    % Find LAST images by date, field, crop, in local disk and display and load into DS9analysis.
    %   Optionally load also the MatchedSources object.
    % Input  : - Mount number
    %          - Camera number
    %          - Date [Day, Month, Year]
    %          - FieldID
    %          - CropID
    %          * ...,key,val,... 
    %            'Visit' - If empty, do not add visit to path.
    %                   Default is [].
    %            'BasePath' - Base path. Default is '/lastdata'.
    %            'Node' - Node number. Default is 1.
    %            'ProjName' - Project name. Default is 'LAST'.
    %            'Type' - Image type. Default is 'sci'.
    %            'Level' - Image level. Default is 'coadd'.
    %            'Product' - Image product. Default is 'Image'.
    %            'Display' - Default is true.
    % Output : - DS9anlaysis object with the images loaded.
    %          - File names.
    % Author : Eran Ofek (2025 Sep) 
    % Example: D9=pipeline.last.disp.ds9_nightFieldCrop(2,3,[1 1 2025], 1101, 10)

    arguments
        Mount
        Cam
        Date
        Field
        Crop
        Args.Visit      = [];
        Args.BasePath   = '/lastdata';
        Args.Node       = 1;
        Args.ProjName   = 'LAST';
        Args.Type       = 'sci';
        Args.Level      = 'coadd';
        Args.Product    = 'Image';
        
        Args.Display    = true;

        Args.LoadMergedMat = true;
    end

    Path=pipeline.last.path.pathProc(Mount, Cam, Date,...
        'BasePath',Args.BasePath,...
        'Node',Args.Node,...
        'ProjName',Args.ProjName);

    PWD = pwd;
    cd(Path);

    if isnumeric(Field)
        Field = string(Field);
    end
    TempName = sprintf('%s*_%s*_%03d_%s_%s_%s*.fits', Args.ProjName, Field, Crop, Args.Type, Args.Level, Args.Product);

    [~,Files]=io.files.findFiles(TempName);

    % Cmd = sprintf('find . -type f -name %s',TempName);
    % [~,Files]=system(Cmd);
    % 
    % % convert to string array
    % Files = splitlines(Files);
    % % remove blank lines
    % Files = Files(strlength(Files) > 0);

    if isempty(Files)
        fprintf('No Images found\n');
    else

        
        if Args.Display
            if isempty(Files)
                fprintf('Files not found\n');
                D9 = [];
            else
                fprintf('Found %d files\n', numel(Files));
                
                AI = AstroImage.readProducts({Files{:}});
                % sort by JD
                JD = AI.julday;
                [~,SI] = sort(JD);
                AI = AI(SI);
    
                D9 = DS9analysis;
                D9.load(AI);
            end
        else
            D9 = [];
        end

        if Args.LoadMergedMat
                  
            TempNameM = sprintf('%s*_%s*_%03d_%s_%s_%s*.hdf5', Args.ProjName, Field, Crop, Args.Type, 'merged', 'MergedMat');

            [~,FilesM]=io.files.findFiles(TempNameM);

            D9.MatchedSources = MatchedSources.read({FilesM{:}});
            D9.MatchedSources = D9.MatchedSources(SI);
            
        end



    end

    cd(PWD);

end

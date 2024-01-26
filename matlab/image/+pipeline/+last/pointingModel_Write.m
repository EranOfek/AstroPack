function [Result] = pointingModel_Write(R1, Args)
    % Write the results of pointingModel_Solve into a configuration file.
    %   This function can be used to generate a pointing model for LAST.
    % Input  : - The structure output of: pipeline.last.pointingModel_Solve
    %            Structure array with number of elements equal to the
    %            number of cameras. Each element contains a field named
    %            Result that contains a table of coordintaes measured in
    %            all the images.
    %          * ...,key,val,... 
    %            See code
    % Output : - 
    % Author : Eran Ofek (2024 Jan) 
    % Example: Res=pipeline.last.pointingModel_Write(R1);

    arguments
        R1                    
        Args.Obs                   = celestial.earth.observatoryCoo('Name','LAST');
        Args.IndRef                = 1;   % camera to use as a reference
        Args.Plot logical          = true;  % Plot distortions as a fun of HA/Dec
        Args.AddZeroAtPole logical = true; % add zero distortion near North celestial pole.
        Args.ConfigFile            = '/home/ocs/pointingModel.txt';   % name of config file to write
    end

    RAD = 180./pi;

    % prepare table of 4 camera combined data
    JD    = R1(Args.IndRef).Result.JD;
    M_RA  = R1(Args.IndRef).Result.M_RA;
    M_HA  = R1(Args.IndRef).Result.M_HA;
    M_DEC = R1(Args.IndRef).Result.M_DEC;
    
    M_JRA  = R1(Args.IndRef).Result.M_JRA;
    M_JHA  = celestial.convert.convert_ha(M_JRA, JD, 'InUnits','deg', 'OutUnits','deg', 'Long',Args.Obs.Lon);
    M_JDEC = R1(Args.IndRef).Result.M_JDEC;
    
    Npt = numel(JD);

    Ncam = numel(R1);

    % Order all the measurmnets from the different camera to much by
    % coordinates:
    for Icam=1:1:Ncam
        Cam(Icam).AstRA  = nan(Npt,1);
        Cam(Icam).AstHA  = nan(Npt,1);
        Cam(Icam).AstDec = nan(Npt,1);
        Cam(Icam).CamRot = nan(Npt,1);

        for Iobs=1:1:Npt
            D = celestial.coo.sphere_dist_fast(M_JRA(Iobs)./RAD, M_JDEC(Iobs)./RAD, R1(Icam).Result.M_JRA./RAD, R1(Icam).Result.M_JDEC./RAD);
            [MinD, IndMinD] = min(D);
            if MinD<(0.01./RAD)
                % found
                Cam(Icam).AstRA(Iobs)  = R1(Icam).Result.CenterRA(IndMinD);
                Cam(Icam).AstHA(Iobs)  = R1(Icam).Result.CenterHA(IndMinD);
                Cam(Icam).AstDec(Iobs) = R1(Icam).Result.CenterDec(IndMinD);

                Cam(Icam).CamRot(Iobs) = R1(Icam).Result.Rotation(IndMinD);
            else
                % skip
            end
        end
    end

    % mean coordinate
    AstRA  = zeros(Npt,0);
    AstHA  = zeros(Npt,0);
    AstDec = zeros(Npt,0);
    CamRot = zeros(Npt,0);
    for Icam=1:1:Ncam
        AstRA = [AstRA, Cam(Icam).AstRA];
        AstHA = [AstHA, Cam(Icam).AstHA];
        AstDec = [AstDec, Cam(Icam).AstDec];
        CamRot = [CamRot, Cam(Icam).CamRot];
    end

    MeanAstRA  = mean(AstRA, 2);
    MeanAstHA  = mean(AstHA, 2);
    MeanAstDec = mean(AstDec, 2);

    % Distortion
    DiffHA     = M_JHA - MeanAstHA;
    DiffDec    = M_JDEC - MeanAstDec;
    
    % HA, Dec, DiffHA, DiffDec [deg]
    DistortionMatrix = [M_HA, M_DEC, DiffHA, DiffDec];


    if Args. AddZeroAtPole
        % add these values to avoid interpolation at dec 90 deg
        AtPole = [-135 90 0 0; -90 90 0 0; -45 90 0 0; 0 90 0 0; 45 90 0 0; ...
            90 90 0 0; 135 90 0 0];
        DistortionMatrix = [DistortionMatrix; AtPole];
    end
        
    Result.DistortionMatrix = DistortionMatrix;



    if Args.Plot
        figure(1);
        scatter(M_HA, M_DEC, 40, DiffHA, 'filled');
        colorbar
        title('DiffHA');

        box on;
        H = xlabel('HA [deg]');
        H = ylabel('Dec [deg]');

        figure(2);
        scatter(M_HA, M_DEC, 40, DiffDec, 'filled');
        colorbar
        title('DiffDec')
        box on;
        H = xlabel('HA [deg]');
        H = ylabel('Dec [deg]');

        
        for Icam=1:1:Ncam
            figure(2+Icam);
            scatter(M_HA, M_DEC, 40, CamRot(:,Icam), 'filled');
            title(sprintf('DiffRot - cam%d',Icam));
            colorbar
            box on;
            H = xlabel('HA [deg]');
            H = ylabel('Dec [deg]');
        end
        

    end

    
    % write DistortionMatrix
 
    if ~isempty(Args.ConfigFile)
        writePMFile(Args.ConfigFile, date, DistortionMatrix);
    end
       
end


%%% Internal functions

function writePMFile(ConfigFile, date, PM)
    % write config file
    FID = fopen(ConfigFile,'w');
    fprintf(FID,'# pointing model interpolation data\n');
    fprintf(FID,'# Generated on: %s\n',date);
    fprintf(FID,'# format:       [M_HA,  M_Dec,  offsetHA,  offsetDec]\n');
            
    fprintf(FID,'PointingData : [\n');
    Npm = size(PM,1);
    for Ipm=1:1:Npm
        fprintf(FID,'         [%11.6f, %11.6f, %11.6f, %11.6f],\n',PM(Ipm,:));
    end
    fprintf(FID,'     ]\n');
    fclose(FID);
     
end





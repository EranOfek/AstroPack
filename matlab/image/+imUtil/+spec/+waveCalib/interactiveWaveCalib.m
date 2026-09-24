function [StoredData,Result] = interactiveWaveCalib(Spec, Args)
    % Interactive wavelength calibration by line marking
    %     The user is prompted to select peaks in the spectrum and to feed
    %     their wavelength. Next, it is possible to fit these selected
    %     lines with a polynomial transformation and to use this to
    %     automatically find additional lines from a line list and to
    %     re-fit the data.
    %     Peaks in the spectrum are marked by empty red circles.
    %     Manulally selected peaks are marked by filled black circles.
    %     Deleted peaks are marked by filled red circle.
    %     Automatically identified peaks are marked by filled green circles.
    %
    % Input  : - Spectrum [PixeelPosition, Intensity], or [Intensity].
    %            If a single column then, the pixel positions are set to a
    %            running index.
    %          * ...,key,val,... 
    %            'LineList' - A vector of the wavelength of known lines in
    %                   the spectrum. Default is [].
    %            'fitWaveCalibArgs' - A cell array of additional arguments
    %                   to pass to imUtil.spec.waveCalib.fitWaveCalib
    %                   Default is [].
    %            See code for additional arguments.
    %
    % Output : - A structure array with the data per each line.
    %            The following fields are available:
    %            .PeakPos - Pixel position.
    %            .Wave - Wavelength
    %            . ...
    %          - A structure with the best fit data.
    %            See imUtil.spec.waveCalib.fitWaveCalib for details.
    % Author : Eran Ofek (2023 Dec) 
    % Example: load SpecArcs.mat
    %          R=imUtil.spec.waveCalib.interactiveWaveCalib(SpecArcs(1).Spec, 'ZoomInset',[]);

    arguments
        Spec
        Args.LineList          = [];
        Args.fitWaveCalibArgs  = {};
        
        %Args.localMaxArgs cell = {};
        %Args.StdFilterHalfSize = [30 200];
        %Args.Threshold         = 10;
        
        Args.MinPeakHeight     = 0;
        Args.MinPeakWidth      = 2;
        Args.MaxPeakWidth      = 20;
        
        Args.ZoomInset         = [30 15];
        
        Args.PeakMarker        = {'Color','r', 'MarkerSize',5};
        Args.GoodPeakMarker    = {'Color','k', 'MarkerFaceColor','k', 'MarkerSize',5};
        Args.AutoPeakMarker    = {'Color','g', 'MarkerFaceColor','g', 'MarkerSize',5};
        Args.DeletedPeakMarker = {'Color','r', 'MarkerFaceColor','r', 'MarkerSize',5};
        Args.DeleteMinWaveDist = 100;
        
        Args.MaxDistWave       = 5;
    end
    
    if isvector(Spec)
        Flux  = Spec(:);
        Nwave = numel(Spec);
        Wave  = (1:1:Nwave).';
    else
        Wave  = Spec(:,1);
        Flux  = Spec(:,2);
    end
    
    
    
    [PeakHeight, PeakLocation,PeakWidth] = findpeaks(Flux, Wave, 'MinPeakHeight',Args.MinPeakHeight, 'MinPeakWidth',Args.MinPeakWidth, 'MaxPeakWidth',Args.MaxPeakWidth);
    
    plot(Wave, Flux, 'k-');
    hold on;
    plot(PeakLocation, PeakHeight, 'o', Args.PeakMarker{:});
    Ha = gca;
    XlimYlim = [Ha.XLim, Ha.YLim];
    
    [PeakAxisX, PeakAxisY] = plot.xy2axesPos(Ha, PeakLocation, PeakHeight);
    
    if ~isempty(Args.ZoomInset)
        Z = plot.ZoomInset;
        Z.ZoomFactorX = Args.ZoomInset(1);
        Z.ZoomFactorX = Args.ZoomInset(2);
    end
    

    showMenu();
    
    Result     = [];
    StoredData = [];
    Cont = true;
    Ind  = 0;
    IsWaveMode = true;
    while Cont
        if IsWaveMode
            fprintf('h - show menu | q - quit | current mode is wavelength | use mouse to select line\n');
            %Ans = input('h - show menu | q - quit | current mode is wavelength | Enter wavelength : ','s');
        else
            fprintf('h - show menu | q - quit | current mode is line index | use mouse to select line\n');
            %Ans = input('h - show menu | q - quit | current mode is line index | Enter line index : ','s');
        end
        [XY, Key] = plot.ginputKeyboard;
        if ~isnan(Key)
            switch lower(Key)
                case 'h'
                    showMenu();
                case 'q'
                    Cont = false;
                case 'z'
                    zoom on;
                    input('  Zoom - click any key when finished zooming : ','s');
                    zoom off;
                case 'l'
                    % show line list
                    showLineList(Args.LineList);
                case 'w'
                    fprintf('Change mode to mark lines by wavelength\n');
                    IsWaveMode = true;
                case 'n'
                    fprintf('Change mode to mark lines by line index\n');
                    IsWaveMode = false;
                case 'd'
                    % delete line
                    [Min,MinInd] = min([StoredData.PeakPos].'-XY(1));
                    
                    if Min>Args.DeleteMinWaveDist
                        fprintf('Wavelength distance to nearest point is %f - point not removed',Min);
                    else
                        delete(StoredData(MinInd).PointH);
                        
                        Nsd = numel(StoredData);
                        Flag = (1:1:Nsd).'~=MinInd;
                        StoredData = StoredData(Flag);
                        
                        plot(PeakLocation(MinInd), PeakHeight(MinInd), 'o', Args.DeletedPeakMarker{:});
                    end
                case 'f'
                    % fit
                    Result = imUtil.spec.waveCalib.fitWaveCalib([StoredData.PeakPos].',[StoredData.Wave].', Args.fitWaveCalibArgs{:});
                case 'i'
                    % identify all lines in LineList
                    Result = imUtil.spec.waveCalib.fitWaveCalib([StoredData.PeakPos].',[StoredData.Wave].', Args.fitWaveCalibArgs{:});
                    
                    PeakCalcWave = Result.pix2wave(PeakLocation, Result);
                    
                    Npeak = numel(PeakCalcWave);
                    % search for wavelength in LineList
                    for Ipeak=1:1:Npeak
                        [MinDistWave, MinIndWave] = min(abs(PeakCalcWave(Ipeak) - Args.LineList));
                        if MinDistWave<Args.MaxDistWave
                            Ind = Ind + 1;
                            StoredData(Ind).IsAuto  = true;
                            StoredData(Ind).PeakPos = PeakLocation(Ipeak);
                            StoredData(Ind).PeakVal = PeakHeight(Ipeak);
                            StoredData(Ind).Wave    = Args.LineList(MinIndWave);
                            StoredData(Ind).PointH  = plot(PeakLocation(Ipeak), PeakHeight(Ipeak), 'o', Args.AutoPeakMarker{:});
                        end
                    end                    
                    
                    Result = imUtil.spec.waveCalib.fitWaveCalib([StoredData.PeakPos].',[StoredData.Wave].', Args.fitWaveCalibArgs{:});
                    
                otherwise
                    showMenu();
            end
        else
            % used mouse - get position
            if IsWaveMode
                Ans = input('Enter wavelength : ','s');
                Wave = str2double(Ans); 
            else
                Ans = input('Enter line index : ','s');
                LineInd  = str2double(Ans);
                Wave     = Args.List(LineInd);
            end
            
            Ind = Ind + 1;
            StoredData(Ind).ClickXY = XY;
            
            [IntPosX,  IntPosY] = plot.xy2axesPos(Ha, XY(1), XY(2), XlimYlim);
            
            Dist = sqrt((IntPosX - PeakAxisX).^2 + (IntPosY - PeakAxisY).^2);
            [Dist, MinInd] = min(Dist);
            
            StoredData(Ind).Dist    = Dist;
            StoredData(Ind).PeakPos = PeakLocation(MinInd);
            StoredData(Ind).PeakVal = PeakHeight(MinInd);
            StoredData(Ind).Wave    = Wave;
            StoredData(Ind).MinInd  = MinInd;
            StoredData(Ind).PointH = plot(PeakLocation(MinInd), PeakHeight(MinInd), 'o',Args.GoodPeakMarker{:});
            StoredData(Ind).IsAuto = false;
        end
        
    end

end

% internal functions
function showMenu()
    % show menu
    
    fprintf('\n');
    fprintf('q - quit\n');
    fprintf('h - show this menu/help\n');
    fprintf('z - zoom\n')
    fprintf('l - show line list\n')
    fprintf('w - Change mode to mark line by wavelength\n')
    fprintf('n - Change mode to mark line by number\n')
    fprintf('d - delete line\n')
    fprintf('f - fit and find more lines\n')
    fprintf('i - identify all lines in list\n')
end

function showLineList(List)
    % show line list
   
    N = size(List, 1);
    if N>0
        fprintf('Line list:\n');
        for I=1:1:N
            fprintf('%3d | %9.3f\n',I, List(I));
        end
    end
    
    
end


function [Result] = interactiveWaveCalib(Spec, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2023 Dec) 
    % Example: imUtil.spec.waveCalib.interactiveWaveCalib(Raper.Spec, 'LineList',ArcSpec.Lines)

    arguments
        Spec
        Args.LineList          = [];
        
        Args.localMaxArgs cell = {};
        Args.StdFilterHalfSize = [30 200];
        Args.Threshold         = 10;
    end
    
    if isvector(Spec)
        Flux  = Spec(:);
        Nwave = numel(Spec);
        Wave  = (1:1:Nwave).';
    else
        Wave  = Spec(:,1);
        Flux  = Spec(:,2);
    end
    
    % find peaks in spectra
    LocalMax = timeSeries.peaks.localMax(Flux, Args.localMaxArgs{:}, 'Dim',1);
    %StdF     = timeSeries.filter.filterStd(Flux, Args.StdFilterHalfSize, 'Dim',1);
    StdF     = 1.253.*mad(Flux);
    Nsigma   = Flux(LocalMax.FlagLocalMax)./StdF;
    Flag     = Nsigma>Args.Threshold;
    IndMax   = find(LocalMax.FlagLocalMax);
    IndMax   = IndMax(Flag);
    

    plot(Wave, Flux, 'k-');
    Ha = gca;
    hold on
    plot(Wave(IndMax), Flux(IndMax), 'ro'); 
    
    showMenu();
    
    Cont = true;
    Ind  = 0;
    while Cont
        Ans = input('  Click any key from the menu: q-quit|h-show menu : ','s');
        switch lower(Ans)
            case 'q'
                Cont = false;
            case 'h'
                showMenu();
            case  'm'
                input('  Zoom if need - press any key to continue : ','s');
                [Res,FigH,Data,Nearest] = plot.getInteractive(Ha, 'mouse', 'DataInd',1);  % 1 for selecting only the peaks (circles)
                showLineList(Args.LineList);
                
                Ans = input('Enter wavelength (>1000) or line index from list : ','s');
                LineWave = str2double(Ans);
                if isnan(LineWave)
                    fprintf('Can not convert input to double\n');
                else
                    if LineWave<1000
                        % ise line index in list:
                        Wave = Args.LineList(LineWave);
                    else
                        Wave = LineWave;
                    end
                    % store data
                    Ind = Ind + 1;
                    StoredData(Ind).Pos  = Nearest.X;
                    StoredData(Ind).Val  = Nearest.Y;
                    StoredData(Ind).Wave = Wave;
                end
                
            case 'f'
                % fit
                R = imUtil.spec.waveCalib.fitWaveCalib([StoredData.Wave].',[StoredData.Pos].')
                
            otherwise
                fprintf('Unknown key - use one of the following:\n');
                showMenu();
        end
    
    end
    
    % ask the user to select a line and give its wavelength
    % if LineList is available then the user can select from the list
    % iterate
    
    % If line list is available then after a 2 lines will attempt to find
    % all the other lines
    
    
    

end

% internal functions
function showMenu()
    % show menu
    
    fprintf('\n');
    fprintf('q - quit\n');
    fprintf('h - show this menu/help\n');
    fprintf('m - mark line\n')
    fprintf('f - fit and find more lines\n')
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



classdef SnrGuiIni < Component
    % Parent class for file based processing
    %
    % Poll input folder for new files:
    %   Call derived processFileImpl() function 
    %   Delete or move the processed file to archive folder
    %   Clean archive folder after specified numberof days
    
    
    % Properties
    properties (SetAccess = public)
              
        %
        IniFileName = 'c:/soc/config/snr_gui.ini';   %
        fid         = [];
    end
    
    %-------------------------------------------------------- 
    methods  
               
        % Constructor    
        function Obj = SnrGuiIni(Args)          
            % Constructor for FileProcessor
            % Input   : - struct array, table, cell array, matrix,
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             'InputPath' - 
            %             'InputMask' - 
            %             'ProcessedPath' - 
            % Output  : - New instance of FileProcessor object
            % Author  : Chen Tishler (2021)
            % Example : 

            arguments
                Args.IniFileName = '';         %
            end
            
            Obj.setName('SnrGuiIni');            
            
            if ~isempty(Args.IniFileName)
                Obj.IniFileName = Args.IniFileName;
            end
        end
              
        %==================================================================
        function Result = process(Obj)
            % Input   : - Object            
            %                      
            % Output  : 
            % Author  : Chen Tishler (Dec. 2022)
            % Example : 
            %           
            arguments
                Obj
            end
            
            Obj.msgLog(LogLevel.Debug, 'process: %s', Obj.IniFileName);                            
            
            % Create file
            Obj.fid = fopen(Obj.IniFileName, 'wt');

            %------------------------------------------------------            
            % Header
            Obj.wrLine(sprintf('; ULTRASAT SNR GUI'));
            Obj.wrLine(sprintf('; Auto generated file by: %s', mfilename('fullpath')));
            Obj.wrLine(sprintf('; Created: %s', datestr(now, 'yyyy-mm-dd HH:MM:SS')));
            
            % R
            Obj.wrSection('R');
            Obj.wrHint('Radial distance from center of field of view [Deg]');            
            Obj.wrDescription('');            
            Obj.wrDefault('R01');
            Obj.wrCount(25);
            for I=1:25
                Obj.wrItem(I, sprintf('R%02d', I));
            end
            
            % Exposure time
            Obj.wrSection('ExpTime');
            Obj.wrHint('Exposure time in seconds');            
            Obj.wrDescription('');
            Obj.wrMin(1);
            Obj.wrMax(999);            
            Obj.wrDefault('300');
            
            % Number of images
            Obj.wrSection('NumImages');
            Obj.wrHint('Number of images');            
            Obj.wrDescription('');            
            Obj.wrMin(1);
            Obj.wrMax(999);
            Obj.wrDefault('1');
            
            % Source - Pickles Models
            Obj.wrSection('Source_PicklesModels');
            Obj.wrHint('Select Pickles Models');            
            Obj.wrDescription('');            
            Obj.wrDefault('AB');
            
            % Pickles Models - Combo Items            
            Pickles = {'P1', 'P2', 'P3'};
            Obj.wrCount(numel(Pickles));
            for i=1:numel(Pickles)
                Obj.wrItem(i, Pickles{i});
            end
            
            % Source - Black Body with Temperature
            Obj.wrSection('Source_BlackBody');
            Obj.wrHint('Black body temperature, 500..500000');            
            Obj.wrDescription('');            
            Obj.wrMin(500);
            Obj.wrMax(500000);            
            Obj.wrDefault(1000);
            
            % Output type - Limiting Magnitude
            Obj.wrSection('Out_LimitingMagnitude');
                      
            % Output type - SNR
            Obj.wrSection('Out_Snr');            
            
            % SNR - Magnitude
            Obj.wrSection('SnrMagnitude');
            Obj.wrHint('Magnitude, -25..30');            
            Obj.wrDescription('');            
            Obj.wrMin(-25);
            Obj.wrMax(30);            
            Obj.wrDefault(10);
            
            % SNR - Calib Filter Family
            Families = {'F1', 'F2', 'F3'};
            Obj.wrSection('CalibFilterFamily');       
            Obj.wrHint('Select Calibration family');            
            Obj.wrDescription('');                        
            Obj.wrCount(numel(Families));
            for i=1:numel(Families)
                Value = Families{i};
                if i == 1
                    Obj.wrDefault(Value);
                end
                
                Obj.wrItem(i, Value);
            end            
            
            % SNR - Calib Filter - list per Family
            for i=1:numel(Families)            
                Obj.wrSection(sprintf('CalibFilter_F%d', i));
                Obj.wrHint('Select Calibration filter for family ...');            
                Obj.wrDescription('');                            
                Obj.wrCount(3);
                for j=1:3
                    Value = sprintf('Filter_F%d_%02d', i, j);
                    if j == 1
                        Obj.wrDefault(Value);
                    end                    
                    Obj.wrItem(j, Value);
                end
            end
            
            % SNR - Calib Magnitude System
            Obj.wrSection('CalibMagnitudeSystem');
            Obj.wrHint('Select Calibration magnitude system');            
            Obj.wrDescription('This is the description BLA BLA BLA ...');            
            Obj.wrDefault('AB');
            Obj.wrCount(2);            
            Obj.wrItem(1, 'AB');
            Obj.wrItem(2, 'Vega');            

            % Result - Message
            Obj.wrSection('SnrMessage');
            Obj.wrHint('SNR calculation message');            
            Obj.wrDescription('');            
            
            % Result - Value
            Obj.wrSection('SnrResult');
            Obj.wrHint('SNR calculation result');            
            Obj.wrDescription('');            
            
            %------------------------------------------------------
            % Close file            
            fclose(Obj.fid);
                
            Obj.msgLog(LogLevel.Info, 'process done');
            Result = true;
        end
        
        %==================================================================        
        function wrSection(Obj, SectionName)            
            % Start new section
            % Input   : - Object
            %           - SectionName
            %
            % Output  : -
            % Author  : Chen Tishler (2022)
            % Example : 
            %
            fprintf(Obj.fid, '\n[%s]\n', SectionName);
        end
        
        
        function wrLine(Obj, Line)
            % Input   : - Object            
            %                      
            % Output  : 
            % Author  : Chen Tishler (Dec. 2022)
            % Example : 
            %                       
            fprintf(Obj.fid, '%s\n', Line);
        end

        
        function wrKey(Obj, Key, Value)
            % Input   : - Object            
            %                      
            % Output  : 
            % Author  : Chen Tishler (Dec. 2022)
            % Example : 
            %                       
            fprintf(Obj.fid, '%s=%s\n', Key, Value);
        end
        
        %==================================================================                
        function wrHint(Obj, Value)
            Obj.wrKey('Hint', Value);
        end
        
        function wrDescription(Obj, Value)
            Obj.wrKey('Description', Value);
        end        

        function wrValue(Obj, Key, Value)
            if isnumeric(Value)            
                Obj.wrKey(Key, sprintf('%d', Value));
            else
                Obj.wrKey(Key, Value);
            end                        
        end
        
        function wrDefault(Obj, Value)
            Obj.wrValue('Default', Value);
        end                
        
        function wrCount(Obj, Value)
            Obj.wrValue('Count', Value);
        end                        

        function wrItem(Obj, Index, Value)
            Obj.wrValue(sprintf('Item%d', Index), Value);
        end                        
        
        function wrMin(Obj, Value)
            Obj.wrValue('Min', Value);
        end                        
        
        function wrMax(Obj, Value)
            Obj.wrValue('Max', Value);
        end                        
        %==================================================================                
    end

    
    % Unit test
    methods(Static)   
        function Result = unitTest()
            Obj = ultrasat.SnrGuiIni();
            Obj.process();
            Result = true;
        end
    end    
        
end

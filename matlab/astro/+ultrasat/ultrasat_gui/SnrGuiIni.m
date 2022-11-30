
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
        FileName = 'snr_gui.ini';   %
        fid      = [];
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
                Args.IniFileName = 'snr_gui.ini';         %
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
            Obj.fid = fopen(Obj, IniFileName, 'wt');

            % Header
            Obj.writeLn('; ULTRAAST SNR GUI');
            Obj.writeLn('');
            
            % R
            Obj.section('');
            Obj.description('');            
            Obj.hint('');
            Obj.default('AB');
            
            % Exposure time
            Obj.section('');
            Obj.description('');            
            Obj.hint('');
            Obj.default('AB');
            
            % Number of images
            Obj.section('');
            Obj.description('');            
            Obj.hint('');
            Obj.default('AB');
            
            % Source - Pickles Models
            Obj.section('');
            Obj.description('');            
            Obj.hint('');
            Obj.default('AB');
            
            % Source - Black Body with Temperature
            Obj.section('');
            
            % Output type
            Obj.section('');
            
            % Limiting Magnitude
            Obj.section('');
            
            % SNR
            Obj.section('');            
            
            % SNR - Magnitude
            Obj.section('SnrMagnitude');                        
            
            % SNR - Calib Filter Family
            Families = [];
            Obj.section('');       
            Obj.default('');
            
            % SNR - Calib Filter - per Family
            Obj.section('');                   
            
            % SNR - Calib Magnitude System
            Obj.section('CalibMagnitudeSystem');
            Obj.description('');            
            Obj.hint('');
            Obj.default('AB');
            Obj.writeKey('Count', 2);            
            Obj.writeKey('Item1', 'AB');
            Obj.writeKey('Item2', 'Vega');            

          
            % Close file            
            fclose(Obj.fid);
                
            Obj.msgLog(LogLevel.Info, 'process done');
            Result = true;
        end
        
        %==================================================================        
        function section(Obj, SectionName)            
            % Start new section
            % Input   : - Object
            %           - SectionName
            %
            % Output  : -
            % Author  : Chen Tishler (2022)
            % Example : 
            %
            fprintf(Obj.fid, '[%s]\n', SectionName);
        end
        
        
        function writeLn(Obj, Line)
            % Input   : - Object            
            %                      
            % Output  : 
            % Author  : Chen Tishler (Dec. 2022)
            % Example : 
            %                       
            fprintf(Obj.fid, '%s\n', Line);
        end

        
        function writeKey(Obj, Key, Value)
            % Input   : - Object            
            %                      
            % Output  : 
            % Author  : Chen Tishler (Dec. 2022)
            % Example : 
            %                       
            fprintf(Obj.fid, '%s=%s\n', Key, Value);
        end
        
        %==================================================================                
        function hint(Obj, Value)
            Obj.writeValue('Hint', Value);
        end
        
        function description(Obj, Value)
            Obj.writeValue('Description', Value);
        end        

        function default(Obj, Value)
            Obj.writeValue('Default', Value);
        end                
        
        function count(Obj, Value)
            Obj.writeValue('Count', Value);
        end                        

        function item(Obj, Index, Value)
            Obj.writeValue(sprintf('Item%d', Index), Value);
        end                        
        
        function min(Obj, Value)
            Obj.writeValue('Min', Value);
        end                        
        
        function max(Obj, Value)
            Obj.writeValue('Max', Value);
        end                        
        %==================================================================                
    end

    
    % Unit test
    methods(Static)   
        function Result = unitTest()
            Obj = SnrGuiIni();
            Obj.process();
            Result = true;
        end
    end    
        
end

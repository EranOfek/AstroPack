% A class to control and manipulate ds9
%   This class (DS9) suppose to replace the ds9 class
% Description: A class for intearction with the ds9 display.
%              This include functions to load images, change their
%              properties, create and plot region files, printing, image
%              examination, interaction with SIM content and more.
%              Type "ds9." followed by <tab> to see the full list of
%              functions.
%              Full manual is available in manual_ds9.pdf
% Input  : null
% Output : null
% Tested : Matlab R2014a
%     By : Eran O. Ofek                    Jul 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Reliable: 2
%--------------------------------------------------------------------------
%
%
% BUGS & ISSUES:
% If ds9.open does not display the image - see possible problem/solution:
%       https://docs.google.com/document/d/1Q2qI25B9DlF2i7IbWdmPPt3sn3TL0Eq0P5HEaYI6HfM/edit#
%       Fix a contradiction in the matlab libraries, that cause a problem running system commands:
%       sudo mv /usr/local/MATLAB/R2020b/sys/os/glnxa64/libstdc++.so.6 /usr/local/MATLAB/R2020b/sys/os/glnxa64/libstdc++.so.6.orig
%
%


classdef DS9_new < handle
    properties
        MethodXPA              = 'ds9';   % Index of current active ID - 
        Frame                  = [];
        InfoAI                    % struct with:
                                  % .Image - An AstroImage
                                  % .Win - Display window
                                  % .Frame - Frame number
                                  % .FileName - File name
        
    end
        
    % Constructor method (display)
    methods
        function Obj = DS9(varargin)
            % Create a DS9 object and open a ds9 window (if not exist)
            %   Use load method to display images.
            %   Use open method to open additional ds9 windows.
            % Input  : * Arbitrary number of arguments that will be passed
            %            to the disp method.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D = DS9(rand(100,100),1)
            
            
            Obj.open;
            
            if nargin>0
                Obj.load(varargin{:});
                
            end
        end
        
    end
    
    methods % setters/getters
        function Val = get.Frame(Obj)
            % getter for the DS9 Frame property
           
            Val = Obj.frame;
            Obj.Frame = Val;
        end
        
        function Obj = set.Frame(Obj, Val)
            % setter for the DS9 Frame property
           
            Obj.frame(Val);
        end
    end
    
    methods (Static) % ID utilities
        function [Result,AllMethods] = getAllWindows
            % get all active ds9 windows methods and names
            % This can be used to identify the names of active ds9
            % To change the ds9 display used by the DS( object, set its
            % MethodXPA propery to the ds9 method you want to use.
            % Output : - A structure array with element per each ds9 window
            %            currently open, and the fields:
            %            .Method
            %            .Name
            %            from the xpa info.
            %          - A cell array of all ds9 method names.
            % Author : Eran Ofek (May 2022)
            % Example: DS9.getAllWindows
            
            String = 'xpaget ds9 xpa info';
            [~,Answer] = system(String);
            IsError    = DS9.isxpaError(Answer);
            if IsError
                Result = struct('Method',{}, 'Name',{});
                AllMethods = {};
            else
                Name   = regexp(Answer,'XPA_NAME:\s+(?<Name>\w+)','names');
                Method = regexp(Answer,'XPA_METHOD:\s+(?<Method>\w+\:\w+)','names');
            
                if numel(Name)==numel(Method)
                    %
                    for I=1:1:numel(Name)
                        Result(I).Method = Method(I).Method;
                        Result(I).Name   = Name(I).Name;
                    end
                else
                    Answer
                    error('Number of ds9 names not equal to number of ds9 methods');
                end
                
                AllMethods = {Result.Method};
            end
        end
        
    end
    
    methods (Static) % xpa commands construction and execuation
        % execute xpa command
        function [Answer,Status,IsError] = system(String,varargin)
            % Construct and execute an xpa command
            % Package: @ds9
            % Input  : - String to execute. The string may include any
            %            special characters (e.g., %s, %d) that will be
            %            populated by the additional parameters.
            %          * Arbitrary number of additional parameters that
            %            will populate the %s %d like format.
            % Output : - Answer
            % Example: ds9.system('xpaset -p ds9 frame frameno %d',FrameNumber);
            % Reliable: 2
            String = sprintf(String,varargin{:});
            if ismac
               String = strcat('set DYLD_LIBRARY_PATH "";', String);
               [Status,Answer]=system(String);
            elseif isunix
               [Status,Answer]=system(String);
            else
               fprintf('\ds9.system(): Windows is not supported yet!\n');
            end
            if (Status~=0)
                if contains(Answer,'not found')
                    % It is possible that xpa is not installed
                    fprintf('\n It seems that xpa and/or ds9 are not installed\n');
                    fprintf('ds9 installation: http://ds9.si.edu/site/Download.html\n');
                    fprintf('xpa installation: http://hea-www.harvard.edu/RD/xpa/index.html\n');
                end
                error('Command: %s failed - Answer: %s',String,Answer);
                
            end
            
            if nargout>2
                IsError = DS9.isxpaError(Answer);
            end
        end

        % construct xpa command
        function String = construct_command(varargin)
            % Construct an arguments string for ds9 command
            % Package: @ds9
            % Input  : * Arbitrary number of arguments from which to
            %            construct a string.
            %            Default is ''.
            % Output : The constructed string
            % Example: ds9.construct_command('scale linear');
            %          ds9.construct_command scale linear
            %          ds9.construct_command('log',100); % return 'log 100'
            %          ds9.construct_command('log 100');
            % Reliable: 2
            String = '';
            Narg = numel(varargin);
            for Iarg=1:1:Narg
                if (ischar(varargin{Iarg}))
                    String = sprintf('%s %s',String,varargin{Iarg});
                else
                    if (mod(varargin{Iarg},1)==0)
                        % argument is an integer
                        String = sprintf('%s %d',String,varargin{Iarg});
                    else
                        % argument is a double
                        String = sprintf('%s %f',String,varargin{Iarg});
                    end
                end
            end
            
        end
        
        % xpa help
        function xpahelp
            % Open the XPA command help web page
            % Package: @ds9
            % Input  : null
            % Output : null
            % Example: ds9.xpahelp
            % Reliable: 2
            web('http://ds9.si.edu/doc/ref/xpa.html');
            
        end
        
        % is error
        function Result = isxpaError(String)
            % Check if xpa anser is an error 
            % Input  : - A string returned by xpaget
            % Output : - A logical indicating if an error
            % Author : Eran Ofek (May 2022)
            % Example: Result = DS9.isxpaError('XPA$ERROR no 'xpaget' access points match template: ds9');
            Result = contains(String, 'XPA$ERROR');
        end
    
        % check if ds9 is open
        function Number = isOpen(UsePS)
            % Return the number of open ds9 windows
            % Input  : - A logical indicating if to use the xpaaccess
            %            (false) or the ps (true) to determine if a ds9
            %            window is open. Default is false.
            % Output : The numer of open ds9 windoes.
            % Author : Eran Ofek (May 2022)
            % Example: DS9.isOpen
            
            arguments
                UsePS logical    = false;
            end
            
            if ~UsePS
                try
                    Ans = DS9.getAllWindows;
                    Number = numel(Ans);
                catch
                    Number = 0;
                end
            else
                if (ismac)
                    Status = system('ps -A | grep ds9 | grep -v grep  > /dev/null');
                else
                    Status = system('ps -xug | grep ds9 | grep -v grep  > /dev/null');
                end
                if (Status==1)
                    % not open
                    Number = 0;
                else
                    % open
                    Number = 1;
                end
            end
            
        end
        
        function Result = parseOutput(String, OutType)
            % parse xpaget output string into array of strings/numbers
            % Input  : - A string
            %          - Output type:
            %            'cell' - Seperate the input string by \s (space
            %                   char) and return a cell array of the
            %                   seperated strings.
            %            'num' - Seperate the input string by \s (space
            %                   char) and return the seperated strings
            %                   converted to array of numbers. NaN if not
            %                   convertable.
            %            Default is 'cell'.
            % Output : - The seperated string in a cell of strings or
            %            numeric array format.
            % Author : Eran Ofek (May 2022)
            % Example: Result = parseOutput(String, 'num');
            
            arguments
                String
                OutType     = 'cell';
            end
            
            SpStr = regexp(String,'\s','split');
            % remove empty
            Flag  = ~cellfun(@isempty,SpStr);
            SpStr = SpStr(Flag);
            
            switch lower(OutType)
                case 'cell'
                    Result = SpStr;
                case 'num'
                    Result = str2double(SpStr);
                otherwise
                    error('Unknown OutType option');
            end
            
        end        
       
        function [Color, Marker, FullColor] = parseColorSymbol(ColorSymbolStr)
            % Parse a color symbol (plot) string into color and symbol
            % Input  : - A color symbol string (e.g., 'ro')
            % Output : - Color (on letter)
            %          - Marker
            %          - Full color name
            % Author : Eran Ofek (May 2022)
            % Example: [a,b,c]=DS9.parseColorSymbol('ro')
            
            if contains(ColorSymbolStr, 'o')
                Marker = 'o';
            elseif contains(ColorSymbolStr, 's')
                Marker = 's';
            else
                Marker = 'o';
            end
            if contains(ColorSymbolStr, 'r')
                FullColor = 'red';
                Color     = 'r';
            elseif contains(ColorSymbolStr, 'b')
                FullColor = 'blue';
                Color     = 'b';
            elseif contains(ColorSymbolStr, 'g')
                FullColor = 'green';
                Color     = 'g';
            elseif contains(ColorSymbolStr, 'k')
                FullColor = 'black';
                Color     = 'k';
            elseif contains(ColorSymbolStr, 'w')
                FullColor = 'white';
                Color     = 'w';
            elseif contains(ColorSymbolStr, 'm')
                FullColor = 'magenta';
                Color     = 'm';
            elseif contains(ColorSymbolStr, 'c')
                FullColor = 'cyan';
                Color     = 'c';
            elseif contains(ColorSymbolStr, 'y')
                FullColor = 'yellow';
                Color     = 'y';
            else
                FullColor = 'red';
            end
        end
        
    end
   
    methods (Static)  % utility functions
        function [ImageName, AI] = loadPrep(Image, Args)
            % An internal utility function for the load command
            % Input  : - A DS9 object.
            %          - An image, or images. One of the following:
            %            1. An AstroImage array.
            %            2. A file name with wild cards.
            %            3. A cell array of file names.
            %            4. A matrix or a cell array of matrices.
            %          * ...,key,val,...
            %            'UseRegExp' - A logical indicating if to use
            %                   regular expressions when interpreting a
            %                   singel file name. If false, will use only
            %                   wild cards. Default is false.
            %            'DataProp' - A data property in the AstroImage
            %                   from which to read the image.
            %                   Options are: 'Image', 'Back', 'Var', 'Mask', 'PSF.
            %                   Default is 'Image'.
            %            'FileName' - A cell array of optional file names
            %                   in which to write the FITS images.
            %                   If empty, use tempname to generate file
            %                   names.
            %                   Default is {}.
            % Output : - The name of the FITS file name containing the image.
            %          - An AstroImage object containing the images.
            %            Created only if second argument is requested.
            % Author : Eran Ofek (May 2022)
            
            arguments
                Image
                Args.UseRegExp logical    = false;
                Args.DataProp             = 'Image';
                Args.FileName             = {};  % use tempname
            end
            
            if nargout>1
                PopAI = true;
            else
                PopAI = false;
            end
            
            if ischar(Args.FileName)
                Args.FileName = {Args.FileName};
            end
            
            if ischar(Image)
                Image = io.files.filelist(Image, Args.UseRegExp);
            end
            
            if isnumeric(Image)
                Image = {Image};
            end
            
            Nim = numel(Image);
            
            ImageName = cell(1,Nim);
            for Iim=1:1:Nim
                % get file name in which to save the image
                % will use this if Image is not a file name
                if isempty(Args.FileName)
                    FileName = tempname;
                else
                    FileName = Args.FileName{Iim};
                end
                if iscell(Image)
                    if isnumeric(Image{Iim})
                        % matrix image
                        % save to FITS file on disk
                        FITS.writeSimpleFITS(Image{Iim}, FileName);
                        ImageName{Iim} = FileName;
                        if PopAI
                            AI(Iim) = AstroImage;
                            AI(Iim).Image = Image{Iim};
                        end
                    elseif ischar(Image{Iim})
                        % image name
                        ImageName{Iim} = Image{Iim};
                        if PopAI
                            AI(Iim) = AstroImage(Image{Iim});
                        end
                    else
                        error('Unknown Image formation option');
                    end
                elseif isa(Image, 'AstroImage')
                    % AstroImage
                    FITS.writeSimpleFITS(Image(Iim).(DataProp), FileName, 'Header',Image(Iim).HeaderData.Data);
                    ImageName{Iim} = FileName;
                    if Iim==1 && PopAI
                        AI = Image;
                    end
                else
                    error('Unknown Image formation option');
                end
                    
                
            end
        end
        
        function xpasetStatic(Command, varargin)
            % Execute an xpaset command (Static version)
            %   See xpaset  and xpasetFrame for a non-static version
            % Input  : - The command to follow the 'xpaset -p ds9'.
            %            This string may also include printf control
            %            characters like %s.
            %          * An arbitrary number of input arguments that will
            %            be inserted to the control characters in the second
            %            input argument.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: DS9.xpasetStatic('mode %s',Mode);
            
            Command = sprintf(Command,varargin{:});
            DS9.system('xpaset -p %s %s','ds9', Command);
        end
        
        function Ans = xpagetStatic(Command, varargin)
            % Execute an xpaget command (Static version)
            %   For non-static version see xpaget
            % Input  : - The command to follow the 'xpaget ds9'.
            %            This string may also include printf control
            %            characters like %s.
            %          * An arbitrary number of input arguments that will
            %            be inserted to the control characters in the second
            %            input argument.
            % Output : - Return output.
            % Author : Eran Ofek (May 2022)
            
            Command = sprintf(Command,varargin{:});
            Ans = ds9.system('xpaget %s %s','ds9', Command);
            Ans = regexprep(Ans, '\n$','');  % remove the \n at the end of the result string  
        end
    end
    
    
    methods % xpaget/xpaset
        function xpaset(Obj, Command, varargin)
            % Execute an xpaset command
            % Input  : - A DS9 object.
            %          - The command to follow the 'xpaset -p ds9'.
            %            This string may also include printf control
            %            characters like %s.
            %          * An arbitrary number of input arguments that will
            %            be inserted to the control characters in the second
            %            input argument.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: Obj.xpaset('mode %s',Mode);
            
            Command = sprintf(Command,varargin{:});
            DS9.system('xpaset -p %s %s',Obj.MethodXPA, Command);
        end
        
        function xpasetFrame(Obj, Frame, Command, varargin)
            % Execute xpaset for multiple frames
            % Input  : - A DS9 object.
            %          - A vector of frames on which to operate the xpaset
            %            command. If empty, or false use current frame.
            %            If 'all' or true apply to all frames in current window.
            %          - The command to follow the 'xpaset -p ds9'.
            %            This string may also include printf control
            %            characters like %s.
            %          * An arbitrary number of input arguments that will
            %            be inserted to the control characters in the second
            %            input argument.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100),1);
            %          D.load(rand(200,200),2)
            %          D.xpasetFrame(true,'zoom to 2')
            %          D.xpasetFrame(true,'zoom to 4')
           
            if isempty(Frame)
                % use current frame
                Frame = Obj.frame;
            end
            if ischar(Frame)
                % use all frames
                [~,Frame] = Obj.nframe;
            elseif islogical(Frame)
                if Frame
                    % use all frames
                    [~,Frame] = Obj.nframe;
                else
                    % use current frame
                    Frame = Obj.frame;
                end
            else
                % do nothing
            end
            
            % get current frame
            CurFrame = Obj.frame;
            
            Nframe = numel(Frame);
            for Iframe=1:1:Nframe
                Obj.frame(Frame(Iframe));
                Obj.xpaset(Command,varargin{:});
            end
            
            % back to original frame
            Obj.frame(CurFrame);
        end
        
        function Ans = xpaget(Obj, Command, varargin)
            % Execute an xpaget command
            % Input  : - A DS9 object.
            %          - The command to follow the 'xpaget ds9'.
            %            This string may also include printf control
            %            characters like %s.
            %          * An arbitrary number of input arguments that will
            %            be inserted to the control characters in the second
            %            input argument.
            % Output : - Return output.
            % Author : Eran Ofek (May 2022)
            
            Command = sprintf(Command,varargin{:});
            Ans = DS9.system('xpaget %s %s',Obj.MethodXPA, Command);
            Ans = regexprep(Ans, '\n$','');  % remove the \n at the end of the result string  
        end
    
        function List = selectWindow(Obj, Id)
            % select/focus on one of the open ds9 windows by running number
            % Input  : - A DS9 object.
            %          - A running index for the ds9 window.
            %            If Inf use last entry.
            %            Default is 1.
            % Output : - The first output argument of DS9.getAllWindows
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9; List = selectWindow(D)
            
            arguments
                Obj
                Id = 1;
            end
            
            List = DS9.getAllWindows;
            if isempty(List)
                Obj.MethodXPA = [];
            else
                if isinf(Id)
                    Obj.MethodXPA = List(end).Method;
                else
                    Obj.MethodXPA = List(Id).Method;
                end
            end
            
        end
         
        function [Result, ChangeResult] = isWindowExist(Obj, ChangeIfNotExist)
            % Check if the current MethodXPA in the DS9 object exist/open.
            % Input  : - A DS9 object.
            %          - A logical indicating what to do if the the ds9
            %            window doesn't exist. If false do nothing.
            %            If true, then will call selectWindow that will
            %            shift the focus to the first listed ds9 window.
            %            The existsence of such window will be indicated in
            %            the second output argument.
            % Output : - A logical indicating if the Method property in the
            %            DS9 object is a valid/exist ds9 window.
            %          - If changed window this is a logical indicating if
            %            the new window exist. 
            % Author : Eran Ofek (May 2022)
            % D = DS9; R = isWindowExist(D);
            
            arguments
                Obj
                ChangeIfNotExist logical   = false;
            end
            
            if Obj.isOpen
                All    = DS9.getAllWindows;
                Result = any(strcmp({All.Method}, Obj.MethodXPA));
                ChangeResult = true;

                if ChangeIfNotExist
                    Obj.selectWindow;
                    if nargout>1
                        [~, ChangeResult] = isWindowExist(Obj, false);
                    end
                end
            else
                ChangeResult = false;
                Result       = false;
            end
            
        end
    end
    
    methods  % AI and InfoAI utilities
        function Obj = addAI(Obj, Image, FileName, Frame)
            % Add an AstroImage to AI and InfoAI properties
            % Input  : - A DS9 object.
            %          - An AstroImage object, or a FITS file name, or cell
            %            array of file names.
            %          - A file name, or a cell array of file names.
            %            Each file name corresponds to an AstroImage
            %            element.
            %            If empty, then generate a file name using
            %            tempname. Default is [].
            %          - A vector of frame indices, corresponding to the
            %            AstroImage elements.
            %            If empty (and AstroImage input is a scalar), then
            %            use the current frame number. Default is [].
            % Output : - An updated DS9 object with the .AI and .InfoAI
            %            properties updated.
            % Author : Eran Ofek (May 2022)
            % Example: 
            
            arguments
                Obj
                Image
                FileName   = [];
                Frame      = [];
            end
           
            if ischar(Image)
                Image = {Image};
            end
            if isa(Image, 'AstroImage')
                ImageAI = Image;
            else
                ImageAI = AstroImage(Image);
            end
            
            if ischar(FileName)
                FileName = {FileName};
            end
                        
            Nim = numel(ImageAI);
            if isempty(Frame)
                if Nim==1
                    % get current frame number;
                    Frame = Obj.frame;
                else
                    error('If frame number is empty, the ImageAI must be a scalar');
                end
            end
            
            for Iim=1:1:Nim
                Nai = numel(Obj.InfoAI);
                I   = Nai + 1;   % new AI

                Obj.InfoAI(I).Image    = ImageAI(Iim);
                Obj.InfoAI(I).Win      = Obj.MethodXPA;
                if isempty(FileName)
                    Obj.InfoAI(I).FileName = tempname;
                else
                    Obj.InfoAI(I).FileName = FileName{Iim};
                end
                Obj.InfoAI(I).Frame    = Frame(Iim);
            end
                        
        end
            
        function Obj = deleteAI(Obj, ID, Frame, Window)
            % Delete entries from InfoAI property in the DS9 object
            % Input  : - A DS9 object.
            %          - Entry number in the InfoAI propery.
            %            If empty, select by frame number and window name.
            %            If 'all' - delete for all frames in window.
            %            Default is []
            %          - A vector of frame numbers to remove.
            %            If empty, use current frame.
            %            Default is 1.
            %          - A window name to remove. If empty, use active
            %            window.
            % Output : - An updated DS9 object with the selected entries
            %            removed from the InfoAI propery.
            % Author : Eran Ofek (May 2022)
            % Example: 
            
            arguments
                Obj
                ID        = [];
                Frame     = 1;
                Window    = [];
            end
           
            if isempty(Frame)
                Frame = Obj.frame;
            end
            
            if isempty(ID)
                % remove InfoAI entry using Frame and Window
                if isempty(Window)
                    % use current window
                    Window = Obj.MethodXPA;
                end
                if ischar(Frame) || isinf(Frame)
                    % get all frame numbers
                    [~,Frame] = Obj.nframe;
                end
                Nf  = numel(Frame);
                Nai = numel(Obj.InfoAI);
                FlagDel = false(Nai,1);
                for Iai=1:1:Nai
                    if ischar(Frame)
                        % delete for all frames in window
                        Flag = strcmp({Obj.InfoAI(Iai).Win}, Window);
                    else
                        Flag = (Obj.InfoAI(Iai).Frame == Frame) & strcmp({Obj.InfoAI(Iai).Win}, Window);
                    end
                    switch sum(Flag)
                        case 0
                            % not found
                        case 1
                            % remove Iai
                            FlagDel(Iai) = true; 
                        otherwise
                            error('InfoAI contains more than one entry per frame');
                    end
                end
                
            else
                % remove by ID entry
                Nai = numel(Obj.InfoAI);
                FlagDel = (1:1:Nai)==ID;
                
            end
            Obj.InfoAI = Obj.InfoAI(~FlagDel);
        end
        
        function [AI,Ind] = getAI(Obj, Frame, Window)
            % get the AstroImage stored in the InfoAI property for a given window/frame
            % Input  : - A DS9 object.
            %          - Frame index. If empty, use current frame.
            %            Default is [].
            %          - ds9 window name. If empty, use current window.
            %            Default is [].
            % Output : - An AstroImage object handle found in the InfoAI property
            %            that corresponds to the requested frame and window.
            %          - Index of image in InfoAI.
            % Author : Eran Ofek (May 2022)
            
            arguments
                Obj
                Frame   = [];
                Window  = [];
            end
           
            if isempty(Window)
                % get current window
                Window = Obj.MethodXPA;
            end
            if isempty(Frame)
                % get current frame
                Frame = Obj.frame;
            end
            
            Flag = strcmp({Obj.InfoAI.Win}, Window) & [Obj.InfoAI.Frame]==Frame;
            switch sum(Flag)
                case 0
                    % not found
                    AI  = [];
                    Ind = [];
                otherwise
                    % use latest
                    Ind = find(Flag, 1, 'last');
                    AI = Obj.InfoAI(Ind).Image;
            end
            
        end
        
        function Obj = popFieldsAI(Obj, Frame, Window, Args)
            % Populate, the Back, Var, Catalog, astrometry, and PSF
            % of the image in the InfoAI assocaited with the cuurent frame.
            % Input  : - A DS9 object.
            %          - Frame index. If empty, use current frame.
            %            Default is [].
            %          - ds9 window name. If empty, use current window.
            %            Default is [].
            %          * ...,key,val,...
            %            'backgroundArgs' - A cell array of additional
            %                   arguments to pass to imProc.background.background.
            %                   Default is {}.
            %            'findMeasureSourcesArgs' - A cell array of additional
            %                   arguments to pass to imProc.sources.findMeasureSources
            %                   Default is {}.
            %            'astrometryCoreArgs' - A cell array of additional
            %                   arguments to pass to
            %                   imProc.astrometry.astrometryCore
            %                   Default is {};
            %            'astrometryRefineArgs' - A cell array of additional
            %                   arguments to pass to
            %                   imProc.astrometry.astrometryRefine
            %                   Default is {};
            %            'constructPSFArgs' - A cell array of additional
            %                   arguments to pass to
            %                   imProc.psf.constructPSF
            %                   Default is {};
            %            'Verbose' - Show status messages. Default is true.
            %            'RePop' - Repopulate Back, Var, Cat, PSF, even if
            %                   exist. Default is false.
            % Output : - A DS9 object, in which the current frame InfoAI
            %            image is updated with the populated fields.
            % Author : Eran Ofek (May 2022)
            % Example: D.load('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits')
            %          D.popFieldsAI;
            
            arguments
                Obj
                Frame                            = [];
                Window                           = [];
                Args.backgroundArgs cell         = {};
                Args.findMeasureSourcesArgs cell = {};
                Args.astrometryCoreArgs cell     = {};
                Args.astrometryRefineArgs cell   = {};
                Args.constructPSFArgs cell       = {};
                Args.Verbose logical             = true;
                Args.RePop logical               = false;
            end
            
            [AI, Ind] = getAI(Obj, Frame, Window);
            
            % background
            if isemptyImage(AI,{'Back'}) || isemptyImage(AI,{'Var'}) || Args.RePop
                if Args.Verbose
                    fprintf('Estimating background\n');
                end
                AI = imProc.background.background(AI, Args.backgroundArgs{:});
            end
            
            % source find
            if sizeCatalog(AI)==0 || Args.RePop
                if Args.Verbose
                    fprintf('Find sources\n');
                end
                AI = imProc.sources.findMeasureSources(AI, Args.findMeasureSourcesArgs{:});
            end
            
            % Estimate PSF
            if isemptyPSF(AI) || Args.RePop
                if Args.Verbose
                    fprintf('Estimate PSF and measure PSF photometry\n');
                end
                [AI] = imProc.psf.constructPSF(AI, Args.constructPSFArgs{:});
                % PSF photometry
                [AI] = imProc.sources.psfFitPhot(AI, 'CreateNewObj',false);                                   
            end
            
            % astrometryif sizeCatalog(AI)==0 || Args.RePop
            if ~imProc.astrometry.isSuccessWCS(AI) || Args.RePop
                if Args.Verbose
                    fprintf('Solve astrometry\n');
                end
                if imProc.astrometry.isSuccessWCS(AI)
                    % refine astrometry
                    AI = imProc.astrometry.astrometryRefine(AI, Args.astrometryRefineArgs{:});
                else
                    AI = imProc.astrometry.astrometryCore(AI, Args.astrometryCoreArgs{:});
                end
            end
            
            Obj.InfoAI(Ind).Image = AI;
        end
    end
    
    methods % open, exit, mode
        % open ds9
        function Found = open(Obj, New, Args)
            % Open ds9 dispaly window and set mode to region
            % Input  : - A DS9 object.
            %          - A logical indicating if to open a new
            %            window if ds9 window already exist.
            %            If true, then will shift focus to the new
            %            window.
            %            Default is false.
            %          * ...,key,val,...
            %            'Wait' - Wait after open the ds9 window.
            %                   Default is 3 [s].
            % Output : - A logical indicating if a new window was opened.
            % Author : Eran Ofek (May 2022)
            % Problems: The command ds9 is not recognized by the bash interpreter, try adding an alias
            %           to the bash profile using ‘vim .bash_profile’ adding the following line to the
            %           profile ‘alias ds9="open -a /Applications/SAOImageDS9.app/Contents/MacOS/ds9"
            % Example: DS9.open
            % Reliable: 2
            
            arguments
                Obj
                New logical        = false;
                Args.Wait          = 1;
                Args.Timeout       = 5;
            end
            SEC_IN_DAY = 86400;
            
            Found = false;
            if DS9.isOpen && ~New
                % do nothing - already open
                fprintf('ds9 is already open - Using existing window\n');
                fprintf('   To open a new ds9 window use open(Obj, true)\n');
            else
                [~,ListOld]   = Obj.getAllWindows;
                [Status, Res] = system('ds9&');
                Tstart = now;
                Found  = false;
                while ~Found && ((now-Tstart).*SEC_IN_DAY)<Args.Timeout
                    pause(Args.Wait);
                    [~,ListNew]   = Obj.getAllWindows;
                    if numel(ListOld) == (numel(ListNew)-1)
                        % identify new window
                        Diff = setdiff(ListNew, ListOld);
                        if numel(Diff)==1
                            Obj.MethodXPA = Diff{1};
                            Found         = true;
                        else
                            error('Number of different windows is not 1');
                        end
                    else
                        Obj.MethodXPA = [];
                    end
                end
                
                List = [];
                Tstart = now;
                while isempty(List) && ((now-Tstart).*SEC_IN_DAY)<Args.Timeout
                    pause(Args.Wait);
                    List = Obj.selectWindow(Inf);
                end
                
                if (Status~=0)
                    warning('Can not open ds9');
                end
                Obj.mode('region');
            end
            
        end
        
        function Result = mode(Obj, Mode)
            % Set ds9 mode
            % Input  : - A DS9 object.
            %          - ds9 mode:
            %            Options are:
            %            none|region|crosshair|colorbar|pan|zoom|rotate|catalog|examine.
            %            If 'show' then display options.
            %            If empty, then return mode state.
            %            If Inf show a message with all possible modes.
            %            Default is 'region'.
            % Output : - If mode is [], then will return the mode state.
            % Author : Eran Ofek (May 2022)
            
            arguments
                Obj
                Mode = 'region';
            end
            
            if strcmpi(Mode,'show')
                fprintf('Mode options: none|region|crosshair|colorbar|pan|zoom|rotate|catalog|examine\n');
            end
            
            Result = [];
            if isempty(Mode)
                % get current Mode
                Str    = Obj.xpaget('mode');
                Result = regexprep(Str,'\n','');
            else
                if isinf(Mode)
                    % show all possible modes
                    fprintf('Possible modes: [none|region|crosshair|colorbar|pan|zoom|rotate|catalog|examine|3d]\n');
                else
                    Obj.xpaset('mode %s',Mode);
                end
            end
        end
        
        function exit(Obj, Id)
            % exit ds9
            % Input  : - A DS9 object.
            %          - A MethodXPA name (ds9 window name).
            %            If empty, use current active window.
            %            Default is [].
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9; D.exit;
            
            arguments
                Obj
                Id    = [];
            end
            
            if isempty(Id)
                % use current MethodXPA
                Id = Obj.MethodXPA;
            else
                Obj.MethodXPA = Id;
            end
            
            Obj = Obj.deleteAI([], 'all', Obj.MethodXPA);
            
            % check if Id exist
            [Exist, ~] = isWindowExist(Obj, true);
            
            %DS9.system('xpaset -p ds9 exit');
            if Exist
                Obj.xpaset('exit');
            end
            
            pause(1);
            Obj.selectWindow(Inf)
              
            
        end
        
    end
    
    methods  % frame related methods
        function Result = frame(Obj, Frame)
            % Set (xpaset) the current ds9 window frame number
            % Input  : - A DS9 object.
            %          - A string or a number to pass to the xpaset after
            %            the frame command. I.e., will execute:
            %            xpaset -p <WindowName> frame <input arg>
            %            If empty, then will return the cuurent frame
            %            number. Default is [].
            % Output : - The number of the current active frame.
            % Author : Eran Ofek (May 2022)
            % Ref    : http://ds9.si.edu/doc/ref/xpa.html#frame
            % Example: D = DS9;
            %          R = D.frame      % only return current frame number
            %          R = D.frame(2)   % goto frame 2
            %          R = D.frame('center') % center current frame
            %          R = D.frame('clear') % clear current frame
            %          R = D.frame('delete') % delete current frame
            %          R = D.frame('new') % create new frame
            %          R = D.frame('new rgb') % create new rgb frame
            %          R = D.frame('reset') % reset current frame
            %          R = D.frame('refresh') % refresh current frame
            %          R = D.frame('hide') % hide current frame
            %          R = D.frame('first') % goto first frame
            %          R = D.frame('prev') % goto previous frame
            %          R = D.frame('last') % goto last frame
            %          R = D.frame('next') % goto next frame
            %          R = D.frame('match wcs') % 
            %          R = D.frame('lock wcs') % 
                        
            arguments
                Obj
                Frame   = [];
            end
            
            if ~isempty(Frame)
                if ischar(Frame)
                    String = sprintf('frame %s',Frame);
                elseif isnumeric(Frame)
                    String = sprintf('frame %d',Frame);
                else
                    error('Unknown Frame type input - must be numeric or char array');
                end
                Obj.xpaset(String);
            end
            
            % get current frame number
            OutStr = Obj.xpaget('frame');
            Result = DS9.parseOutput(OutStr, 'num');
                                     
        end
        
        function [N, ID, ActiveID] = nframe(Obj)
            % Return the number of available frames in current ds9 window
            % Input  : - A DS9 object.
            % Output : - Number of frames.
            %          - Vector of IDs of all frames
            %          - Vector of IDs of all active frames
            % Author : Eran Ofek (May 2022)
            % See also: frame method.
            % Example: D = DS9; [R,ID,AID] = D.nframe
            
            OutStr = Obj.xpaget('frame all');
            ID     = DS9.parseOutput(OutStr, 'num');
            N      = numel(ID);
            
            if nargout>2
                OutStr = Obj.xpaget('frame active');
                ActiveID     = DS9.parseOutput(OutStr, 'num');
            end
            
        end
        
        function clearFrame(Obj, ID)
            % clear frame, by ID or curreny
            %   After the clear is done then will return to the starting point frame
            % Input  : - A DS9 object.
            %          - A vector of frame numbers to clear.
            %            If empty, clear current frame.
            %            If Inf or char array, then clear all frames.
            %            Default is [].
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9; D.clearFrame;
            %          D.clearFrame(2);
            %          D.clearFrame('all');
            
            arguments
                Obj
                ID     = [];
            end
            
            if ~isempty(ID)
                % current ID
                CurID = Obj.frame;

                if ischar(ID) || isinf(ID)
                    % clear all
                    [~,ID] = Obj.nframe;
                end
                
                Nid = numel(ID);
                % move to frame to clear
                for I=1:1:Nid
                    Obj.frame(ID(I));
                    Obj.frame('clear');
                end
                
                % return to original frame
                Obj.frame(CurID);
            else
                Obj.frame('clear');
            end
        end
        
        function deleteFrame(Obj, ID)
            % delete frame, by ID or curreny
            %   After the clear is done then will NOT return to the starting point frame
            % Input  : - A DS9 object.
            %          - A vector of frame numbers to delete.
            %            If empty, clear current frame.
            %            If Inf or char array, then delete all frames.
            %            Default is [].
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9; D.deleteFrame;
            %          D.deleteFrame(2);
            %          D.deleteFrame('all');
            
            arguments
                Obj
                ID     = [];
            end
            
            if ~isempty(ID)
                if ischar(ID) || isinf(ID)
                    % clear all
                    [~,ID] = Obj.nframe;
                end
                
                Nid = numel(ID);
                % move to frame to clear
                for I=1:1:Nid
                    Obj.frame(ID(I));
                    Obj.frame('delete');
                end
            else
                Obj.frame('delete');
            end
           
        end
    end
    
    methods  % load and display images
        
        function url(Obj, URL, Frame)
            % Display FITS files in URL links
            % Input  : - A DS9 object.
            %          - A char array containing a URL link or a cell array
            %            of URLs.
            %          - Frame number or a vector of frame numbers in which
            %            to load the images. If empty, open a new frame.
            %            Default is [].
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.url(URL);
            
            arguments
                Obj
                URL
                Frame            = [];
            end
           
            if ischar(URL)
                URL = {URL};
            end
            Nurl = numel(URL);
            
            if isempty(Frame)
                % open a new frame
                Frame = Obj.frame('new');
            end
                
            if numel(Frame)==1 && Nurl>1
                Frame = (Frame:1:Frame+Nurl-1);
            end
            
            for Iurl=1:1:Nurl
                Obj.frame(Frame(Iurl));
                Obj.xpaset('url %s',URL{Iurl});
            end
            
        end
        
        function load(Obj, Image, Frame, Args)
            % Load an image into ds9 display.
            %   Also optionaly populate the InfoAI property.
            % Input  : - A DS9 object.
            %          - An image, or images. One of the following:
            %            1. An AstroImage array.
            %            2. A file name with wild cards.
            %            3. A cell array of file names.
            %            4. A matrix or a cell array of matrices.
            %          - Array of frame numbers in which to display the
            %            images. If scalar will build the array starting
            %            with the provided number (steps of 1).
            %            If Inf, open a new frame.
            %            If empty, use the current frame.
            %            Default is [].
            %          * ...,key,val,...
            %            'UseRegExp' - A logical indicating if to use
            %                   regular expressions when interpreting a
            %                   singel file name. If false, will use only
            %                   wild cards. Default is false.
            %            'DataProp' - A data property in the AstroImage
            %                   from which to read the image.
            %                   Options are: 'Image', 'Back', 'Var', 'Mask', 'PSF.
            %                   Default is 'Image'.
            %            'PopAI' - A logical indicating if to populate the
            %                   InfoAI property. If true, then an
            %                   AstroImage containing the images will be
            %                   loaded into the InfoAI property.
            %                   Default is true.
            %            'Scale' - set scale. If empty do nothing.
            %                   Default is 'zscale'.
            %            'Colorbar' - set colorbar. Default is false.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D=DS9; D.disp(rand(10,10));
            
            arguments
                Obj
                Image                           % Image, matrix, AstroImage\
                Frame                    = [];  % if empty use current frame, or 1 if not exist, Inf for new frame
                Args.UseRegExp logical   = false;
                Args.PopAI logical       = true;
                Args.DataProp            = 'Image';
                Args.Scale               = 'zscale';
                Args.Colorbar logical    = false;
            end
            
            if ischar(Image)
                Image = {Image};
            end
           
            if iscellstr(Image)
                FileName = Image;
                IsURL    = www.isURL(FileName);
            else
                FileName = {};
                IsURL    = false;
            end
            
            if Args.PopAI
                [ImageName, AI] = DS9.loadPrep(Image, 'UseRegExp', Args.UseRegExp,...
                                                       'DataProp',Args.DataProp,...
                                                       'FileName',FileName);
            else
                [ImageName] = DS9.loadPrep(Image, 'UseRegExp', Args.UseRegExp,...
                                                       'DataProp',Args.DataProp,...
                                                       'FileName',FileName);
                AI = [];
            end
            
            Nim = numel(ImageName);
            if isempty(Frame)
                % use the current frame
                Frame = Obj.frame;
            else
                if isinf(Frame)
                    Frame = Obj.frame('new');
                else
                    Frame = Obj.frame(Frame);
                end
            end
                
            if numel(Frame)==1 && Nim>1
                Frame = (Frame:1:Frame+Nim-1);
            end
            
            if numel(Frame)>1 && Nim~=numel(Frame)
                error('Number if frames must be one or equal to the number of images');
            end
            
            if ~isempty(AI)
                Obj = addAI(Obj, AI, ImageName, Frame);
            end
            
            for Iim=1:1:Nim
                Obj.frame(Frame(Iim));
                Obj.xpaset('fits %s', ImageName{Iim});
            end
            if ~isempty(Args.Scale)
                Obj.scale(Args.Scale);
            end
            Obj.colorbar(Args.Colorbar);
        end
        
    end
    
    methods  % read image from ds9
        function FileName = save(Obj, FileName, Args)
            % Save current ds9 frame to FITS file
            % Input  : - A DS9 object.
            %          - A file name. Default is to generate a file name 
            %            using tempname.
            %          * ...,key,val,...
            %            'Type' - Image type to save - one of the following options:
            %                   [fits|rgbimage|rgbcube|mecube|mosaic|mosaicimage].
            %                   [eps|gif|tiff|jpeg|png]
            %                   Default is 'fits'.
            %            'Jquality' - jpeg quality. Default is 75.
            % Output : - Saved file name.
            % Author : Eran Ofek (May 2022)
            % Example: D=DS9(rand(100,100)); FN = D.save; delete(FN)
            
            arguments
                Obj
                FileName         = tempname;
                Args.Type        = 'fits';
                Args.Jquality    = 75;
            end
            
            switch lower(Args.Type)
                case {'fits','rgbimage','rgbcube','mecube','mosaic','mosaicimage'}
                    Obj.xpaset('save %s %s', Args.Type, FileName);
                case {'eps','gif','tiff','png'}
                    Obj.xpaset('saveimage %s %s', Args.Type, FileName);
                case {'jpeg','jpg'}
                    Obj.xpaset('saveimage jpeg %s %d', FileName, Args.Juality);
                otherwise
                    error('Unknown Type option');
            end
        end
    end
    
    methods % zoom, pan, rotate, ...
        function zoom(Obj, Zoom, All)
            % Apply zoom to ds9 frame
            % Input  : - A DS9 object.
            %          - Zoom level:
            %            Use positve numbers to use absolute zoom level.
            %            Use negative numbers to use zoom relative to current zoom level.
            %            Use NaN for .zoom to fit'.
            %            Use string for a zoom string - e.g., 'to fit', 'in', 'out', 'open', 'close'.
            %          - Either a vector of frame indices on which to apply
            %            the zoom level (the same zoom for all frames in
            %            the current ds9 window),
            %            or a logical indicatig if to apply the zoom to all
            %            frames (true), or only the current frame (false).
            %            Default is false.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100),1);
            %          D.load(rand(100,100),2);
            %          D.zoom(5)
            %          D.zoom(-0.5)
            %          D.zoom(4,true)
            %          D.zoom   % zoom to fit
            %          D.zoom('to 5')
            %          D.zoom('out',2)
            
            arguments
                Obj
                Zoom        = [];
                All         = false;   % or vector of numbers
            end
            
            if isempty(Zoom)
                % zoom to fit
                Obj.xpasetFrame(All, 'zoom to fit');
            else
                if isnumeric(Zoom)
                    if Zoom>0
                        % absolute zoom "to zoom"
                        if numel(Zoom)==1
                            Obj.xpasetFrame(All, 'zoom to %f', Zoom);
                        else
                            Obj.xpasetFrame(All, 'zoom to %f %f', Zoom);
                        end
                    else
                        % relative zoom
                        if numel(Zoom)==1
                            Obj.xpasetFrame(All, 'zoom %f', abs(Zoom));
                        else
                            Obj.xpasetFrame(All, 'zoom %f %f', abs(Zoom));
                        end
                    end
                elseif ischar(Zoom) || isstring(Zoom)
                    Obj.xpasetFrame(All, 'zoom %s', Zoom);
                else
                    error('Unkown zoom option');
                end
            end
        end
        
        function [Limits,ScaleType] = scale(Obj, Val, Frame, Args)
            % Set the scale limits and function for ds9 frames
            % Input  : - A DS9 object.
            %          - Either a vector of [lower upper] limit of the scaling in
            %            units of the image pixel values, or a string
            %            containing some specific scale xpa/ds9 command
            %            (see examples).
            %            If the 'IsQuantile' argument is true, then the
            %            limits will be interpreted as quantiles.
            %            If empty, only return arguments (do not use
            %            xpaset). Default is [].
            %          - Either a vector of frame indices on which to apply
            %            the scale (the same scale or quantile for all frames in
            %            the current ds9 window),
            %            or a logical indicatig if to apply the zoom to all
            %            frames (true), or only the current frame (false).
            %            If empty, apply to all frames.
            %            Default is [].
            %          * ...,key,val,...
            %            'IsQuantile' - A logical indicating if to
            %                   interpret the limits as quantiles.
            %                   The quantiles will be translated to limits
            %                   using the images stored in the InfoAI
            %                   property.
            %                   Default is false.
            % Output : - Return a two elements vector of limits for the current frame only.
            %          - Scale type (e.g., 'linear').
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100));
            %          [L,ST] = D.scale;
            %          D.scale([0 0.5])
           
            arguments
                Obj
                Val                        = [];
                Frame                      = [];
                Args.IsQuantile logical    = false;
                Args.Window                = [];
            end
            
            if ~isempty(Val)
                if Args.IsQuantile
                    AI = Obj.getAI(Args.Frame, Args.Window);
                    Q1 = imProc.stat.quantile(Val(1));
                    Q2 = imProc.stat.quantile(Val(2));
                    Val = [Q1(:), Q2(:)];
                end

                if isnumeric(Val)
                    % Val containing limits
                    switch numel(Val)
                        case 2
                            % [lower upper] limits
                            String = sprintf('limits %f %f',Val);
                        otherwise
                            error('scale 2nd argument must contain 1 or 2 elements');
                    end
                elseif ischar(Val) || isstring(Val)
                    String = Val;
                else
                    error('Unknown scale value option');
                end
                Obj.xpasetFrame(Frame, 'scale %s', String);
            end
            
            StrLimits = Obj.xpaget('scale limits');
            %Limits    = split(StrLimits, ' ');
            %Limits    = str2double(Limits);
            %Limits    = Limits(:).';
            Limits    = DS9.parseOutput(StrLimits, 'num');
            
            if nargout>1
                ScaleType = Obj.xpaget('scale');
            end
        end
        
        function cmapInvert(Obj, Val, Frame)
            % invert colormap of frames in a ds9 window
            % Input  : - A DS9 object.
            %          - A logical indicating if invert is yes (true)
            %            or no (false). Default is true.
            %          - Either a vector of frame indices on which to apply
            %            the scale (the same scale or quantile for all frames in
            %            the current ds9 window),
            %            or a logical indicatig if to apply the zoom to all
            %            frames (true), or only the current frame (false).
            %            If empty, apply to all frames.
            %            Default is [].
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D=DS9(rand(100,100));
            %          D.cmapInvert
            
            arguments
                Obj
                Val logical     = true;
                Frame           = [];
            end
            
            if Val
                Invert = 'yes';
            else
                Invert = 'no';
            end
            Obj.xpasetFrame(Frame, 'cmap invert %s', Invert)
            
        end
        
        function colorbar(Obj, Val, Frame)
            % Control ds9 window colorbar
            % Input  : - A DS9 object.
            %          - Either a logical indicating if to add (true) or
            %            remove (false) the color bar, or a string that
            %            will be appended after the colorbar command (e.g.,
            %            'fontsize 14').
            %            Default is false.
            %          - Either a vector of frame indices on which to apply
            %            the scale (the same scale or quantile for all frames in
            %            the current ds9 window),
            %            or a logical indicatig if to apply the zoom to all
            %            frames (true), or only the current frame (false).
            %            If empty, apply to all frames.
            %            Default is [].
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(200,200));
            %          D.colorbar
            
            arguments
                Obj
                Val       = false;
                Frame     = [];
            end
            
            if islogical(Val)
                if Val
                    Command = 'yes';
                else
                    Command = 'no';
                end
            else
                Command = Val;
            end
            Command = sprintf('colorbar %s',Command);
            Obj.xpasetFrame(Frame, Command);
            
        end
        
        % orient
        % rotate to
        % header
        
        function [CurXY, CurCoo] = pan(Obj, Val, CooSys)
            % Controls the current image cursor location for the current frame
            % Input  : - A DS9 object.
            %          - A two element vector of cursor absolute position [X Y] or
            %            [Long Lat] (in deg) coordinates.
            %          - Coordinate system: 'image' | 'fk4' | 'icrs'.
            %            'image' is for x and y pixel position.
            %            Default is 'image'.
            % Output : - [X, Y] pixel image position of cursor.
            %          - [RA Dec] in deg for cursor position.
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100));
            %          [a,b]=D.pan
            %          [a,b]=D.pan([100 100])
            %          D.pan('to 100 100'); % another way to control absolute position
            %          D.pan('100 100'); % relatuve position
           
            arguments
                Obj
                Val      = [];
                CooSys   = 'image';
            end
            
            if ~isempty(Val)
                Obj.xpaset('pan to %f %f %s',Val(1), Val(2), CooSys);
            end
            
            if nargout>0
                CurXY = Obj.xpaget('pan');
                CurXY = DS9.parseOutput(CurXY, 'num');
                if nargout>1
                    CurCoo = Obj.xpaget('pan wcs icrs');
                    CurCoo = DS9.parseOutput(CurCoo, 'num');
                end
            end
            
        end
    end
    
    methods   % printing
        function print(Obj, FileName,Resolution,Color,Level)
            % Print current frame into a postscript file in current directory
            % Input  : - PS file name.
            %          - Resolution. Default is 150.
            %          - Color. Default is 'cmyk'.
            %          - PS level. Default is 2.
            % Output : null
            % Author : Eran Ofek (May 2022)

            arguments
                Obj
                FileName
                Resolution  = 150;
                Color       = 'cmyk';
                Level       = 2;
            end
            FileName = sprintf('%s%s%s',pwd,filesep,FileName);  % to print in current directory
            Obj.xpaset('print destination file');
            Obj.xpaset('print filename %s',FileName);
            Obj.xpaset('print resolution %d',Resolution);
            Obj.xpaset('print color %s',Color);
            Obj.xpaset('print level %d',Level);
            Obj.xpaset('print');
        end
        
    end
    
    methods (Static)  % region files (Static): regionWrite
        % construct a region file
        function FileName=regionWrite(Cat, Args)
            % Write a regions file for a list of coordinates and properties
            % Description: Write a regions file for a list of coordinates
            %              and properties.
            % Input  : - A two column matrix of coordinates [X,Y] or
            %            [RA,Dec] (degrees), or an
            %            AstroCatalog or AstCat object.
            %            The AstCat object coordinates column names are
            %            defined by the 'ColName...' argumenents.
            %            For RA/Dec units must be degrees.
            %            NaN positions will not be displayed.
            %          * Arbitrary number of ...,key,val,... pairs.
            %            The following keywords are available:
            %            'FileName' - Output region file name.
            %                         Default is tempname.
            %            'Append'   - Append region file to an existing
            %                         region file. Default is false.
            %            'Coo'      - Coordinates type: 'image'|'fk5'.
            %                         Default is 'image' (i.e., pixels).
            %            'Units'    - If 'Coo' is 'fk5' than this specify
            %                         if the input coordinates are in 'deg'
            %                         or 'rad'. Default is 'deg'.
            %            'Marker'   - A string or a cell array of strings
            %                         of markers type.
            %                         Options are: 'circle'|'circ'|'o'
            %                                      'box'|'s'
            %                                      'ellipse'|'e'
            %                                      'line'|'l'
            %                                      'vector'|'v'
            %                                      'polygon'|'p'
            %                         Default is 'circle'.
            %             'Color'   - A string or a cell array of strings
            %                         of marker colors
            %                         ('red'|'blue'|'green'|'black'|
            %                          'white'|...).
            %                         Default is 'red'.
            %             'Width'   - A scalar or a vector of markers
            %                         width. Default is 1.
            %             'Size'    - Size array. Either one row, or a row
            %                         per coordinate. Default is 10.
            %                         Note that the required number of
            %                         columns in size depands on the marker
            %                         type.
            %                         Following attributes are needed:
            %                           For 'circle':  [Radius]
            %                           For 'box':     [Width,Height]
            %                           For 'ellipse': [Major axis, Minor axis]
            %                           For 'vector':  [Length,PA]
            %                           For 'line':    [StartX,StartY,EndX,EndY]
            %                           For 'polygon': The coordinates are
            %                           used as the polygon verteces.
            %                         Default is 10.
            %             'Text'    - Text to plot with marker.
            %                         Default is ''.
            %             'Font'    - Font type. Default is 'helvetica'.
            %             'FontSize'- Font size. Default is 16.
            %             'FontStyle'-Font style. Default is 'normal'.
            %             'ColNameX'- Cell array of possible column names
            %                         of X coordinate in AstCat object.
            %                         If the first input in an AstCat
            %                         object then the first exitsing column
            %                         name will be used.
            %                         Default is
            %                         {'X','X1','X_IMAGE','XWIN_IMAGE','X1','X_PEAK','XPEAK','x'};
            %             'ColNameY'- Like 'ColNameX', but for the Y
            %                         coordinate.
            %                         Default is
            %                         {'Y','Y1','Y_IMAGE','YWIN_IMAGE','Y1','Y_PEAK','YPEAK','y'}
            %             'ColNameRA'-Like 'ColNameX', but for the RA
            %                         coordinate.
            %                         Default is
            %                         {'RA','Mean_RA','Median_RA','ALPHA','ALPHAWIN_J2000','ALPHA_J2000','RA_J2000','RAJ2000','RightAsc'};
            %             'ColNameDec'-Like 'ColNameX', but for the Dec
            %                         coordinate.
            %                         Default is
            %                         {'Dec','DEC','Mean_Dec','Median_Dec','DELTA','DELTAWIN_J2000','DELTA_J2000','DEC_J2000','DEJ2000','Declination'};
            %             'Precision' - precision format of the coordinate output, e.g. '%15.8f' or '%8.2f'
            %             'PrintIndividualProp' -- whether to print individual object color and text after each line
            % Output : - Region file name.
            % See also: DS9.plot
            % Example: FileName=DS9.regionWrite(Cat);
            %          DS9.regionWrite([X,Y],'Marker','s','Color','cyan')
            
            arguments
                Cat
                Args.FileName        = tempname;  % use temp file name
                Args.Append          = false;
                Args.Coo             = 'image';   % 'image'|'fk5'
                Args.Units           = 'deg';     % for 'image' this is always pix!
                Args.Marker          = 'circle';  % 'circle'|'box'|...
                Args.Color           = 'red';
                Args.Width           = 1;
                Args.Size            = 10;
                Args.Text            = '';
                Args.Font            = 'helvetica';  %'helvetica 16 normal'
                Args.FontSize        = 16;
                Args.FontStyle       = 'normal';
                Args.ColNameX        = AstroCatalog.DefNamesX; %{'X','X1','X_IMAGE','XWIN_IMAGE','X1','X_PEAK','XPEAK','x'};
                Args.ColNameY        = AstroCatalog.DefNamesY;
                Args.ColNameRA       = AstroCatalog.DefNamesRA;
                Args.ColNameDec      = AstroCatalog.DefNamesDec;
                Args.Precision       = '%15.8f';
                Args.PrintIndividualProp logical = true; % whether to print individual object color and text after each line
            end
            
            % check if region file exist
            %if (exist(Args.FileName,'file')==0)
            if ~isfile(Args.FileName)
               if (Args.Append)
                   error('User requested to append region file, but file doesnt exist');
               end
            end
            
            IsXY = false;
            switch lower(Args.Coo)
             case 'image'
                CooUnits    = '';
                IsXY        = true;
                Args.Units = 'pix';
             case {'fk5','icrs','fk4'}
                CooUnits = '"';
             otherwise
                error('Coo units is not supported');
            end
            
            % prepare catalog
            if (AstCat.isastcat(Cat))
                % read the coordinates from an AstCat object
                if (IsXY)
                    ColNameX   = select_exist_colnames(Cat,Args.ColNameX(:));
                    ColNameY   = select_exist_colnames(Cat,Args.ColNameY(:));
                else
                    ColNameX   = select_exist_colnames(Cat,Args.ColNameRA(:));
                    ColNameY   = select_exist_colnames(Cat,Args.ColNameDec(:));
                end
                
                X = col_get(Cat,ColNameX);
                Y = col_get(Cat,ColNameY);
            elseif isa(Cat, 'AstroCatalog')
                if IsXY
                    [X, Y] = getXY(Cat, 'ColX', Args.ColNameX, 'ColY', Args.ColNameY);
                else
                    [X, Y] = getLonLat(Cat, 'deg', 'ColLon',Args.ColNameRA, 'ColLat',Args.ColNameDec);
                    Args.Units = 'deg';
                end
                
            elseif isa(Cat, 'AstroImage')
                if IsXY
                    [X, Y] = getXY(Cat.CatData, 'ColX', Args.ColNameX, 'ColY', Args.ColNameY);
                else
                    [X, Y] = getLonLat(Cat.CatData, 'deg', 'ColLon',Args.ColNameRA, 'ColLat',Args.ColNameDec);
                    Args.Units = 'deg';
                end
            else
                % Assume Cat is a two column matrix [X,Y]
                X = Cat(:,1);
                Y = Cat(:,2);
            end
            
            % In case of spherical coordinates - convert to deg
            if (~IsXY)
                Factor = convert.angular(Args.Units,'deg');
                X      = X.*Factor;
                Y      = Y.*Factor;
            end
                
            % remove NaN positions
            Flag = ~isnan(X) & ~isnan(Y);
            X    = X(Flag);
            Y    = Y(Flag);
            
            
            % Number of regions to plot
            Nreg    = numel(X);
            % Prep properties
            if (~iscell(Args.Marker))
                Args.Marker = {Args.Marker};
            end
            Nmarker = numel(Args.Marker);
            if (~iscell(Args.Color))
                Args.Color = {Args.Color};
            end
            Ncolor = numel(Args.Color);
            Nwidth = numel(Args.Width);
            Nsize  = size(Args.Size,1);
            if (~iscell(Args.Text))
                Args.Text = {Args.Text};
            end
            Ntext = numel(Args.Text);
            if (~iscell(Args.Font))
                Args.Font = {Args.Font};
            end
            Nfont = numel(Args.Font);
            Nfontsize = numel(Args.FontSize);
            if (~iscell(Args.FontStyle))
                Args.FontStyle = {Args.FontStyle};
            end
            Nfontstyle = numel(Args.FontStyle);

            FmtString = sprintf('global color=%%s dashlist=8 3 width=%%d font="%%s %%d %%s" select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 delete=1 include=1 source=1\n');

            % Open file
            if (Args.Append)
                % append header to an existing region file
                FID = fopen(Args.FileName,'a');
            else
                FID = fopen(Args.FileName,'w');
                fprintf(FID,'# Region file format: DS9 version 4.1\n');
                fprintf(FID,'# Written by Eran Ofek via ds9.write_region.m\n');
%                 fprintf(FID,'global color=green dashlist=8 3 width=1 font="helvetica 10 normal" select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 delete=1 include=1 source=1\n');
                fprintf(FID,FmtString, Args.Color{1}, Args.Width(1), Args.Font{1}, Args.FontSize(1), Args.FontStyle{1});
                fprintf(FID,'%s\n',Args.Coo);
            end

            FmtStringCircle = sprintf('%%s(%s,%s,%s%%s)',Args.Precision,Args.Precision,Args.Precision);
            FmtStringBox = sprintf('%%s(%s,%s,%s%%s,%s%%s,%%9.5f)',Args.Precision,Args.Precision,Args.Precision,Args.Precision);

            % for each coordinate (refion)
            for Ireg=1:1:Nreg
                
                switch lower(Args.Marker{min(Ireg,Nmarker)})
                    case {'circle','circ','o'}
                        fprintf(FID, FmtStringCircle,...
                                    'circle', X(Ireg), Y(Ireg),...
                                    Args.Size(min(Ireg,Nsize)),CooUnits);
                    case {'box','b','s'}
                        if (numel(Args.Size)==1)
                            fprintf(FID,FmtStringBox,...
                                        'box',X(Ireg),Y(Ireg),...
                                        Args.Size(min(Ireg,Nsize),1),CooUnits,...
                                        Args.Size(min(Ireg,Nsize),1),CooUnits,...
                                        0);
                        else
                            fprintf(FID,FmtStringBox,...
                                        'box',X(Ireg),Y(Ireg),...
                                        Args.Size(min(Ireg,Nsize),1),CooUnits,...
                                        Args.Size(min(Ireg,Nsize),2),CooUnits,...
                                        Args.Size(min(Ireg,Nsize),3));
                        end
                    case {'ellipse','e'}
                        fprintf(FID,FmtStringBox,...
                                    'ellipse',X(Ireg),Y(Ireg),...
                                    Args.Size(min(Ireg,Nsize),1),CooUnits,...
                                    Args.Size(min(Ireg,Nsize),2),CooUnits,...
                                    Args.Size(min(Ireg,Nsize),3));
                    case {'vector','v'}
                        fprintf(FID,'# %s(%15.8f,%15.8f,%15.8f%s,%9.5f)',...
                                    'vector',X(Ireg),Y(Ireg),...
                                    Args.Size(min(Ireg,Nsize),1),CooUnits,...
                                    Args.Size(min(Ireg,Nsize),2));
    
                    case {'line','l'}
                        fprintf(FID,'%s(%15.8f,%15.8f,%15.8f,%15.8f)',...
                                    'line',...
                                    Args.Size(min(Ireg,Nsize),1),...
                                    Args.Size(min(Ireg,Nsize),2),...
                                    Args.Size(min(Ireg,Nsize),3),...
                                    Args.Size(min(Ireg,Nsize),4));
                                
                    case {'polygon','p'}
                        if (Ireg==1)
                            fprintf(FID,'%s(','polygon');
                        end
                        fprintf(FID,'%15.8f,%15.8f,',X(Ireg),Y(Ireg)); %Par.Size(Ireg,Isize));
                        if (Ireg==Nreg)
                            fprintf(FID,'%15.8f,%15.8f)',X(Ireg),Y(Ireg)); %Par.Size(Ireg,Nsize));
                        end
                    otherwise
                        error('Unknown Marker option');
                end
                
                % additional properties
                if Args.PrintIndividualProp
                    fprintf(FID,'# color=%s width=%d font="%s %d %s" text={%s}\n',...
                                Args.Color{min(Ireg,Ncolor)},...
                                Args.Width(min(Ireg,Nwidth)),...
                                Args.Font{min(Ireg,Nfont)},...
                                Args.FontSize(min(Ireg,Nfontsize)),...
                                Args.FontStyle{min(Ireg,Nfontstyle)},...
                                Args.Text{min(Ireg,Ntext)});
                else
                    fprintf(FID,'\n');
                end
                        
            end
            fclose(FID); % close region file

            FileName = Args.FileName;
            pause(0.2);
            
        end
    end
    
    methods % region files: regionLoad, regionDelete, regionSave
        % load regions from file
        function FileName = regionLoad(Obj, FileName)
            % load regions file name into current ds9 frame
            % Description: load regions file name into current ds9 frame.
            % Input  : - A DS9 object.
            %          - FileName. If file name does not contains the
            %            full path then assume the file is in the current
            %            directory.
            % Output : Full File name used.
            % Author : Eran Ofek (Jan 2011)
            % Example: D.regionLoad('A.reg')
            
            % check if FileName contains directory name
            if ~contains(FileName,filesep)
                % File Name without dir
                % add pwd to file name
                FileName = sprintf('%s%s%s',pwd,filesep,FileName);
            end
            
            Obj.xpaset('regions load %s',FileName);
        end
        
        % delete regions from frame
        function regionDelete(Obj)
            % Delete all regions from current ds9 frame
            % Description: Delete all regions from ds9 frame
            % Input  : - A DS9 object.
            % Output : null
            % Example: D.regionDelete
            % Reliable: 2
            
            Obj.xpaset('regions delete all');
        end
        
        % save regions to file
        function regionSave(Obj, FileName)
            % Save regions in current ds9 frame into a file
            % Input  : - A DS9 object.
            %          - FileName. If file name does not contains the
            %            full path then save it in the current
            %            directory. Default is 'ds9.reg'.
            % Output : null
            % Author : Eran Ofek (Jan 2011)
            % Example: D.regionSave
            
            arguments
                Obj
                FileName = 'ds9.reg';
            end
      
            % check if FileName contains directory name
            if ~contains(FileName,filesep)
                % File Name without dir
                % add pwd to file name
                FileName = sprintf('%s%s%s',pwd,filesep,FileName);
            end
            Obj.xpaset('regions save %s',FileName);
        end
    end
        
    methods  % plot catalogs 
        function FileName = plotc(Obj,varargin)
            % Generate and plot a region file from a list of celestial coordinates [RA, Dec]
            % Input  : * see Obj.plot(...,'Coo','fk5')
            % Output : - A region file name.
            % Author : Eran Ofek (May 2022)
            
            FileName = Obj.plot(varargin{:},'Coo','fk5');
        end
        
        % plot regions
        function FileName = plot(Obj, X, Y, ColorSymbol, Args)
            % Generate and plot a region file from a list of coordinates
            % Package: @ds9
            % Description: Generate and plot a region file from a list of
            %              coordinates.
            %              This is like the function ds9.write_region with
            %              the exception that it also load the region file
            %              into the current frame.
            %              By default this program deletes the region file
            %              name after plotting. Use ds9.write_region to
            %              create the file.
            % Input  : * Parameters to pass to ds9.write_region.m.
            %            See ds9.write_region.m for details.
            %            Alternatively, the second argument may be a string
            %            containing the marker and color
            %            (e.g., 'yo' - yellow circle; 'ks' - black square).
            %            Alternatively, the first argument is X coordinate,
            %            the second is Y coordinate and the rest as before.
            %            Will remove NaN positions.
            % Output : null
            % Example: D = DS9(rand(100,100));
            %          D.plot(X, Y, 'ro')
            %          D.plot([X, Y],[] 'ro')
            %          D.plot(RA, Dec, 'ro', 'Coo','icrs')
            %          D.plotc(RA, Dec, 'ro')
            %          D.plot(AC, {'X','Y'}, 'ro')
            %          D.plot(AC, {}, 'ro')
            %          D.plot(AC, {'RA','Dec'}, 'ro', 'Coo','icrs')
            %          D.plot(AC, {}, 'ro', 'Coo','icrs')
            
            arguments 
                Obj
                X
                Y                    = [];
                ColorSymbol          = 'ro';  % color, symbol
                
                Args.Coo             = 'image';  % 'image' | 'icrs'
                Args.Size            = [];  % default is [10 10] or [10 10 0]
                Args.FileName        = tempname;  % use temp file name
                Args.Append          = false;
                Args.Units           = 'deg';     % for 'image' this is always pix!
                Args.Marker          = 'circle';  % 'circle'|'box'|...
                Args.Color           = 'red';
                Args.Width           = 1;
                Args.Text            = '';
                Args.Font            = 'helvetica';  %'helvetica 16 normal'
                Args.FontSize        = 16;
                Args.FontStyle       = 'normal';
                Args.ColNameX        = AstroCatalog.DefNamesX; %{'X','X1','X_IMAGE','XWIN_IMAGE','X1','X_PEAK','XPEAK','x'};
                Args.ColNameY        = AstroCatalog.DefNamesY;
                Args.ColNameRA       = AstroCatalog.DefNamesRA;
                Args.ColNameDec      = AstroCatalog.DefNamesDec;
                
                Args.DeleteFile logical = true;
            end
                        
            if isnumeric(X)
                if isempty(Y)
                    if size(X,2)<2
                        error('If second input argument is empty, then first input must be an AstroCatalog, or a matrix with at least two columns');
                    end
                    Y = X(:,2);
                    X = X(:,1);
                end
                Cat = [X Y];
            else
                Cat = X;
                if isempty(Y)
                    % use Args default
                else
                    switch lower(Args.Coo)
                        case 'image'
                            Args.ColNameX = Y{1};
                            Args.ColNameY = Y{2};
                        otherwise
                            Args.ColNameRA  = Y{1};
                            Args.ColNameDec = Y{2};
                    end
                end
            end
                
            if ~isempty(ColorSymbol)
                [~,Args.Marker,Args.Color]=DS9.parseColorSymbol(ColorSymbol);
                if isempty(Args.Size)
                    switch Args.Marker
                        case 'o'
                            Args.Size       = [10 10];
                            Args.Marker     = 'circle';
                        case 's'
                            Args.Size       = [10 10 0];
                            Args.Marker     = 'box';
                        otherwise
                            % do nothing
                    end
                end
            end
                
            DS9.regionWrite(Cat, 'Coo',Args.Coo,...
                                 'Size',Args.Size,...
                                 'Marker',Args.Marker,...
                                 'Color',Args.Color,...
                                 'FileName',Args.FileName,...
                                 'Append',false,...
                                 'Units',Args.Units,...
                                 'Width',Args.Width,...
                                 'Text',Args.Text,...
                                 'Font',Args.Font,...
                                 'FontSize',Args.FontSize,...
                                 'FontStyle',Args.FontStyle,...
                                 'ColNameX',Args.ColNameX,...
                                 'ColNameY',Args.ColNameY,...
                                 'ColNameRA',Args.ColNameRA,...
                                 'ColNameDec',Args.ColNameDec);
                    
            Obj.regionLoad(Args.FileName);
            if Args.DeleteFile
                delete(Args.FileName);
                FileName = [];
            else
                FileName = Args.FileName;
            end
            
        end
   
        % plot line/polygon by x/y coordinates
        function FileName = plotLine(Obj, X, Y, ColorSymbol, Args)
            % Plot a line, broken line, or a polygon
            % Description: Plot a broken line (curve) in ds9 image.
            % Input  : - A DS9 object.
            %          - Vector of X points of the line verteces.
            %          - Vector of Y points of the line verteces.
            %          - Color and symbol string. Symbol ignored.
            %            Default is 'r-'.
            %            'FileName' - Output region file name.
            %                         Default is tempname.
            %            'Append'   - Append region file to an existing
            %                         region file. Default is false.
            %            'Coo'      - Coordinates type: 'image'|'fk5'.
            %                         Default is 'image' (i.e., pixels).
            %            'Units'    - If 'Coo' is 'fk5' than this specify
            %                         if the input coordinates are in 'deg'
            %                         or 'rad'. Default is 'deg'.
            %             'Width'   - A scalar or a vector of markers
            %                         width. Default is 1.
            %             'Text'    - Text to plot with marker.
            %                         Default is ''.
            %             'Font'    - Font type. Default is 'helvetica'.
            %             'FontSize'- Font size. Default is 16.
            %             'FontStyle'-Font style. Default is 'normal'.
            %             'DeleteFile' - A logical indicating if to delete
            %                   region file after useage.
            %                   Default is true.
            % Output : - Region file path and name.
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100))
            %          D.plotLine([1 10],[1 20])
            %          D.plotLine([1 20],[1 50],'g-','Width',3)
            
            arguments
                Obj
                X
                Y
                ColorSymbol                 = 'r-';
                
                Args.FileName        = tempname;  % use temp file name
                Args.Append          = false;
                Args.Coo             = 'image';   % 'image'|'fk5'
                Args.Units           = 'deg';     % for 'image' this is always pix!
                Args.Width           = 1;
                Args.Text            = '';
                Args.Font            = 'helvetica';  %'helvetica 16 normal'
                Args.FontSize        = 16;
                Args.FontStyle       = 'normal';
                
                Args.DeleteFile logical     = true;
            end
                            
            [~,~,Color] = DS9.parseColorSymbol(ColorSymbol);
              
            X1 = X(1:end-1);
            Y1 = Y(1:end-1);
            X2 = X(2:end);
            Y2 = Y(2:end);
            
            FileName = DS9.regionWrite([X1(:), Y1(:)],'Color',Color,'Marker','line','Size',[X1(:), Y1(:), X2(:), Y2(:)],...
                                                      'FileName',Args.FileName,...
                                                      'Append',Args.Append,...
                                                      'Coo',Args.Coo,...
                                                      'Units',Args.Units,...
                                                      'Width',Args.Width,...
                                                      'Text',Args.Text,...
                                                      'Font',Args.Font,...
                                                      'FontSize',Args.FontSize,...
                                                      'FontStyle',Args.FontStyle);
                                                      
            Obj.regionLoad(FileName);
            if Args.DeleteFile
                delete(FileName);
                FileName = [];
            end
            
        end
       
        function plotLineSlope(Obj, X,Y,Theta,Length,varargin)
            % Plot multiple lines based on X,Y,theta,length
            % Description: Plot multiple lines based on X,Y,length,theta
            % Input  : - Vector of X starting points.
            %            Each one of the first 4 input args may be a scalar
            %            or a vector.
            %          - Vector of Y starting points.
            %          - Vector of line angles [deg] measured from X-axis.
            %          - Vector of line lengths.
            %          * Additional parameters to pass to DS9/regionWrite
            %            function. The first argument may be a plot-like
            %            color indicator.
            % Output : - FileName for the region file.
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100));
            %          D.plotLineSlope(10,10,100,100,'r-','Width',4)
            %          D.plotLineSlope(10,10,[0;10;20],100,'r-','Width',4)
            
            Xend = X + Length.*cosd(Theta);
            Yend = Y + Length.*sind(Theta);
                        
            NXe = numel(Xend);
            NYe = numel(Yend);
            NX  = numel(X);
            NY  = numel(Y);
            NT  = numel(Theta);
            NL  = numel(Length);
            
            N = max([NX, NY, NT, NL]);
            for I=1:1:N
                % for each line
                Ix  = min(I,NX);
                Iy  = min(I,NY);
                Ixe = min(I,NXe);
                Iye = min(I,NYe);
                Obj.plotLine([X(Ix), Xend(Ixe)],[Y(Iy), Yend(Iye)],varargin{:});
            end
        end
        
        % plot text
        function  plotText(Obj,X,Y,Text,Args)
            % plot text to ds9 current frame
            % Package: @ds9
            % Description: plot text to ds9 current frame in image
            %              coordinates position.
            % Input  : - Image coordinate X position (pixels), or J2000.0
            %            R.A. (deg or sexagesimal string). If string then
            %            will set 'Coo' to 'fk5' and assumes that also Dec
            %            is given in sexagesimal format.
            %          - Image coordinate Y position (pixels), or J2000.0
            %            Dec. (deg or sexagesimal string).
            %          - Text.
            %          * Arbitrary number of ...,key,val,... pairs.
            %            The following keywords are available:
            %             'Coo'     - Coordinate system: 'image'|'fk5'.
            %                         If 'fk5' units are deg.
            %                         Default is 'image'.
            %             'Color'   - A string or a cell array of strings
            %                         of marker colors
            %                         ('red'|'blue'|'green'|'black'|
            %                          'white'|...).
            %                         Default is 'red'.
            %             'Width'   - A scalar or a vector of markers
            %                         width. Default is 1.
            %             'Font'    - Font type. Default is 'helvetica'.
            %             'FontSize'- Font size. Default is 16.
            %             'FontStyle'-Font style. Default is 'normal'.
            % Output : null
            % Example: D = DS9(rand(100,100));
            %          D.plotText(30,30,'Hello')
            %          D.plotText(30,50,'Hello','FontSize',30, 'Color','blue')

            arguments
                Obj
                X
                Y
                Text
                Args.Coo         = 'image';
                Args.Color       = 'red';
                Args.Width       = 1;
                Args.Font        = 'helvetica';  %'helvetica 16 normal'
                Args.FontSize    = 16;
                Args.FontStyle   = 'normal';
            end
            
            if (ischar(X))
                InPar.Coo = 'fk5';
                IsSexagesimal = true;
            else
                IsSexagesimal = false;
            end
            
            if (IsSexagesimal)
                DS9.system('echo "%s; text %s %s # color=%s width=%d font={%s %d %s} text={%s}" | xpaset ds9 regions',...
                                Args.Coo,X,Y,Args.Color,Args.Width,Args.Font,Args.FontSize,Args.FontStyle,Text);
            else
                
                DS9.system('echo "%s; text %f %f # color=%s width=%d font={%s %d %s} text={%s}" | xpaset ds9 regions',...
                            Args.Coo,X,Y,Args.Color,Args.Width,Args.Font,Args.FontSize,Args.FontStyle,Text);
            end
            pause(0.2);
        end
        
    end
        
    methods    % Tile/blink methods
        % Set the tile display mode
        function tile(Obj, Val, Gap)
            % Set the tile the display mode of ds9
            % Package: @ds9
            % Description: Set the tile the display mode of ds9.
            % Input  : - Either a string to pass to the tile command,
            %            or a logical:
            %            true - change to tile mode
            %            false - change to single mode
            %            [] - toggle tile mode
            %            Two element vector - [X Y] tiles
            %            Default is [].
            %          - Gap between tiles.
            %            If empty, use no gap default.
            %            Default is [].
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,1000,1);
            %          D.load(rand(100,100),2)
            %          D.tile(true)
            %          D.tile(false)
            %          D.tile([2 1])
            %          D.tile([2 1],30)
            %          D.tile  % toggle
            %          D.tile  % toggle
            
            arguments
                Obj
                Val = [];
                Gap = [];
            end
            
            if isempty(Val)
                % get tile mode
                Res = Obj.xpaget('tile');
                switch lower(Res)
                    case 'no'
                        Val = true;
                    case 'yes'
                        Val = false;
                    otherwise
                        error('tile command returned an unknown option');
                end
            end
                
            if ischar(Val)
                Obj.xpaset('tile %s',Val);
            elseif islogical(Val)
                if Val
                    Obj.xpaset('tile yes');
                else
                    Obj.xpaset('single');
                end
            else
                % numeric vector
                Obj.xpaset('tile grid layout %d %d', Val(1), Val(2));
                if ~isempty(Gap)
                    Obj.xpaset('tile grid gap %d', Gap);
                end
                Obj.xpaset('tile');
            end
            
            
        end
        
        function blink(Obj, Par)
            % Blink all active frames
            % Input  : - Blink toggle, state, or interval.
            %            If empty - toggle blink state.
            %            If logical - change to true/false blink state.
            %            If numeric - set the interval to the numeric
            %            value, and start blinking.
            %            Default is [].
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100));
            %          D.load(rand(100,100));
            %          D.blink
            %          D.blink
            %          D.blink(1)
            %          D.blink(0.2)
            %          D.blink(false)
        
            arguments
                Obj
                Par   = [];   % toggle, logical, interval
            end
            
            if isempty(Par)
                % blink toggle - get state
                Res = Obj.xpaget('blink');
                switch lower(Res)
                    case 'yes'
                        Par = false;
                    case 'no'
                        Par = true;
                    otherwise
                        error('tile command returned an unknown option');
                end
            end
            
            Interval = [];
            if isnumeric(Par)
                Interval = Par;
                Par      = true;
            end
            
            if ~isempty(Interval)
                Obj.xpaset('blink interval %f',Interval);
            end
            
            if Par
                Obj.xpaset('blink yes');
            else
                Obj.xpaset('blink no');
            end
        end
    end
    
    methods  % lock and match
        % lock by wcs coordinates
        function lock(Obj, Par, System, Command)
            % Lock frames image/wcs/scale/scalelimits/colorbar
            % Input  : - A DS9 object.
            %          - A logical indicating if to set lock true/false.
            %            If empty, then toggle lock.
            %            If char, then append to the lock command and use
            %            xpaset to execute.
            %            Default is empty.
            %          - System to lock: 'image' | 'wcs' | 'scale' |
            %                            'scalelimits' | 'colorbar'
            %            Default is 'image'.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100));
            %          D.load(rand(100,100),2);
            %          D.tile
            %          D.zoom(5,'all')
            %          D.lock
            %          D.lock
           
            arguments
                Obj
                Par     = [];
                System  = 'image';
                Command = 'lock';      %for internal use only (see match)
            end
            
            if ischar(Par)
                Obj.xpaset(Par);
            else
                if isempty(Par)
                    % toggle lock - get state
                    Res = Obj.xpaget('%s frame',Command);
                    switch lower(Res)
                        case 'none'
                            Par = true;
                        otherwise
                            Par = false;
                    end
                end

                if Par
                    switch lower(System)
                        case 'image'
                            Obj.xpaset('%s frame image',Command);
                        case {'icrs','fk4','fk5','sphere','wcs'}
                            Obj.xpaset('%s frame wcs',Command);
                        case 'scale'
                            Obj.xpaset('%s scale yes',Command);
                        case 'colorbar'
                            Obj.xpaset('%s colorbar yes',Command);
                        case 'scalelimits'
                            Obj.xpaset('%s scalelimits yes',Command);
                        otherwise
                            error('Unknown System option');
                    end
                else
                    switch lower(System)
                        case 'image'
                            Obj.xpaset('%s frame none',Command);
                        case {'icrs','fk4','fk5','sphere','wcs'}
                            Obj.xpaset('%s frame none',Command);
                        case 'scale'
                            Obj.xpaset('%s scale no',Command);
                        case 'colorbar'
                            Obj.xpaset('%s colorbar no',Command);
                        case 'scalelimits'
                            Obj.xpaset('%s scalelimits no',Command);
                        otherwise
                            error('Unknown System option');
                    end
                end
            end
        end
        
        function match(Obj, System)
            % match frames image/wcs/scale/scalelimits/colorbar
            % Input  : - A DS9 object.
            %          - System to match - either 'image' | 'wcs'.
            %            Default is 'image'.
            %            Alternatively, this can be a string to pass after
            %            the match command (e.g., 'scale',
            %            'scalelimits','colorbar','block','smooth','axes','bin',...).
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100));
            %          D.load(rand(100,100),2);
            %          D.tile
            %          D.zoom(5,'all')
            %          D.match
           
            arguments
                Obj
                System  = 'image';
            end
            
            Command = 'match';
            
            switch lower(System)
                case {'fk5','fk4','icrs'}
                    System = 'wcs';
            end
            
            switch lower(System)
                case {'image','wcs'}
                    Obj.xpaset('%s frame %s',Command, System);
                otherwise
                    Obj.xpaset('%s %s',Command, System);
            end
           
        end
                
    end
    
    methods   % xy2sky, sky2xy
        function [CooX,CooY] = sky2xy(Obj,RA,Dec,Args)
            % Convert RA/Dec to X/Y (image) using ds9 tools
            % Input  : - A DS9 object.
            %          - J2000.0 RA [sexagesimal string, rad or deg]
            %            See 'CooUnits'. Default is 'deg'.
            %          - J2000.0 Dec
            %          * ...,key,val,...
            %            'CooType' - Output coordinate type: {'image'|'physical'}.
            %                   Default is 'image'.
            %            'CooUnits' - Input coordinates units 'rad'|'deg'
            %                   Default is 'deg'.
            %            'Frame' - Frame index. If empty, use current
            %                   frame. Default is [].
            % Output : - X [pix].
            %          - Y [pix].
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100));
            %          [a,b]=D.sky2xy(1, 3)
           
            arguments
                Obj
                RA
                Dec
                Args.CooType   = 'image';
                Args.CooUnits  = 'deg';
                Args.Frame     = [];
            end
            RAD = 180./pi;
            
            if isempty(Args.Frame)
                FrameChanged = false;
            else
                CurFrame = Obj.Frame;
                if Args.Frame~=CurFrame
                    Obj.frame(Args.Frame);
                    FrameChanged = true;
                end
            end
            
            if ischar(RA) && ischar(Dec)
                RA  = celestial.coo.convertdms(RA, 'gH','d');
                Dec = celestial.coo.convertdms(Dec, 'gD','d');
            else
                switch lower(Args.CooUnits)
                    case 'rad'
                        RA  = RA.*RAD;
                        Dec = Dec.*RAD;
                end
            end
            
            N    = numel(RA);
            CooX = nan(size(RA));
            CooY = nan(size(Dec));
            for I=1:1:N
                %--- set coordinates of crosshair ---
                Obj.xpaset('crosshair %f %f wcs icrs',RA(I),Dec(I));
                
                %--- get Coordinates of crosshair ---
                CooStr = Obj.xpaget('crosshair %s',Args.CooType);
                CooXY = DS9.parseOutput(CooStr, 'num');
                
                CooX(I) = CooXY(1);
                CooY(I) = CooXY(2);
                
            end
            Obj.xpaset('mode pointer');

            if FrameChanged
                % return to original frame
                Obj.frame(CurFrame);
            end
            
        end
        
        function [RA,Dec] = xy2sky(Obj,X,Y,Args)
            % Convert RA/Dec to X/Y (image) using ds9 tools
            % Input  : - A DS9 object.
            %          - X (pix).
            %          - Y (pix).
            %          * ...,key,val,...
            %            'CooType' - Input coordinate type: {'image'|'physical'}.
            %                   Default is 'image'.
            %            'CooUnits' - Output coordinates units 'rad'|'deg'
            %                   Default is 'deg'.
            %            'Frame' - Frame index. If empty, use current
            %                   frame. Default is [].
            % Output : - J2000.0 RA in 'CooUnits' units.
            %          - J2000.0 Dec 
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100));
            %          [a,b]=D.xy2sky(1, 3)
           
            arguments
                Obj
                X
                Y
                Args.CooType   = 'image';
                Args.CooUnits  = 'deg';
                Args.Frame     = [];
            end
            RAD = 180./pi;
            
            if isempty(Args.Frame)
                FrameChanged = false;
            else
                CurFrame = Obj.Frame;
                if Args.Frame~=CurFrame
                    Obj.frame(Args.Frame);
                    FrameChanged = true;
                end
            end
            
            N    = numel(X);
            RA   = nan(size(X));
            Dec  = nan(size(X));
            for I=1:1:N
                %--- set coordinates of crosshair ---
                Obj.xpaset('crosshair %f %f image',X(I),Y(I));
                
                %--- get Coordinates of crosshair ---
                CooStr = Obj.xpaget('crosshair wcs'); % %s',Args.CooType);
                CooRD = DS9.parseOutput(CooStr, 'num');
                
                RA(I)  = CooRD(1);
                Dec(I) = CooRD(2);
                
            end
            switch lower(Args.CooUnits)
                case 'rad'
                    RA  = RA./RAD;
                    Dec = Dec./RAD;
            end
            
            Obj.xpaset('mode pointer');

            if FrameChanged
                % return to original frame
                Obj.frame(CurFrame);
            end
            
        end
        
    end
    
    methods   % dss, skyview, vla, nvss
        function skyNVSS(Obj, RA, Dec, Size, Args)
            % Display NVSS (VLA radio) image in ds9
            % Input : - A DS9 object.
            %          - J2000.0 RA - Numerical value in deg, or
            %            sexagesimal string.
            %            If Dec, is empty, then this will be interpreted as
            %            object name (e.g., 'm31').
            %          - J2000.0 Dec (like RA, but for Dec).
            %            Default is [].
            %          - Image size [Width, height].
            %            Default is 30 (default units are arcmin).
            %          * ...,key,val,...
            %            'SizeUnits' - Size units for image size.
            %                   Default is 'arcmin'.
            %            'Frame' - Frame index in which to display the
            %                   image. If Inf, open a new frame.
            %                   If empty, use current frame.
            %                   Default is Inf.
            %            'Server' - For internal use. Default is 'nvss'.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.dss('m31',[],30)
            %          D.dss('12:00:00','+21:10:10',10,'Frame',[])
            %          D.dss(1,1)  % deg
            
            arguments
                Obj
                RA
                Dec              = [];
                Size             = [30 30];
                Args.SizeUnits   = 'arcmin';
                Args.Survey      = []
                Args.Frame       = Inf;   % [] - curreny, Inf - new
                Args.Server      = 'nvss';
            end
            
            if numel(Size)==1
                Size = [Size, Size];
            end
            
            if isempty(Args.Frame)
                Obj.xpaset('%s frame current', Args.Server)
            else
                if isinf(Args.Frame)
                    % do nothing - will open a new frame
                else
                    % move to frame
                    Obj.frame(Args.Frame);
                    Obj.xpaset('%s frame current',Args.Server)
                end
            end
            
            Obj.xpaset('%s size %f %f %s',Args.Server, Size(1), Size(2), Args.SizeUnits);
            
            if ~isempty(Args.Survey)
                Obj.xpaset('%s survey %s',Args.Server, Args.Survey);
            end
            
            if isempty(Dec)
                % assume object name
                Obj.xpaset('%s %s',Args.Server, RA);
            else
                if isnumeric(RA)
                    RA = celestial.coo.convertdms(RA, 'd', 'SH');
                end
                if isnumeric(Dec)
                    Dec = celestial.coo.convertdms(Dec, 'd', 'SD');
                end
                Obj.xpaset('%s %s %s',Args.Server, RA, Dec);
            end
        end

        function skyDSS(Obj, RA, Dec, Size, Args)
            % Display DSS (Digitized Sky Survey) image in ds9
            % Input  : - A DS9 object.
            %          - J2000.0 RA - Numerical value in deg, or
            %            sexagesimal string.
            %            If Dec, is empty, then this will be interpreted as
            %            object name (e.g., 'm31').
            %          - J2000.0 Dec (like RA, but for Dec).
            %            Default is [].
            %          - Image size [Width, height].
            %            Default is 30 (default units are arcmin).
            %          * ...,key,val,...
            %            'SizeUnits' - Size units for image size.
            %                   Default is 'arcmin'.
            %            'Frame' - Frame index in which to display the
            %                   image. If Inf, open a new frame.
            %                   If empty, use current frame.
            %                   Default is Inf.
            %            'Server' - ['sao'] | 'stsci' | 'eso'.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.skyDSS('m31',[],30)
            %          D.skyDSS('12:00:00','+21:10:10',10,'Frame',[])
            %          D.skyDSS(1,1)  % deg
            
            arguments
                Obj
                RA
                Dec              = [];
                Size             = [30 30];
                Args.SizeUnits   = 'arcmin';
                Args.Survey      = '1b';
                Args.Frame       = Inf;   % [] - curreny, Inf - new
                Args.Server      = 'sao';  % 'stsci' | 'eso' | 'sao'
            end
            
            switch lower(Args.Server)
                case {'stsci'}
                    Server   = 'dssstsci';
                    % poss2ukstu_red|poss2ukstu_ir|poss2ukstu_blue | poss1_blue|poss1_red]
                    switch lower(Args.Survey)
                        case '1b'
                            Survey = 'poss1_blue';
                        case '1r'
                            Survey = 'poss1_red';
                        case '2b'
                            Survey = 'poss2ukstu_blue';
                        case '2r'
                            Survey = 'poss2ukstu_red';
                        case '2i'
                            Survey = 'poss2ukstu_ir';
                        otherwise
                            error('Unwknon Survey option');
                    end
                case {'eso'}
                    Server   = 'dsseso';
                    % 'DSS1|DSS2-red|DSS2-blue|DSS2-infrared
                    switch lower(Args.Survey)
                        case '1b'
                            Survey = 'DSS1';
                        case '1r'
                            Survey = 'DSS1';
                        case '2b'
                            Survey = 'DSS2-blue';
                        case '2r'
                            Survey = 'DSS2-red';
                        case '2i'
                            Survey = 'DSS-infrared';
                        otherwise
                            error('Unwknon Survey option');
                    end
                case {'sao'}
                    Server   = 'dsssao';
                    Survey   = [];
                otherwise
                    error('Unknown Server option');
            end
            
            Obj.skyNVSS(RA, Dec, Size, 'SizeUnits',Args.SizeUnits,...
                                       'Survey',Survey,...
                                       'Frame',Args.Frame,...
                                       'Server',Server);
            
        end
           
        function skyFIRST(Obj, RA, Dec, Size, Args)
            % Display FIRST (VLA radio) image in ds9
            % Input : - A DS9 object.
            %          - J2000.0 RA - Numerical value in deg, or
            %            sexagesimal string.
            %            If Dec, is empty, then this will be interpreted as
            %            object name (e.g., 'm31').
            %          - J2000.0 Dec (like RA, but for Dec).
            %            Default is [].
            %          - Image size [Width, height].
            %            Default is 30 (default units are arcmin).
            %          * ...,key,val,...
            %            'SizeUnits' - Size units for image size.
            %                   Default is 'arcmin'.
            %            'Frame' - Frame index in which to display the
            %                   image. If Inf, open a new frame.
            %                   If empty, use current frame.
            %                   Default is Inf.
            %            'Survey' - ['first'] | 'stripe82'
            %            'Server' - For internal use. Default is 'nvss'.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.skyFIRST('m51',[],30)
            %          D.skyFIRST('12:00:00','+21:10:10',10,'Frame',[])
            %          D.skyFIRST(1,1)  % deg
            
            arguments
                Obj
                RA
                Dec              = [];
                Size             = [30 30];
                Args.SizeUnits   = 'arcmin';
                Args.Survey      = 'first';  % 'first' | 'stripe82'
                Args.Frame       = Inf;   % [] - curreny, Inf - new
                Args.Server      = 'vla';
            end
            
            Obj.skyNVSS(RA, Dec, Size, 'SizeUnits',Args.SizeUnits,...
                                       'Survey',Args.Survey,...
                                       'Frame',Args.Frame,...
                                       'Server','vla');
        end
        
        function skyVLSS(Obj, RA, Dec, Size, Args)
            % Display VLSS (VLA radio) image in ds9
            % Input : - A DS9 object.
            %          - J2000.0 RA - Numerical value in deg, or
            %            sexagesimal string.
            %            If Dec, is empty, then this will be interpreted as
            %            object name (e.g., 'm31').
            %          - J2000.0 Dec (like RA, but for Dec).
            %            Default is [].
            %          - Image size [Width, height].
            %            Default is 30 (default units are arcmin).
            %          * ...,key,val,...
            %            'SizeUnits' - Size units for image size.
            %                   Default is 'arcmin'.
            %            'Frame' - Frame index in which to display the
            %                   image. If Inf, open a new frame.
            %                   If empty, use current frame.
            %                   Default is Inf.
            %            'Server' - For internal use. Default is 'vlss'.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.skyVLSS('m51',[],30)
            %          D.skyVLSS('12:00:00','+21:10:10',10,'Frame',[])
            %          D.skyVLSS(1,1)  % deg
            
            arguments
                Obj
                RA
                Dec              = [];
                Size             = [30 30];
                Args.SizeUnits   = 'arcmin';
                Args.Frame       = Inf;   % [] - curreny, Inf - new
                Args.Server      = 'vlss';
            end
            
            Obj.skyNVSS(RA, Dec, Size, 'SizeUnits',Args.SizeUnits,...
                                       'Survey',[],...
                                       'Frame',Args.Frame,...
                                       'Server','vlss');
        end
        
        function sky2MASS(Obj, RA, Dec, Size, Args)
            % Display 2MASS (NIR) image in ds9
            % Input : - A DS9 object.
            %          - J2000.0 RA - Numerical value in deg, or
            %            sexagesimal string.
            %            If Dec, is empty, then this will be interpreted as
            %            object name (e.g., 'm31').
            %          - J2000.0 Dec (like RA, but for Dec).
            %            Default is [].
            %          - Image size [Width, height].
            %            Default is 30 (default units are arcmin).
            %          * ...,key,val,...
            %            'SizeUnits' - Size units for image size.
            %                   Default is 'arcmin'.
            %            'Frame' - Frame index in which to display the
            %                   image. If Inf, open a new frame.
            %                   If empty, use current frame.
            %                   Default is Inf.
            %            'Server' - For internal use. Default is '2mass'.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.sky2MASS('m51',[],30)
            %          D.sky2MASS('12:00:00','+21:10:10',10,'Frame',[])
            %          D.sky2MASS(1,1)  % deg
            
            arguments
                Obj
                RA
                Dec              = [];
                Size             = [30 30];
                Args.SizeUnits   = 'arcmin';
                Args.Frame       = Inf;   % [] - curreny, Inf - new
                Args.Server      = 'vlss';
            end
            
            Obj.skyNVSS(RA, Dec, Size, 'SizeUnits',Args.SizeUnits,...
                                       'Survey',[],...
                                       'Frame',Args.Frame,...
                                       'Server','2mass');
        end
        
        function skyView(Obj, RA, Dec, Size, Args)
            % Display an image from the skyview collection in ds9
            % Input : - A DS9 object.
            %          - J2000.0 RA - Numerical value in deg, or
            %            sexagesimal string.
            %            If Dec, is empty, then this will be interpreted as
            %            object name (e.g., 'm31').
            %          - J2000.0 Dec (like RA, but for Dec).
            %            Default is [].
            %          - Image size [Width, height].
            %            Default is 30 (default units are arcmin).
            %          * ...,key,val,...
            %            'SizeUnits' - Size units for image size.
            %                   Default is 'arcmin'.
            %            'Frame' - Frame index in which to display the
            %                   image. If Inf, open a new frame.
            %                   If empty, use current frame.
            %                   Default is Inf.
            %            'Survey' - Default is 'sdssg'.
            %            'Server' - For internal use. Default is 'skyview'.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.sky2MASS('m51',[],30)
            %          D.sky2MASS('12:00:00','+21:10:10',10,'Frame',[])
            %          D.sky2MASS(1,1)  % deg
            
            arguments
                Obj
                RA
                Dec              = [];
                Size             = [30 30];
                Args.SizeUnits   = 'arcmin';
                Args.Frame       = Inf;   % [] - curreny, Inf - new
                Args.Survey      = 'sdssg';
                Args.Server      = 'skyview';
            end
            
            Obj.skyNVSS(RA, Dec, Size, 'SizeUnits',Args.SizeUnits,...
                                       'Survey',[],...
                                       'Frame',Args.Frame,...
                                       'Server','skyview');
        end
        
    end 
        
    methods  % header
        function header(Obj, Par)
            % Display header of current ds9 frame in a window
            % Input  : - A DS9 object.
            %          - Addition string argument.
            %            E.g., '2', 'close'
            %            Default is ''.
            % Output : null
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9(rand(100,100));
            %          D.header;
            %          D.header('close');
            
            arguments
                Obj
                Par   = '';
            end
            
            Obj.xpaset('header %s',Par);
            
        end
        
        function [Val, Key] = getVal(Obj, Key, varargin)
            % Apply getVal on header of AstroImage available for the current frame.
            % Input  : - A DS9 object.
            %          - Header Keyword name.
            %          * Additional arguments to pass to
            %            AstroHeader/getVal. Default is {}.
            % Output : - Key value.
            %          - Key name.
            % Author : Eran Ofek (May 2022)
            % Example: D.getVal('NAXIS1')
           
            AI         = Obj.getAI;
            [Val, Key] = AI.HeaderData.getVal(Key, varargin{:});
        end
    end
    
    
    methods  % interactive mouse/keyboard
        function Result = getPixData(Obj, X,Y, HalfSize, CooSys, Args)
            % Get pixel data value around pixel position from the image stored in the ds9 window
            % Input  : - A DS9 object.
            %          - Vector of X [pix] or J2000.0 RA [deg/ard/sex]
            %          - Vector of X [pix] or J2000.0 RA [deg/rad/sex]
            %          - [X, Y] half size of data around central pixel to
            %            retrieve. Use [0 0] to retrieve a singel filter.
            %            Default is [1 1].
            %          - Coordinate system: 'image' | 'physical' | 'wcs'
            %            Default is 'image'.
            %          * ...,key,val,...
            %            'CooUnits' - If Coordinates are in 'wcs', this is
            %                   either 'rad', or 'deg'.
            %                   Default is 'deg'.
            %                   If coordinates are sexagesinal, then will
            %                   be ognored.
            % Output : - A structure array (element per input coordinate)
            %            with the following fields:
            %            .Data - Data value matrix
            %            .MatX - X pix matrix
            %            .MatY - Y pix matrix
            %            .X    - X center
            %            .Y    - Y center
            %            .Xcorner - X lower corner
            %            .Ycorner - Y lower corner
            % Author : Eran Ofek (May 2022)
            % XPA syntax:
            %Syntax:
            %data [<coordsys> [<skyframe>] <x> <y> <width> <height> [yes|no]]
            % 
            %Example:
            %$xpaget ds9 data image 450 520 3 3 yes
            %$xpaget ds9 data physical 899 1039 6 6 no
            %$xpaget ds9 data fk5 202.47091 47.196811 0.00016516669 0.00016516669 no
            %$xpaget ds9 data wcs fk5 13:29:53.018 +47:11:48.52 0.00016516669 0.00016516669 no
            % Example: D = DS9;
            %          D.load('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits')
            %          a=D.getPixData(100,100,[1 1])
            %          [RA,Dec]=D.xy2sky(100,100);
            %          b=D.getPixData(RA,Dec,[1 1], 'wcs')
            
            arguments
                Obj
                X
                Y
                HalfSize        = [1 1];
                CooSys          = 'image';  % 'image' | 'wcs' | 'physical'
                %Args.SkyFrame   = 'fk5';
                Args.CooUnits   = 'deg';    % deg | rad
                
            end
            SkyFrame = '';
            
            if ischar(X) && ischar(Y)
                X = {X};
                Y = {Y};
            end
            Ncoo = numel(X);
            
            switch lower(CooSys)
                case 'wcs'
                    % convert RA/Dec to pix position
                    [X, Y]   = Obj.sky2xy(X, Y, 'CooType','image', 'CooUnits',Args.CooUnits);
                    CooSys   = 'image';
                    
            end
            
            if numel(HalfSize)==1
                HalfSize = [HalfSize, HalfSize];
            end
            Size = HalfSize.*2 + 1;
            
            X = X - HalfSize(1);
            Y = Y - HalfSize(2);
            X = round(X);
            Y = round(Y);
            
            Result = struct('Data',cell(Ncoo,1), 'MatX',cell(Ncoo,1), 'MatY',cell(Ncoo,1), 'X',cell(Ncoo,1), 'Y',cell(Ncoo,1), 'Xcorner', cell(Ncoo,1), 'Ycorner', cell(Ncoo,1));
            for Icoo=1:1:Ncoo
                % float coo
                Str = Obj.xpaget('data %s %s %d %d %d %d no',CooSys, SkyFrame, X(Icoo), Y(Icoo), Size(1), Size(2));
                Result(Icoo).X       = X(Icoo) + HalfSize(1);
                Result(Icoo).Y       = Y(Icoo) + HalfSize(2);
                Result(Icoo).Xcorner = X(Icoo);
                Result(Icoo).Ycorner = Y(Icoo);
                
                Tmp = regexp(Str, '(?<X>\d+),(?<Y>\d+) = (?<Val>[\w\.]+)','names');
                Ntmp = numel(Tmp);
                Result(Icoo).Data = nan(Size(2), Size(1));
                Result(Icoo).MatX = nan(Size(2), Size(1));
                Result(Icoo).MatY = nan(Size(2), Size(1));
                for Itmp=1:1:Ntmp
                    Xn = str2double(Tmp(Itmp).X);
                    Yn = str2double(Tmp(Itmp).Y);
                    X1 = Xn - X(Icoo) + 1;
                    Y1 = Yn - Y(Icoo) + 1;
                    Result(Icoo).MatX(Y1,X1) = Xn;
                    Result(Icoo).MatY(Y1,X1) = Yn;
                    Result(Icoo).Data(Y1,X1) = str2double(Tmp(Itmp).Val);
                end
            end
            
        end
        
        function [X,Y,Click, RA, Dec, Data] = ginput1(Obj, Mode, Args)
            % iexam/ginput - get pixel position/value by mouse/key clicking on current frame.
            %   Executing this function will show a crosshair on the ds9
            %   frame, by clicking (mouse or/and keyboard) on a pixel, the
            %   function will return the position and data for this pixel.
            %   See: ginput for multiple clicks function.
            % Input  : - A DS9 object.
            %          - Mode: one of the following: 'button'|'key'|['any']
            %            'button' - use mouse.
            %            'key - use keyboard.
            %            'any' - use mouse or keyboard.
            %          * ...,key,val,...
            %            'DataSize' - half size around pixel for which to
            %                   return data. [0 0] will return a single
            %                   pixel. Default is [1 1].
            %            'CooUnits' - Units of output RA/Dec.
            % Output : - X [pix] position.
            %          - Y [pix] position.
            %          - Click: <1>,<2>,<3> for mousr buttons, or letters
            %            for keyboard.
            %          - J2000.0 RA (in CooUnits units).
            %          - J2000.0 Dec (in CooUnits units).
            %          - A structure of data for pixel value
            %            (See getPixData for fields description).
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.load('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits')
            %          [X,Y,Click, RA, Dec, Data] = D.ginput1;
            
            arguments
                Obj
                Mode             = 'any';
                Args.DataSize    = [1 1];     % width | height
                Args.CooUnits    = 'deg';
            end
            
            Command = 'iexam';
            Str = Obj.xpaget('%s %s coordinate image',Command, Mode);
            Res = DS9.parseOutput(Str, 'cell');
            Click = Res{1};
            X     = str2double(Res{2});
            Y     = str2double(Res{3});

            if nargout>3
                % get RA/Dec
                [RA, Dec] = Obj.xy2sky(X, Y, 'CooUnits',Args.CooUnits);
                
                if nargout>5
                    % get data
                    Data = Obj.getPixData(X,Y,Args.DataSize);
                end
            end
        end
        
        function [X,Y,Click, RA, Dec, Data] = ginput(Obj, Mode, Nclick, Args)
            % iexam/ginput - get pixel position/value by mouse/key clicking on current frame.
            %   Executing this function will show a crosshair on the ds9
            %   frame, by clicking (mouse or/and keyboard) on a pixel, the
            %   function will return the position and data for this pixel.
            %   See: ginput1 for single click function.
            % Input  : - A DS9 object.
            %          - Mode: one of the following: 'button'|'key'|['any']
            %            'button' - use mouse.
            %            'key - use keyboard.
            %            'any' - use mouse or keyboard.
            %          - Number of clicks to execute.
            %            If Inf, then will stop on stop-click defined
            %            in the 'Stop' argument.
            %          * ...,key,val,...
            %            'Stop' - Stop character or click: 
            %                   '<1>' for left click.
            %                   Default is 'q'.
            %            'DataSize' - half size around pixel for which to
            %                   return data. [0 0] will return a single
            %                   pixel. Default is [1 1].
            %            'CooUnits' - Units of output RA/Dec.
            % Output : - X [pix] position vector.
            %            The data include the position collected when the
            %            stop character was pressed.
            %          - Y [pix] position vector.
            %          - Cell array of click: <1>,<2>,<3> for mousr buttons, or letters
            %            for keyboard.
            %          - J2000.0 RA (in CooUnits units) vector.
            %          - J2000.0 Dec (in CooUnits units) vector.
            %          - A structure array of data for pixel value
            %            (See getData for fields description).
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.load('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits')
            %          [X,Y,Click, RA, Dec, Data] = D.ginput;
            
            arguments
                Obj
                Mode             = 'any';
                Nclick           = Inf;
                Args.Stop        = 'q';
                Args.DataSize    = [1 1];     % width | height
                Args.CooUnits    = 'deg';
            end
            
            Ind = 0;
            Cont = true;
            while Cont
                Ind = Ind + 1 ;
                [X(Ind),Y(Ind),Click{Ind}, RA(Ind), Dec(Ind), Data(Ind)] = ginput1(Obj, Mode, 'DataSize',Args.DataSize,'CooUnits',Args.CooUnits);
                if isinf(Nclick)
                    switch Click{Ind}
                        case Args.Stop
                            % right click found
                            Cont = false;
                    end
                else
                    if Ind>=Nclick 
                        Cont = false;
                    end
                end
            end
            
        end
        
        function [RA, Dec, Click, Info] = getCoo(Obj, Coo, Args)
            % get RA/Dec coordinates from image click or user input
            %   This is a utility function that return RA/Dec from
            %   interactive user click on current frame, or user input in
            %   arguments. If interactive, it optionaly display a
            %   message/instructions to the MATLAB screen.
            % Input  : - A DS9 object.
            %          - Coordinates. If empty, working in interatice mode
            %            (clicking on current frame).
            %            Alternatively [RA, Dec],
            %            or {RAsexagesimal, Decsexagesimal}
            %            Default is [].
            %          * ...,key,val,...
            %            'PromptIfEmptyCoo' - Text message to prompt on
            %                   MATLAB screen if Coo is empty and the user is
            %                   requsted to click on frame (e.g., a menue).
            %                   Default is ''.
            %            'Mode' - one of the following: 'button'|'key'|['any']
            %                   'button' - use mouse.
            %                   'key - use keyboard.
            %                   'any' - use mouse or keyboard (Default).
            %            'CooUnits' - If Coo is a numeric vector then these
            %                   are te input coordinates units.
            %                   Default is 'deg'.
            %            'OutUnits' - Output units of RA/Dec ['rad'|'deg']
            %                   Default is 'deg'.
            % Output : - J2000.0 RA
            %          - J2000.0 Dec
            %          - A structure array with additional info on the
            %            click position. Empty if user supplied coordinates.
            % Author : Eran Ofek (May 2022)
            % Example: [RA, Dec, Click, Info] = getCoo(D, {'12:00:00','+20:10:20'})
            %          [RA, Dec, Click, Info] = getCoo(D, [180, +20.1])
            
            arguments
                Obj
                Coo
                Args.PromptIfEmptyCoo = '';  % text to prompt if Coo isempty
                Args.Mode             = 'any';
                Args.CooUnits         = 'deg';
                Args.OutUnits         = 'deg';
            end
            
            RAD = 180./pi;
            
            Info = [];
            if isempty(Coo)
                if ~isempty(Args.PromptIfEmptyCoo)
                    % prompt user for clicking the image
                    fprintf('%s',Args.PromptIfEmptyCoo);
                end
                
                [X,Y,Click, RA, Dec, Data] = ginput1(Obj, Args.Mode);
                Info.Click = Click;
                Info.X     = X;
                Info.Y     = Y;
                Info.Data  = Data;
            else
                Click = [];
                if iscell(Coo)
                    RA  = celestial.coo.convertdms(Coo(1), 'SH', 'd');
                    Dec = celestial.coo.convertdms(Coo(2), 'SD', 'd');
                elseif isnumeric(Coo)
                    RA  = Coo(1);
                    Dec = Coo(2);
                    ConvFactor = convert.angular(Args.CooUnits, 'deg');
                    RA  = RA.*ConvFactor;
                    Dec = Dec.*ConvFactor;
                else
                    error('Unknown Coo option');
                end
            end
            
            ConvFactor = convert.angular('deg', Args.OutUnits);
            RA         = RA.*ConvFactor;
            Dec        = Dec.*ConvFactor;
        end
        
        function [X, Y] = crosshair(Obj, Args)
            % Get crosshair position
            % Input  : - A DS9 object.
            %          * ...,key,val,...
            %            'CooSys' - ['image'] | 'physical' | 'wcs'
            %            'CooRef' - Default is 'icrs'.
            %            'ChangeMode' - A logical indicating if to change
            %                   mode to 'crosshair', and then return to original
            %                   mode after crosshair coordinates retrival
            %                   is done. Default is true.
            % Output : - X coordinate [pix] or RA [deg]
            %            Use 'CooSys', and 'CooRef' to control output.
            %          - Y coordinate [pix] or Dec [deg]
            % AUthor : Eran Ofek (May 2022)
            % Example: [x,y]=D.crosshair('CooSys','wcs')
            
            arguments
                Obj
                Args.CooSys                = 'image';  % 'wcs' 
                Args.CooRef                = 'icrs';
                Args.ChangeMode logical    = true;
            end
            
            if Args.ChangeMode
                OrigMode = Obj.mode([]);
                if ~strcmp(OrigMode, 'crosshair')
                    % change mode
                    Obj.mode('crosshair');
                end
            end
            
            switch lower(Args.CooSys)
                case {'image','physical'}
                    Str = Obj.xpaget('crosshair %s',Args.CooSys);
                otherwise
                    Str = Obj.xpaget('crosshair %s %s',Args.CooSys, Args.CooRef);
            end
            Coo = DS9.parseOutput(Str, 'num');
            X   = Coo(1);
            Y   = Coo(2);
            
            if Args.ChangeMode
                % back to original mode
                Obj.mode(OrigMode);
            end
            
        end
        
        function Data = getInfoData(Obj, X,Y, Radius, Args)
            % Get data around position from the image stored in InfoAI
            % Input  : - A DS9 object containing images in the InfoAI
            %            property.
            %          - X [pix] or J2000.0 RA [Units in 'CooUnits' arg]
            %          - Y [pix] or J2000.0 Dec [Units in 'CooUnits' arg]
            %          - Radius [pix] of the rectangular stamp that will be
            %            returned around each position.
            %          * ...,key,val,...
            %            'CooSys' - Coo. system: ['image']|'wcs'
            %                   This argument refers to the X/Y coordinates
            %                   input.
            %            'CooUnits' - If Coo sys. is 'wcs', then these are
            %                   the coordinate units: 'rad'|['deg']
            %            'DataProp' - A cell array of data properties in
            %                   the AstroImage (stored in InfoAI) that will
            %                   be queried.
            %                   Default is {'Image','Back','Var','Mask'}.
            %            'GetCat' - A logical indicating if to query the
            %                   catalog data in the AstroImage around the
            %                   input position.
            %                   Default is true.
            %            'Frame' - Frame index. If empty, use current.
            %                   Default is [].
            %            'Window' - ds9 window name. If empty, use current.
            %                   Default is [].
            % Iutput : - A structure containing the retrievd data.
            %            The following fields are available:
            %            .Val - A structure with field per queried
            %                   'DataProp'. These are the values of the
            %                   pixel at the specified position.
            %            .Mean - Same as Val, but for the mean of pixel
            %                   values within the radius (rectanngular shape)
            %                   around the search position.
            %            .Std - Same as Mean, but for the std of the pixel
            %                   values.
            %            .Cat - An AstroCatalog with the selected sources
            %                   within the search radius from the search
            %                   position.
            % Author : Eran Ofek (May 2022)
            % Example: Data = getInfoData(D, 100,100, 3);,
            
            arguments
                Obj
                X
                Y
                Radius                  = 3;   % [pix]
                Args.CooSys             = 'image';
                Args.CooUnits           = 'deg';    % input coo units
                Args.DataProp           = {'Image','Back','Var','Mask'};
                Args.GetCat logical     = true;
                %Args.IsCircle logical   = true;
                
                Args.Frame              = [];
                Args.Window             = [];
                
            end
            
            AI = Obj.getAI(Args.Frame, Args.Window);
            
            switch lower(Args.CooSys)
                case 'image'
                    % get RA/Dec
                    AI.populateWCS;
                    [RA, Dec] = AI.WCS.xy2sky(X, Y, 'OutUnits','deg');
                otherwise
                    % input is RA/Dec
                    RA  = X;
                    Dec = Y;
                    % get X/Y
                    [X, Y] = AI.WCS.sky2xy(RA, Dec, 'InUnits',Args.CooUnits);
            end
            
            Nprop = numel(Args.DataProp);
            for Iprop=1:1:Nprop
                
                Data.Val.(Args.DataProp{Iprop})   = getImageVal(AI, X, Y, 'DataProp',Args.DataProp{Iprop});
                switch numel(AI.(Args.DataProp{Iprop}))
                    case 0
                        Stamp  = [];
                    case 1
                        Stamp  = Data.Val.(Args.DataProp{Iprop});
                    otherwise
                        LongProp                          = AstroImage.PropDataTranslation(Args.DataProp{Iprop});
                        Stamp                             = imProc.image.cutouts(AI, [X, Y], 'Shift',false, 'DataProp',LongProp, 'HalfSize',Radius);
                end
                
                Data.Stamp.(Args.DataProp{Iprop}) = Stamp;
                
                switch lower(Args.DataProp{Iprop})
                    case 'mask'
                        Data.Mean.(Args.DataProp{Iprop})  = tools.array.bitor_array(Stamp);
                        Data.Std.(Args.DataProp{Iprop})   = [];
                    otherwise
                        Data.Mean.(Args.DataProp{Iprop}) = mean(Stamp,'all');
                        Data.Std.(Args.DataProp{Iprop})  = std(Stamp,[],'all');
                end
                
            end
            
            if Args.GetCat && sizeCatalog(AI)>0
                % query for sources around position
                XY   = getXY(AI.CatData);
                Dist = sqrt( (XY(:,1)-X).^2 + (XY(:,2)-Y).^2 );
                Data.Cat  = selectRows(AI.CatData.Catalog, Dist < Radius);
            else
                Data.Cat = [];
            end
            
        end
        
        function Result = getStamp(Obj, X, Y, Args)
            % get stamp around image coordinates from ds9 frame or from InfoAI
            % Input  : - A DS9 object.
            %          - X image position.
            %          - Y image position.
            %          * ...,key,val,...
            %            'UseInfoAI' - A logical indicating if to read the
            %                   image stamp from InfoAI (true) or ds9
            %                   (false.
            %                   Default is true.
            %            'DataProp' - From which data property in InfoAI
            %                   the image data should be read
            %                   {'Image'|'Back'|,'Var'|'Mask'}
            %                   Default is 'Image'.
            %            'HalfSize' - Half size of image stamp.
            %                   Default is 15 (so stamp is 31x31).
            % Output : - A matrix containing the image stamp around the
            %            requested coordinates.
            % Author : Eran Ofek (Jun 2022)
            % Example: D = DS9;
            %          D.load('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits')
            %          [Result] = D.exam(512,512);
            
            arguments
                Obj
                X
                Y
                Args.UseInfoAI logical     = true;
                Args.DataProp              = 'Image';
                Args.HalfSize              = 15;
            end
            
            if Args.UseInfoAI
                % read data around position from InfoAI
                Data     = Obj.getInfoData(X,Y, Args.HalfSize, 'CooSys','image');
                Result = Data.Stamp.(Args.DataProp);
            else
                % read data around position from image in current frame
                Data     = Obj.getPixData(X,Y, Args.HalfSize, 'image');
                Result = Data.Data;
            end
            
        end
        
    end
     
    methods  % interact with external DB ard resources
        
        % missing: SIMBAD, GALEX, ZTF, PTF, WISE, IRAS, UKIRT, ...
        function [Link,RA,Dec] = getLink(Obj, Service, Coo, Args)
            % Get some service (e.g., SDSS navigator) link by clicking on frame
            %   Get link for some sky maps service (SDSS, PS1, etc) by
            %   clicking in franme and getting WCS coordinates.
            %   Alternatively open the link in a browser.
            % Input  : - A DS0 object.
            %          - Serive - one of the following web services:
            %               'SDSS' - SDSS navigator
            %               'PS1' - PS1 navigator
            %               'NED' - NED search (see 'nedArgs' arg)
            %               'DECaLS' - DECaLS image viewer (see
            %                           'decalsArgs' arg').
            %               '
            %            Default is 'SDSS'.
            %            However, if using clicking option (see next arg),
            %            the user can specify which service to open, by
            %            clicking by a letter.
            %            In the clicking mode - a menue will be dispalyed
            %            in the MATLAB window.
            %          - Coordinates. If empty, working in interatice mode
            %            (clicking on current frame).
            %            Alternatively [RA, Dec],
            %            or {RAsexagesimal, Decsexagesimal}
            %            Default is [].
            %          * ...,key,val,...
            %            'CooUnits' - If Coo is a numeric vector then these
            %                   are te input coordinates units.
            %                   Default is 'deg'.
            %            'OpenLink' - A logical indicating if to open the
            %                   link in a web window.
            %            'nedArgs' - A cell array of additional arguments to
            %                   pass to VO.NED.ned_link.
            %                   Default is {3} - i.e., 3 arcmin search
            %                   radius.
            %            'decalsArgs' -  cell array of additional arguments to
            %                   pass to VO.DECaLS.decals_viewer_link
            %                   Default is {}.
            % Output : - A cell array of a single WWW link to the service.
            %          - J2000.0 RA [deg]
            %          - J2000.0 Dec [deg]
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.load('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits')
            %          D.getLink
            
            arguments
                Obj
                Service                = 'SDSS';
                Coo                    = [];          % [] - click, {} - sexa, [RA, Dec]
                Args.CooUnits          = 'deg';  % input units 'deg' | 'rad' | 'pix'
                Args.OpenLink logical  = true;
                Args.nedArgs           = {3};
                Args.decalsArgs        = {};
            end
            RAD = 180./pi;
            
            PromptMsg = sprintf('Use any key to click on position\n');
            PromptMsg = sprintf('%s    Mouse left click : %s\n',PromptMsg, Service);
            PromptMsg = sprintf('%s    s - SDSS\n',PromptMsg);
            PromptMsg = sprintf('%s    p - Pan-STARRS\n',PromptMsg);
            PromptMsg = sprintf('%s    n - NED\n',PromptMsg);
            PromptMsg = sprintf('%s    d - DECaLS\n',PromptMsg);
            PromptMsg = sprintf('%s    \n',PromptMsg);
            
            [RA, Dec, Click, Info] = getCoo(Obj, Coo, 'PromptIfEmptyCoo',PromptMsg,...
                                                      'Mode','any',...
                                                      'CooUnits',Args.CooUnits,...
                                                      'OutUnits','deg');
                                                  
            RA_rad  = RA./RAD;   % [rad]
            Dec_rad = Dec./RAD;  % [rad]
            
            if isempty(Click)
                % use Service from input argumnets
            else
                if strcmp(Click, '<1>')
                    % use defult service
                else
                    Service = Click;
                end
            end
            
            switch lower(Service)
                case {'s','sdss'}
                    Link = VO.SDSS.navigator_link(RA_rad, Dec_rad);
                case {'p','ps1'}
                    Link = telescope.PS1.navigator_link(RA_rad, Dec_rad);
                case {'n','ned'}
                    Link = VO.NED.ned_link(RA_rad, Dec_rad, Args.nedArgs{:});
                case {'d','decals'}
                    Link = VO.DECaLS.decals_viewer_link(RA_rad, Dec_rad, Args.decalsArgs{:});
                otherwise
                    error('Not supported option');
            end       
            
            if Args.OpenLink
                web(Link{1}, '-browser');
            end
                
        end    
        
    end
    
    methods  % image examination
        function Result = imexam(Obj, Args)
            % Interactive image examination - photometry an dplot curves, surface
            % Input  : - A DS9 object.
            %          * ...,key,val,...
            %            '
            % Output : -
            % Author : Eran Ofek (Jun 2022)
            % Example: D = DS9;
            %          D.load('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits')
            %          D.inexam
            
            arguments
                Obj
                Args.Fun                   = 'r';
                Args.X                     = [];
                Args.Y                     = [];
                Args.RA                    = [];
                Args.Dec                   = [];
                Args.Mode                  = 'any';
                Args.HalfSize              = 15;
                Args.UseInfoAI logical     = true;
                Args.DataProp              = 'Image';
                
                Args.Step                  = 1;
                
                Args.LineWidth             = 2;
            end
            
            Vec = (-Args.HalfSize:1:Args.HalfSize);
            
            Cont = true;
            Iclick = 0;
            while Cont
                Iclick = Iclick + 1;
                Click = [];
                if ~(isempty(Args.RA) && isempty(Args.Dec))
                    % convert RA/Dec to X/Y
                    [X, Y] = Obj.sky2xy(Args.RA, Args.Dec);
                    Cont = false;
                else
                    if ~(isempty(Args.X) && isempty(Args.Y))
                        X = Args.X;
                        Y = Args.Y;
                        Cont = false;
                    else
                        % get X/Y from mouse
                        [X, Y, Click, RA, Dec] = Obj.ginput1(Args.Mode);
                    end
                end

                Stamp = Obj.getStamp(X, Y, 'HalfSize',Args.HalfSize, 'UseInfoAI',Args.UseInfoAI, 'DataProp',Args.DataProp);

                if isempty(Click)
                    % non-interactive mode
                    Fun = Args.Fun;
                else
                    % interactive mode
                    switch Click
                        case {'<1>','<2>','<3>'}
                            % moiuse click - use default Fun
                            Fun = Args.Fun;
                        otherwise
                            Fun = Click;
                    end
                end

                % execute function on data
                switch Fun
                    case {'h','H'}
                        % show help
                        fprintf('\n');
                        fprintf('--- Interactive menu ---\n');
                        fprintf('  q - quit\n');
                        fprintf('  h - show this help menu\n');
                        fprintf('  r - Radial plot with centering\n');
                        fprintf('  t - Radial plot without centering\n');
                        fprintf('  s - surface plot\n');
                        fprintf('  x - curve throuh x axis\n');
                        fprintf('  y - curve throuh y axis\n');
                        fprintf('\n');
                        
                    case {'q','Q'}
                        % quit
                        Cont = false;
                    otherwise

                        % center on source
                        [M1,M2,Aper] = imUtil.image.moment2(Stamp, Args.HalfSize+1, Args.HalfSize+1);
                        Xc = M1.X;
                        Yc = M1.Y;

                        % print center on scrren
                        fprintf('--- Moments and aperture photometry ---\n');
                        fprintf('Xinii = %7.3f, Yini=%7.3f\n',X, Y);
                        fprintf('X1    = %7.3f, Y2  =%7.3f\n',M1.X-Args.HalfSize+X, M1.Y-Args.HalfSize+Y);
                        fprintf('Niter = %d\n',M1.Iter);
                        fprintf('X2=%f,  Y2=%f,  XY=%f\n',M2.X2, M2.Y2, M2.XY);
                        fprintf('Aper phot: Back=%f,  BackStd=%f\n',Aper.AnnulusBack, Aper.AnnulusStd);
                        for Iaper=1:1:numel(Aper.AperRadius)
                            fprintf('Aper phot: Radius=%f,  Flux=%f\n',Aper.AperRadius(Iaper), Aper.AperPhot(Iaper));
                        end


                        % no source centering
                        Xp = Args.HalfSize + 1;
                        Yp = Args.HalfSize + 1;

                        switch lower(Fun)
                            case 'r'
                                % radial plot w/centering
                                cla;
                                Radial = imUtil.psf.radialProfile(Stamp, [Xc, Yc], 'Radius',Args.HalfSize, 'Step',Args.Step);
                                Result{Iclick} = Radial;

                                plot(Radial.R, Radial.MeanV, 'k-','LineWidth',Args.LineWidth);
                                H = xlabel('Radius [pix]');
                                H.FontSize    = 18;
                                H.Interpreter = 'latex';
                                H = ylabel('Value');
                                H.FontSize    = 18;
                                H.Interpreter = 'latex';
                                drawnow;
                            case 't'
                                % radial plot without centering
                                Radial = imUtil.psf.radialProfile(Stamp, [Xp, Yp], 'Radius',Args.HalfSize, 'Step',Args.Step);
                                Result{Iclick} = Radial;
                                cla;
                                plot(Radial.R, Radial.MeanV, 'k-','LineWidth',Args.LineWidth);
                                H = xlabel('Radius [pix]');
                                H.FontSize    = 18;
                                H.Interpreter = 'latex';
                                H = ylabel('Value');
                                H.FontSize    = 18;
                                H.Interpreter = 'latex';
                                drawnow;

                            case 'x'
                                % x plot with centering
                                cla;
                                plot(X+Vec,  Stamp(round(Yc),:), 'k-', 'LineWidth',Args.LineWidth);
                                
                                Result{Iclick} = Stamp(round(Yc),:);
                                H = xlabel('X [pix]');
                                H.FontSize    = 18;
                                H.Interpreter = 'latex';
                                H = ylabel('Value');
                                H.FontSize    = 18;
                                H.Interpreter = 'latex';
                                drawnow;
                            case 'y'
                                % y plot with centering
                                cla;
                                plot(Y+Vec,  Stamp(:,round(Xc)), 'k-', 'LineWidth',Args.LineWidth);
                                
                                Result{Iclick} = Stamp(:,round(Xc));
                                H = xlabel('Y [pix]');
                                H.FontSize    = 18;
                                H.Interpreter = 'latex';
                                H = ylabel('Value');
                                H.FontSize    = 18;
                                H.Interpreter = 'latex';
                                drawnow;
                            case 'w'
                            case 'v'
                            case 's'
                                % surface plot
                                cla;
                                surface(Y+Vec, X+Vec, Stamp);
                                colorbar;
                                
                                Result{Iclick} = Stamp;
                                H = xlabel('X [pix]');
                                H.FontSize    = 18;
                                H.Interpreter = 'latex';
                                H = ylabel('Y [pix]');
                                H.FontSize    = 18;
                                H.Interpreter = 'latex';
                                drawnow;

                            case 'a'
                            case 'p'
                            case 'g'
                                % galaxy fitting
                            otherwise

                        end
                end
            end
        end
        
    end
    
    methods  % interact with catsHTM
        % coneSearch on specific catalog including MergedCat
        
        function [Cat, RA, Dec] = catsHTM(Obj, CatalogName, Coo, Args)
            % Query a catsHTM catalog by interactive source position on current ds9 frame
            % Input  : - A DS9 object.
            %          - Default catsHTM catalog name to query.
            %            Execute: catsHTM.catalogs for options.
            %            Default is 'MergedCat'
            %            If empty, use default.
            %          - Coordinates. If empty, working in interatice mode
            %            (clicking on current frame).
            %            Alternatively [RA, Dec],
            %            or {RAsexagesimal, Decsexagesimal}
            %            Default is [].
            %          * ...,key,val,...
            %            'SearchRadius' - Cone search search radius.
            %                   Default is 10 (default units 'arcsec').
            %            'SearchradiusUnits' - Cone search radius units.
            %                   Default is 'arcsec'.
            %            'CooUnits' - If coo are not empty or sexagesimal,
            %                   this is the input coordinates units 'rad'|'deg'.
            %                   Default is 'deg'.
            %            'OutType' - catsHTM.cone_search output type.
            %                   Options are 'mat'|'astcat'|'catcl'|'astrocatalog'|'table'
            %                   Default is 'table'.
            % Output : - A catsHTM catalog with sources around the
            %            requested coordinates.
            %          - Searched J2000.0 RA [deg].
            %          - Searched J2000.0 Dec [deg].
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.load('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits')
            %          [Cat, RA, Dec] = catsHTM(D,[], [10 10])
            %          [Cat, RA, Dec] = catsHTM(D,[], {'01:15:10','-20:10:20'})
            %          [Cat, RA, Dec] = catsHTM(D,[], []);  % interactive
           
            arguments
                Obj
                CatalogName             = 'MergedCat';
                Coo                     = [];
                Args.SearchRadius       = 10;
                Args.SearchradiusUnits  = 'arcsec';
                Args.CooUnits           = 'deg';
                Args.OutType            = 'table';
            end
            
            if isempty(CatalogName)
                CatalogName = 'MergedCat';
            end
                
            TextMsg = sprintf('Interactively select a source in the image to query\n');
            TextMsg = sprintf('%s  Press one of the following keys to select a catalog query\n',TextMsg);
            TextMsg = sprintf('%s     Left mouse click - %s\n',TextMsg, CatalogName);
            TextMsg = sprintf('%s     s - SDSS\n',TextMsg);
            TextMsg = sprintf('%s     p - PS1\n',TextMsg);
            TextMsg = sprintf('%s     g - GAIA\n',TextMsg);
            TextMsg = sprintf('%s     \n',TextMsg);
            TextMsg = sprintf('%s     \n',TextMsg);
            TextMsg = sprintf('%s     \n',TextMsg);
            
            [RA, Dec, Click] = Obj.getCoo(Coo, 'CooUnits', Args.CooUnits, 'Mode','any', 'PromptIfEmptyCoo',TextMsg);
            
            switch Click
                case '<1>'
                    % use default catalog name
                    CatName = catalogName;
                case 's'
                    CatName = 'SDSSDR10';
                case 'p'
                    CatName = 'PS1';
                case 'g'
                    CatNAME = 'GAIAEDR3';
                otherwise
                    error('Unknown Catalog name option');
            end
            Cat = catsHTM.cone_search(CatName, RA, Dec, Args.SearchRadius, 'RadiusUnits',Args.SearchradiusUnits,...
                                                                           'OutType',Args.OutType);
           
        end
    end
    
    methods  % catalogs
        function [Data,FileName] = catalog(Obj, CatName, Args)
            % Overplot a ds9 catalog on a ds9 frame and upload the catalog to memory.
            %       The search radius is set by ds9.
            % Input  : - A DS9 object.
            %          - Catalog name.
            %            if 'clear' | 'cancel' | 'close',  then run this command after
            %            the catalog command.
            %            Otherwise this is a catalog name:
            %            Default is '2mass'
            %               [ned|simbad|denis|skybot]
            %               [aavso|ac|ascss|cmc|gaia|gsc1|gsc2|gsc3|nomad|ppmx|sao|sdss5|sdss6|sdss7|sdss8|sdss9|tycho]
            %               [ua2|ub1|ucac2|ucac2sup|ucac3|ucac4|urat1]
            %               [2mass|iras]
            %               [csc|xmm|rosat]
            %               [first|nvss]
            %               [chandralog|cfhtlog|esolog|stlog|xmmlog]
            %               [cds <catalogname>]
            %               [cds <catalogid>]
            %          * ...,key,val,...
            %            'OutType' - 'file' | 'table' | ['AstroCatalog']
            %            FileName - File name in which to save the data.
            %                   Default is tempname.
            %            'ReadType' - File type to save 'rdb'|['tsv']
            %            'KeepFile' - Keep saved file. Default is false.
            % Output : - The output catalog.
            %          - File name (if kept).
            % Author : Eran Ofek (May 2022)
            % Example: D = DS9;
            %          D.load('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
            %          Cat = D.catalog('sdss9');
            
            arguments
                Obj
                CatName                 = '2mass';
                Args.OutType            = 'Astrocatalog';  % 'AstroCatalog' | 'file'
                Args.FileName           = tempname;
                Args.ReadType           = 'tsv'; % rdb|tsv
                Args.KeepFile logical   = false;
            end
            
            Obj.xpaset('catalog %s',CatName);
            
            switch lower(CatName)
                case {'clear','close','cancel'}
                    % do nothing
                    Data     = [];
                    FileName = [];
                otherwise
                    if nargout>0
                        [Data,FileName] = catExport(Obj, 'OutType',Args.OutType,...
                                                         'FileName',Args.FileName,...
                                                         'ReadType',Args.ReadType,...
                                                         'KeepFile',Args.KeepFile);
                    end
            end
        end
        
        function [Data,FileName] = catExport(Obj, Args)
            % Export catalog from ds9 catalog dialog box (see catalog method)
            % Input  : - A DS9 object.
            %          * ...,key,val,...
            %            'OutType' - 'file' | 'table' | ['AstroCatalog']
            %            FileName - File name in which to save the data.
            %                   Default is tempname.
            %            'ReadType' - File type to save 'rdb'|['tsv']
            %            'KeepFile' - Keep saved file. Default is false.
            % Output : - The output catalog.
            %          - File name (if kept).
            % Author : Eran Ofek (May 2022)
            % Example: Data = D.catExport;
            
            arguments
                Obj
                
                Args.OutType            = 'Astrocatalog';  % 'AstroCatalog' | 'file'
                Args.FileName           = tempname;
                Args.ReadType           = 'tsv'; % rdb|tsv
                Args.KeepFile logical   = false;
            end
            
            Obj.xpaset('catalog export %s %s',Args.ReadType, Args.FileName);
            
            FileName = Args.FileName;
            switch lower(Args.OutType)
                case 'file'
                    Args.KeepFile = true;
                otherwise
				
                    Table = files.io.readtable1(FileName, 'FileType','delimitedtext', 'Delimiter','\t'); 
                    switch lower(Args.OutType)
                        case 'table'
                            Data = Table;
                        case 'astrocatalog'
                            Data = AstroCatalog;
                            Data.Catalog  = Table;
                            Data.ColNames = Table.Properties.VariableNames;
                        otherwise
                            error('Unknown OutType option');
                    end
            end
                                
            if ~Args.KeepFile
                delete(Args.FileName);
                FileName = [];
            end
            
        end
    end
    
    methods  % asteroids
        % Plot known asteroids on image
        function Result = plotAsteroids(Obj, Args)
            %
            % Example: D = DS9;
            %          D.load('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
            %          D.plotAsteroids
            
            arguments
                Obj
                Args.popFieldsAIArgs cell           = {};
                Args.match2solarSystemArgs cell     = {};
            end
            
            % get RA/Dec/FoV/JD of image
            % check if InfoAI is available for image
            AI = Obj.getAI;
            if isempty(AI)
                %
                error('Not supported yet for images without InfoAI');
            else
                Obj.popFieldsAI(Args.popFieldsAIArgs{:});
                AI = Obj.getAI;
                [Result, CatOut] = imProc.match.match2solarSystem(AI, Args.match2solarSystemArgs{:});
            end
            
            % Find all known asteroids in FOV
            
            % plot 
        
        end
    end
    
    methods  % interactive inspection based on InfoAI
        
        
    end
    
    
       
    methods % GUI
        function gui(Obj, Args)
            %
            % https://www.mathworks.com/help/matlab/creating_guis/share-data-among-callbacks.html#mw_b98520d7-da2a-44fe-a5d2-255683b09365
            % https://www.mathworks.com/help/matlab/creating_guis/share-data-among-callbacks.html
            
            
            arguments
                Obj
                Args.Fun   = [];
            end
            
            AI = Obj.getAI;
            AI.populateWCS;
            
            
            Fig = uifigure;
            
            
            F = figure;
            UI.B        = uicontrol(F,'Style','pushbutton','String','STOP', 'Position',[20 20 100 40], 'callback','src');            
            UI.Title    = uicontrol(F,'Style','text',      'String','Display image crosshair information', 'FontSize',14, 'Position',[20 400 500 40]);
            UI.PosXY    = uicontrol(F,'Style','text',      'String','', 'FontSize',12, 'Position',[20 300 500 40]);
            UI.PosRD    = uicontrol(F,'Style','text',      'String','', 'FontSize',12, 'Position',[20 200 500 40]);
            drawnow;
            
            UI.B.UserData.StopCrosshairMode = false;
            
            Obj.mode('crosshair');
            
            Cont = true;
            Ind  = 0;
            while ~F.UserData.StopCrosshairMode
                Ind = Ind + 1;
                
                Str = Obj.xpaget('crosshair');
                Coo = DS9.parseOutput(Str, 'num');
                
                [RA, Dec] = AI.WCS.xy2sky(Coo(1), Coo(2));
                
                
                UI.PosXY.String = sprintf('X=%f     Y=%f',Coo(1), Coo(2));
                UI.PosRD.String = sprintf('RA=%f    Dec=%f',RA, Dec);
                
                F.UserData.StopCrosshairMode
                pause(0.1)
                drawnow;
            end
            
            Obj.mode('region');
        end
    end
    
    
    
    
    
    
    
    
    
    % Interactive examination
    % (imexam,
    methods (Static)
        
        % plot and return a line profile along the x-axis
        function [Res]=imexam1(Image,varargin)
            % ds9 image examination utility
            % Package: @ds9
            % Description: Interactive image examination in ds9.
            % Input  : - Optional image. This may be a SIM object
            %            containing a catalog.
            %            If not given then read the image from the current
            %            ds9 frame.
            %          * Arbitrary number of ...,keyword,value,... pairs.
            %            The following keywords are available:
            %            'Plot' - Display plot. Default is true.
            %            'PlotPar - Cell array of additional arguments to
            %                     pass to the plot command.
            %                     Default is {}.
            %            'AperRad' - Vector of aperture radii in which to
            %                     calculate aperture photometry.
            %                     Default is [2 4 8 12 16].
            %            'Annulus' - Aperture photometry background annulus
            %                     [inner outer] radius.
            %                     Default is [16 22].
            %            'ZP'    - Photometry zero point. Default is 22.
            %            'SemiLen' - Cuts line semi-length [pix].
            %                      Default is 15.
            %            'MeanFun' - Radial plots annular mean function
            %                      handle. Default is @mean.
            %            'Radius' - Radial plots radius [pix].
            %                      Default is 7.
            %            'Sigma'  - PSF sigma guess for first moment
            %                      estimation [pix]. Default is 1.5.
            %            'MaxIter' - Maximum number of centering
            %                      iterations. Default is 3.
            %            'BackPar' - Cell array of additional arguments to
            %                      pass to the SIM/background function.
            %                      Default is {'Block',[128 128]}.
            %            'ExtractionFun' - Source extraction function
            %                      handle. Default is @mextractor.
            %            'ExtractionFunPar' - Cell array of additional
            %                      arguments to pass to the source
            %                      extraction function.
            %            'SearchRad' - Nearest object catalog search
            %                      radius [pix]. Default is 50.
            %            'Field' - SIM field from which to extract cut
            %                      values. Default is 'Im'.
            % Output : - A structure array (element per click) with the
            %            retrieved numerical information.
            
            DefV.Plot             = true;
            DefV.PlotPar          = {};
            DefV.AperRad          = [2 4 8 12 16];
            DefV.Annulus          = [16 22];
            DefV.ZP               = 22;
            DefV.SemiLen          = 15;
            DefV.MeanFun          = @mean;
            DefV.Radius           = 7;
            DefV.Sigma            = 1.5;
            DefV.MaxIter          = 3;
            DefV.BackPar          = {'Block',[128 128]};
            DefV.ExtractionFun    = @mextractor;
            DefV.ExtractionFunPar = {};
            DefV.SearchRad        = 50;
            DefV.Field            = SIM.ImageField;
            InPar = InArg.populate_keyval(DefV,varargin,mfilename);

            if (nargin==0)
                Image = [];
            end
            
            if (isempty(Image))
                % Read entire image from ds9
                Image = ds9.read2sim;
            else
                if (SIM.issim(Image))
                    % image already in SIM format - do nothing
                else
                    Image = images2sim(Image);
                end
            end
            
            Cont = true;
            Ind  = 0;
            while Cont
                Ind = Ind + 1;
                fprintf('\n');
                fprintf('Click h for help, q to abort\n');
                [CooX,CooY,~,Key]=ds9.getpos(1);
                Res(Ind).Type = Key;
                Res(Ind).CooX = CooX;
                Res(Ind).CooY = CooY;
                
                switch Key{1}
                    case {'Right','Left','Up','Down'}
                        % move cursor
                        IndPos = strcmp(Key{1},{'Right','Left','Up','Down'});
                        Pos = [1 0; -1 0; 0 -1; 0 1]; % note that up and down are inverted (ds9)
                        % Move cursor relative position
                        ds9.system('xpaset -p ds9 cursor %d %d',Pos(IndPos,:));
                        
                    case {'h','?','H'}
                        % display help
                        fprintf('\n --- ds9.imexam Menu:\n')
                        fprintf('q   - Abort\n');
                        fprintf('h   - This help\n');
                        fprintf('Left mouse click - recenter on mouse click\n');
                        fprintf('Arrows - move cursor\n');
                        fprintf('v   - Plot vector cut between 2 points\n');
                        fprintf('x,j - Plot vector cut along x axis\n');
                        fprintf('y,i - Plot vector cut along y axis\n');
                        fprintf('s   - Surface plot around point\n');
                        fprintf('c   - Countour plot around point\n');
                        fprintf('m   - First and second moments\n');
                        fprintf('r   - Radial profile with centering\n');
                        fprintf('R   - Radial profile without centering\n');
                        fprintf('a   - Aperture photometry with centering\n');
                        fprintf('A   - Aperture photometry without centering\n');
                        fprintf('p   - PSF photometry with centering\n');
                        fprintf('P   - PSF photometry without centering\n');
                        fprintf('b   - Estimate local background and noise\n');
                        fprintf('S   - Return the nearest source found using mextractor\n');
                        fprintf('e   - Edit/set parameters (and show help for editing)\n');
                    case 'q'
                        % Abort
                        Cont = false;
                    case '<1>'
                        % center on mouse click
                        fprintf('Recenter image on X=%9.3f  Y=%9.3f\n',CooX,CooY);
                        ds9.pan('to',CooX,CooY,'image');
                        
                    case 'v'
                        % plot vector cut between two points
                        fprintf('Click on second point for vector cut plot\n');
                        [CooX2,CooY2,~,~]=ds9.getpos(1);
                        cla;  % clear axis
                        Res(Ind).Res  = vector_prof(Image,[CooX,CooY],[CooX2,CooY2],...
                                                    'Plot',InPar.Plot,'PlotPar',InPar.PlotPar,'Field',InPar.Field);
                    case {'x','j'}
                        % plot x-axis vector around a single point
                        cla;  % clear axis
                        Res(Ind).Res = vector_prof(Image,[CooX-InPar.SemiLen, CooY],...
                                                         [CooX+InPar.SemiLen, CooY],...
                                                   'Plot',InPar.Plot,'PlotPar',InPar.PlotPar,'Field',InPar.Field,...
                                                   'AxisType','x');
                    case {'y','i'}
                        % plot y-axis vector around a single point
                        cla;  % clear axis
                        Res(Ind).Res = vector_prof(Image,[CooX, CooY-InPar.SemiLen],...
                                                         [CooX, CooY+InPar.SemiLen],...
                                                   'Plot',InPar.Plot,'PlotPar',InPar.PlotPar,'Field',InPar.Field,...
                                                   'AxisType','y');
                    case 's'
                        % plot surface
                        cla;  % clear axis
                        Res(Ind).Res = local_surface(Image,[CooX, CooY],InPar.SemiLen,...
                                                     'Plot',InPar.Plot,'PlotPar',InPar.PlotPar,'Field',InPar.Field,...
                                                     'PlotFun',@surface);
                                                 
                    case 'c'
                        % plot contour
                        cla; % clear axis
                        Res(Ind).Res = local_surface(Image,[CooX, CooY],InPar.SemiLen,...
                                                     'Plot',InPar.Plot,'PlotPar',InPar.PlotPar,'Field',InPar.Field,...
                                                     'PlotFun',@contour);
                    case 'm'
                        % first and second moments
                        Res(Ind).Res = moments(Image,[CooX,CooY],...
                                               'Field',InPar.Field,'Radius',InPar.Radius,...
                                               'Sigma',InPar.Sigma,'MaxIter',InPar.MaxIter);
                        Table = astcat_array2table(Res(Ind).Res);
                        fprintf('Clicked position: X=%9.3f   Y=%9.3f\n',Res(Ind).CooX,Res(Ind).CooY);
                        fprintf('1st and 2nd moments (with %d iterations centering):\n',InPar.MaxIter);
                        disp(Table.Cat)
                    case 'r'
                        % radial profile with centering
                        ResM = moments(Image,[CooX,CooY],...
                                               'Field',InPar.Field,'Radius',InPar.Radius,...
                                               'Sigma',InPar.Sigma,'MaxIter',InPar.MaxIter);
                        X = col_get(ResM,'XWIN_IMAGE');
                        Y = col_get(ResM,'YWIN_IMAGE');
                        fprintf('Clicked position: X=%9.3f   Y=%9.3f\n',Res(Ind).CooX,Res(Ind).CooY);
                        fprintf('Centered position after %d iterations: X=%9.3f   Y=%9.3f\n',InPar.MaxIter,X,Y);
                        cla;  % clear axis
                        Res(Ind).Res = rad_prof(Image,[X, Y],...
                                                       InPar.Radius,...
                                                 'Plot',InPar.Plot,'PlotPar',InPar.PlotPar,...
                                                 'MeanFun',InPar.MeanFun,...
                                                 'Field',InPar.Field);
                        Res(Ind).Res.X = X;
                        Res(Ind).Res.Y = Y;
                        
                    case 'R'
                        % radial profile without centering
                        cla;  % clear axis
                        Res(Ind).Res = rad_prof(Image,[CooX, CooY],...
                                                       InPar.Radius,...
                                                 'Plot',InPar.Plot,'PlotPar',InPar.PlotPar,...
                                                 'MeanFun',InPar.MeanFun,...
                                                 'Field',InPar.Field);
                    case 'a'
                        % Aperture photometry with centering
                        
                        CatField = AstCat.CatField;
                        
                        ResM = moments(Image,[CooX,CooY],...
                                               'Field',InPar.Field,'Radius',InPar.Radius,...
                                               'Sigma',InPar.Sigma,'MaxIter',InPar.MaxIter);
                        X = col_get(ResM,'XWIN_IMAGE');
                        Y = col_get(ResM,'YWIN_IMAGE');
                        
                        fprintf('Clicked position: X=%9.3f   Y=%9.3f\n',Res(Ind).CooX,Res(Ind).CooY);
                        fprintf('Centered position after %d iterations: X=%9.3f   Y=%9.3f\n',InPar.MaxIter,X,Y);
                        
                        [Res(Ind).Res,ColAper] = aper_phot(Image,[X, Y],...
                                                 'AperRad',InPar.AperRad,...
                                                 'Annulus',InPar.Annulus);
                                             
                        Naper = numel(InPar.AperRad);
                        fprintf('Aperture photometry\n');
                        for Iaper=1:1:Naper
                            fprintf('  AperPhot Luptitude (rad=%6.2f): %6.3f +/- %6.3f\n',InPar.AperRad(Iaper),...
                                                 convert.flux2mag(Res(Ind).Res.(CatField)(1,ColAper.Aper(Iaper)),InPar.ZP),...
                                                 Res(Ind).Res.(CatField)(1,ColAper.AperErr(Iaper))./...
                                                 Res(Ind).Res.(CatField)(1,ColAper.Aper(Iaper)));
                            fprintf('  AperPhot flux (rad=%6.2f): %6.3f +/- %6.3f\n',InPar.AperRad(Iaper),...
                                                                             Res(Ind).Res.(CatField)(1,ColAper.Aper(Iaper)),...
                                                                             Res(Ind).Res.(CatField)(1,ColAper.AperErr(Iaper)));
                        end
                    case 'A'
                        % Aperture photometry without centering
                        CatField = AstCat.CatField;
                        
                        fprintf('Clicked position: X=%9.3f   Y=%9.3f\n',Res(Ind).CooX,Res(Ind).CooY);

                        
                        [Res(Ind).Res,ColAper] = aper_phot(Image,[CooX, CooY],...
                                                 'AperRad',InPar.AperRad,...
                                                 'Annulus',InPar.Annulus);
                                             
                        Naper = numel(InPar.AperRad);
                        fprintf('Aperture photometry\n');
                        for Iaper=1:1:Naper
                            fprintf('  AperPhot Luptitude (rad=%6.2f): %6.3f +/- %6.3f\n',InPar.AperRad(Iaper),...
                                                 convert.flux2mag(Res(Ind).Res.(CatField)(1,ColAper.Aper(Iaper)),InPar.ZP),...
                                                 Res(Ind).Res.(CatField)(1,ColAper.AperErr(Iaper))./...
                                                 Res(Ind).Res.(CatField)(1,ColAper.Aper(Iaper)));
                            fprintf('  AperPhot flux (rad=%6.2f): %6.3f +/- %6.3f\n',InPar.AperRad(Iaper),...
                                                                             Res(Ind).Res.(CatField)(1,ColAper.Aper(Iaper)),...
                                                                             Res(Ind).Res.(CatField)(1,ColAper.AperErr(Iaper)));
                        end
                                                                             

                    case 'p'
                        % PSF photometry with centering
                    case 'P'
                        % PSF photometry without centering
                        
                        CatField = AstCat.CatField;
                        
                        [Res(Ind).Res,ColPSF] = psf_phot(Image,[CooX, CooY]);
                                                 
                                             
                        fprintf('PSF photometry\n');
                        fprintf('  PSF phot: %6.3f +/- %6.3f\n',Res(Ind).Res.(CatField)(1,ColPSF.Flux),...
                                                                Res(Ind).Res.(CatField)(1,ColPSF.FluxErr));
                    case 'b'
                        % Estimate local background and noise
                        BackField = SIM.BackField;
                        ErrField  = SIM.ErrField;
                        
                        if (~isfield_populated(Image,BackField) || ~isfield_populated(Image,ErrField))
                            % populate the background field
                            Image = background(Image,InPar.BackPar{:});
                        end
                        
                        % get background
                        if (numel(Image.(BackField))==1)
                            Res(Ind).Res.Back = Image.(BackField);
                        else
                            Res(Ind).Res.Back = Image.(BackField)(round(CooY),round(CooX));
                        end
                        % get noise
                        if (numel(Image.(ErrField))==1)
                            Res(Ind).Res.Err = Image.(ErrField);
                        else
                            Res(Ind).Res.Err = Image.(ErrField)(round(CooY),round(CooX));
                        end
                        
                        fprintf('Clicked position: X=%9.3f   Y=%9.3f\n',Res(Ind).CooX,Res(Ind).CooY);
                        fprintf('  Background : %f\n',Res(Ind).Res.Back);
                        fprintf('  Noise      : %f\n',Res(Ind).Res.Err);
                        
                    case 'S'
                        % Return the nearest source found using mextractor
                        % check if catalog exist
                        CatField = AstCat.CatField;
                        if (~isfield_populated(Image,CatField))
                            Image = InPar.ExtractionFun(Image,InPar.ExtractionFunPar{:});
                        end
                        [AstC,Dist] = near_coo(Image,CooX,CooY,InPar.SearchRad,'RadiusUnits','pix');
                        D = col_get(Dist,'Dist');
                        [~,MinI] = min(D);
                        NearCat = row_select(Image,MinI);
                        % show catalog of nearest source
                        fprintf('  Nearest source\n');
                        show(NearCat);
                        
                    case 'e'
                        % Edit/set parameters (and show help for editing)
                        fprintf('Edit the internal ds9.imexam parameters\n');
                        fprintf('Possible parameters:\n');
                        fprintf('  Plot    - plot figure [default is true]\n');
                        fprintf('  AperRad - Aperture phot radii [default is [2 4 8 12 16]]\n');
                        fprintf('  Annulus - Annulus inner/outer radii [default is [16 22]]\n');
                        fprintf('  ZP      - Photometric ZP [default is 22]\n');
                        fprintf('  PlotPar - Plot parameters [default is {}]\n');
                        fprintf('  SemiLen - Line/surface semi length [default is 15]\n');
                        fprintf('  Radius  - Moments and radial radius [default is 7]\n');
                        fprintf('  MeanFun - Function for radial profile mean [default is @mean]\n');
                        fprintf('  Sigma   - Moments sigma of Gaussian weighting [default is 1.5]\n');
                        fprintf('  MaxIter - Number of moments iterations [default is 3]\n');
                        fprintf('  BackPar - Additional pars to pass to SIM/background [default is {}]\n');
                        fprintf('  ExtractionFun- Source extraction prog [default is @mextractor]\n');
                        fprintf('  ExtractionFunPar- Additional pars to pass to extraction prog [default is {}]\n');
                        fprintf('  SearchRad- Sources search radius [default is 50]\n');
                        fprintf('  Field   - SIM field on which to operate [default is Im]\n');
                        
                        Ans = input('Insert parameter and its value (e.g., "SearchRad=100") : ','s');
                        if (~isempty(Ans))
                            eval(sprintf('InPar.%s',Ans));
                        end
                                                                       
                    otherwise
                        fprintf('  Unknown ds9.imexam click option - try again\n');
                end
                        
                        
                        
                        
            
            
            end
        end
            
    end  % end methods
    
    % Interactive ploting
    % (imark, iline, ipoly)
   
    
    % Interactive coordinates to image in browser
    methods (Static)
        function [RA,Dec,Link]=sdssnavi(Browser)
            % Open SDSS navigator for clicked position
            % Package: @ds9
            % Description: Click on a position in an image displayed in ds9 and this
            %              program will open the SDSS navigator web page for the
            %              coordinates.
            % Input  : - Broweser type:
            %            'browser' - existing system browser (default).
            %            'new'     - Matlab internal browser.
            % Output : - J2000.0 RA of position [radians].
            %          - J2000.0 Dec of position [radians].
            %          - Link to SDSS navigator.
            % Required: XPA - http://hea-www.harvard.edu/saord/xpa/
            % Tested : Matlab 7.11
            %     By : Eran O. Ofek                    May 2011
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: [RA,Dec,Link]=ds9.sdssnavi;
            % Reliable: 2
            %------------------------------------------------------------------------------
            Def.Browser = 'browser';
            if (nargin==0)
               Browser = Def.Browser;
            elseif (nargin==1)
               % do nothing
            else
               error('Illegal number of input arguments');
            end

            % get RA/Dec
            fprintf('Click on coordinates to get SDSS navigator in browser\n');
            [RA,Dec]=ds9.getcoo(1);

            % get Link to SDSS navigator
            [Link] = VO.SDSS.navigator_link(RA,Dec);
            web(Link{1},sprintf('-%s',Browser));
            Link = Link{1};
        end

        function [RA,Dec,Link]=nedlink(Browser)
            % Open NED link for clicked position
            % Package: @ds9
            % Description: Click on a position in an image displayed in ds9 and this
            %              program will open the NED coordinate search web page for the
            %              coordinates.
            % Input  : - Broweser type:
            %            'browser' - existing system browser (default).
            %            'new'     - Matlab internal browser.
            % Output : - J2000.0 RA of position [radians].
            %          - J2000.0 Dec of position [radians].
            %          - Link to NED search page.
            % Required: XPA - http://hea-www.harvard.edu/saord/xpa/
            % Tested : Matlab 7.11
            %     By : Eran O. Ofek                    May 2011
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: [RA,Dec,Link]=ds9.sdssnavi;
            % Reliable: 2
            %------------------------------------------------------------------------------
            SearchRad = 3;  % arcmin
            Def.Browser = 'browser';
            if (nargin==0)
               Browser = Def.Browser;
            elseif (nargin==1)
               % do nothing
            else
               error('Illegal number of input arguments');
            end

            % get RA/Dec
            fprintf('Click on coordinates to get SDSS navigator in browser\n');
            [RA,Dec]=ds9.getcoo(1);

            % get Link to SDSS navigator
            [Link] = VO.NED.ned_link(RA,Dec,SearchRad);
            web(Link{1},sprintf('-%s',Browser));
            Link = Link{1};
        end

    end % methods
    
    % Interactive coordinate to local/external catalog
    methods (Static)
        function [Cat,RA,Dec]=sdsscat(SearchRadius)
            % Get SDSS catalog near clicked position
            % Package: @ds9
            % Description: Get SDSS catalog near clicked position
            % Input  : - Search radius [arcsec]. Default is 10.
            % Output : - Catalog containing SDSS sources near clicked
            %            position.
            %          - Clicked J2000.0 RA coordinate [rad].
            %          - Clicked J2000.0 Dec coordinate [rad].
            % Example: [RA,Dec,Cat]=ds9.sdsscat;
            % Reliable: 2
            
            RAD = 180./pi;
            
            Def.SearchRadius = 10;
            if (nargin==0)
                SearchRadius = Def.SearchRadius;
            end
            
            % get RA/Dec
            fprintf('Click on position to get SDSS catalog near coordinates\n');
            [RA,Dec]=ds9.getcoo(1);
            
            [Cat]=get_sdss(RA,Dec,SearchRadius./(RAD.*3600));
            
        end
        
        
        
        
    end % methods
    
    % Interact with SIM catalogs
    methods (Static)
        

        function [Cat,MinDist,Units,XRA,YDec,PixVal]=nearestcat(Sim,varargin)
            % Get the nearest source in a SIM/AstCat object
            % Package: @ds9
            % Description: Get the nearest source in a SIM/AstCat object
            %              to the clicked position.
            % Input  : - A SIM or AstCat object.
            %          * Arbitrary number of pairs of ...,key,val,...
            %            arguments. The following keywords are available:
            %            'ColX'  - Cell array of possible keywords
            %                      containing the X coordinate column name
            %                      on which to search.
            %                      The first existing column name will be
            %                      used.
            %                      Default is
            %                      {'XWIN_IMAGE','X','ALPHAWIN_J2000','RA'}
            %            'ColY'  - Like 'ColX' but for the Y coordinate.
            %                      Default is
            %                      {'YWIN_IMAGE','Y','DELTAWIN_J2000','Dec'}
            %            'ColUnits' - Units corresponding to 'ColX'.
            %                      Default is {'pix','pix','deg','ra'}.
            %            'SrcFun'  - Source extraction function to use.
            %                      Default is @mextractor
            %            'SrcPar'  - Cell array of additional parameters
            %                      to pass to the source extraction
            %                      function. Default is {}.
            %            'SearchRad' - Search radius [rad] or [pix].
            %                      Units defined by availability of column
            %                      in the catalog.
            %                      If empty then return only the nearest
            %                      source.
            %                      Default is empty.
            %            'Verbose' - Default is true.
            % Output : - AstCat object containing the nearest source to
            %            clicked position.
            %          - Distance to nearest source [rad] or [pix].
            %          - Units of RA/Dec (and distance).
            %          - RA or X coordinate of clicked position.
            %          - Dec or Y coordinate of clicked position.
            %          - Value at clicked position.
            % See also: ds9.nearcat
            % Example: [Cat,MinDist,RA,Dec]=ds9.nearestcat(Sim);
            % Reliable: 2
            
            if (~AstCat.isastcat(Sim))
                error('First input argument must be an AstCat object');
            end
            
            RAD = 180./pi;
            CatField     = AstCat.CatField;
            ColCellField = AstCat.ColCellField;
            
            DefV.ColX                = {'XWIN_IMAGE','X','ALPHAWIN_J2000','RA'};
            DefV.ColY                = {'YWIN_IMAGE','Y','DELTAWIN_J2000','Dec'};
            DefV.ColUnits            = {'pix','pix','deg','rad'};
            DefV.SrcFun              = @mextractor;
            DefV.SrcPar              = {};
            DefV.SearchRad           = [];
            DefV.Verbose             = true;
            InPar = InArg.populate_keyval(DefV,varargin,mfilename);
            
            if (~isfield_populated(Sim,ColCellField))
                % Cat is not populated
                % run mextractor
                fprintf('Catalog is not populated - extract sources\n');
                Sim = InPar.SrcFun(Sim,InPar.SrcPar{:});
            end
            [~,ColIndX,UseIndX] = select_exist_colnames(Sim,InPar.ColX(:));
            [~,ColIndY]         = select_exist_colnames(Sim,InPar.ColY(:));
            ColUnits            = InPar.ColUnits{UseIndX};
            
            X = col_get(Sim,ColIndX);
            Y = col_get(Sim,ColIndY);
            switch lower(ColUnits)
                case 'deg'
                    X = X./RAD;
                    Y = Y./RAD;
                    ColUnits = 'rad';
                otherwise
                    % do nothing - already in pix or rad
            end
            
            Cont = true;
            Ind  = 0;
            while Cont
                Ind = Ind + 1;
                switch lower(ColUnits)
                    case {'deg','rad'}
                        % get RA/Dec - spherical coordinates
                        fprintf('Click on position to get SIM catalog near coordinates - q to abort\n');
                        [RA,Dec,Val,Key]=ds9.getcoo(1);

                        D = sphere_dist(X,Y,RA,Dec);
                    case 'pix'
                        % get X/Y - planner coordinates
                        fprintf('Click on position to get SIM catalog near coordinates - q to abort\n');
                        [RA,Dec,Val,Key]=ds9.getpos(1);

                        D = tools.math.geometry.plane_dist(X,Y,RA,Dec);
                    otherwise
                        error('Unknown ColUnits option');
                end
                
                switch lower(Key{1})
                    case 'q'
                        % abort
                        Cont = false;
                    otherwise
                        [~,MinInd]   = min(D);
                        MinDist(Ind) = D(MinInd);
                        if (isempty(InPar.SearchRad))
                            % found sources
                            IndF     = MinInd;
                        else
                            IndF     = D<InPar.SearchRad;
                        end
                        Cat(Ind)     = row_select(Sim,IndF);
                        Units{Ind}   = ColUnits;
                        XRA(Ind)     = RA;
                        YDec(Ind)    = Dec;
                        PixVal(Ind)  = Val;

                        if (InPar.Verbose)
                            fprintf('\n');
                            fprintf('Clicked position:  %f %f [%s]\n',RA,Dec,Units{Ind})
                            fprintf('Distance: %f [%s]\n',MinDist(Ind),Units{Ind});
                            fprintf('Clicked position value: %f\n',PixVal(Ind));
                            fprintf('-----------------------\n');
                            disp(array2table(Cat(Ind).(CatField),'VariableNames',Cat(Ind).(ColCellField)))
                        end
                end
            end
        end
        
        
        function [Cat,MinDist,Units,XRA,YDec,PixVal]=nearcat(Sim,SearchRad,varargin)
            % Get the nearest source in a SIM/AstCat object
            % Package: @ds9
            % Description: Get the nearest source in a SIM/AstCat object
            %              to the clicked position.
            % Input  : - A SIM or AstCat object.
            %          - Search radius. Default is 100 (in units of queried
            %            column).
            %          * Arbitrary number of pairs of ...,key,val,...
            %            arguments. The following keywords are available:
            %            'ColX'  - Cell array of possible keywords
            %                      containing the X coordinate column name
            %                      on which to search.
            %                      The first existing column name will be
            %                      used.
            %                      Default is
            %                      {'XWIN_IMAGE','X','ALPHAWIN_J2000','RA'}
            %            'ColY'  - Like 'ColX' but for the Y coordinate.
            %                      Default is
            %                      {'YWIN_IMAGE','Y','DELTAWIN_J2000','Dec'}
            %            'ColUnits' - Units corresponding to 'ColX'.
            %                      Default is {'pix','pix','deg','ra'}.
            %            'SrcFun'  - Source extraction function to use.
            %                      Default is @mextractor
            %            'SrcPar'  - Cell array of additional parameters
            %                      to pass to the source extraction
            %                      function. Default is {}.
            %            'Verbose' - Default is true.
            % Output : - AstCat object containing the nearest source to
            %            clicked position.
            %          - Distance to nearest source [rad] or [pix].
            %          - Units of RA/Dec (and distance).
            %          - RA or X coordinate of clicked position.
            %          - Dec or Y coordinate of clicked position.
            %          - Value at clicked position.
            % See also: ds9.nearcat
            % Example: [Cat,MinDist,RA,Dec]=ds9.nearcat(Sim,100);
            % Reliable: 2
          
            
            RAD = 180./pi;
            CatField     = AstCat.CatField;
            ColCellField = AstCat.ColCellField;
            
            if (nargin==1)
                SearchRad = 100;
            end
            
            DefV.ColX                = {'XWIN_IMAGE','X','ALPHAWIN_J2000','RA'};
            DefV.ColY                = {'YWIN_IMAGE','Y','DELTAWIN_J2000','Dec'};
            DefV.ColUnits            = {'pix','pix','deg','rad'};
            DefV.SrcFun              = @mextractor;
            DefV.SrcPar              = {};
            DefV.Verbose             = true;
            InPar = InArg.populate_keyval(DefV,varargin,mfilename);
            
            [Cat,MinDist,Units,XRA,YDec,PixVal] = ds9.nearestcat(Sim,varargin{:},'SearchRad',SearchRad);

        end
        
        
        
    end % methods
    
    % Interact with SIM images
    methods (Static)
        
        function Res=simval(Sim)
            % Interactively get values from SIM images
            % Package: @ds9
            % Description: Interactively get values from SIM images (image,
            %              background, error, weight and mask) at clicked
            %              positions. Click 'q' to abort.
            % Input  : - A single element SIM object.
            % Output : - A structure array with the results: clicked
            %            positions and pixel values.
            % Example: Res=simval(Sim);
            % Reliable:
            
            Verbose = true;
            
            ImageField  = SIM.ImageField;
            BackField   = SIM.BackField;
            ErrField    = SIM.ErrField;
            WeightField = SIM.WeightField;
            MaskField   = MASK.MaskField;
            
            Cont = true;
            Ind  = 0;
            while Cont
                % get info from mouse/key click
                fprintf('Click on pixel position - q to abort\n');
                [X,Y,Val,Key]=ds9.getpos(1);
                
                if (strcmpi(Key{1},'q'))
                    % abort
                    Cont = false;
                else
                    % Continue
                    Ind = Ind + 1;
                    
                    Res(Ind).X   = X;
                    Res(Ind).Y   = Y;
                    Res(Ind).Val = Val;
                    Res(Ind).Key = Key{1};
                    FX = floor(X);
                    FY = floor(Y);
                    
                    % get SIM data
                    % Image
                    if (isfield_populated(Sim,ImageField))
                        FI = sub2ind(size(Sim.(ImageField)),FY,FX);
                        Res(Ind).ImageVal = Sim.(ImageField)(FI);
                    else
                        Res(Ind).ImageVal = [];
                    end
                    % background
                    if (isfield_populated(Sim,BackField))
                        if (numel(Sim.(BackField))==1)
                            FI = 1;
                        else
                            FI = sub2ind(size(Sim.(BackField)),FY,FX);
                        end
                        Res(Ind).BackVal = Sim.(BackField)(FI);
                    else
                        Res(Ind).BackVal = [];
                    end
                    % error
                    if (isfield_populated(Sim,ErrField))
                        if (numel(Sim.(ErrField))==1)
                            FI = 1;
                        else
                            FI = sub2ind(size(Sim.(ErrField)),FY,FX);
                        end
                        Res(Ind).ErrVal = Sim.(ErrField)(FI);
                    else
                        Res(Ind).ErrVal = [];
                    end
                    % weight
                    if (isfield_populated(Sim,WeightField))
                        if (numel(Sim.(WeightField))==1)
                            FI = 1;
                        else
                            FI = sub2ind(size(Sim.(WeightField)),FY,FX);
                        end
                        Res(Ind).WeightVal = Sim.(WeightField)(FI);
                    else
                        Res(Ind).WeightVal = [];
                    end
                    % mask
                    if (isfield_populated(Sim,MaskField))
                        FI = sub2ind(size(Sim.(MaskField)),FY,FX);
                        Res(Ind).MaskVal = Sim.(MaskField)(FI);
                        % get bit mask name
                        Res(Ind).BitNames = mask2bitname(Sim);
                    else
                        Res(Ind).MaskVal  = [];
                        Res(Ind).BitNames = {};
                    end
                    
                    % show report
                    if (Verbose)
                        fprintf('\n');
                        fprintf('Clicked pixel: %f %f\n',X,Y);
                        fprintf('ds9 image value: %f\n',Val);
                        if (~isempty(Res(Ind).ImageVal))
                            fprintf('SIM image value: %f\n',Res(Ind).ImageVal);
                        end
                        if (~isempty(Res(Ind).BackVal))
                            fprintf('SIM background value: %f\n',Res(Ind).BackVal);
                        end
                        if (~isempty(Res(Ind).ErrVal))
                            fprintf('SIM error value: %f\n',Res(Ind).ErrVal);
                        end
                        if (~isempty(Res(Ind).WeightVal))
                            fprintf('SIM weight value: %f\n',Res(Ind).WeightVal);
                        end
                        if (~isempty(Res(Ind).MaskVal))
                            fprintf('SIM mask value: %f\n',Res(Ind).MaskVal);
                            fprintf('   List of bit mask names\n',Res(Ind).MaskVal);
                            for Im=1:1:numel(Res(Ind).BitNames)
                                fprintf('   %s\n',Res(Ind).BitNames{Im});
                            end
                        end
                        fprintf('\n');
                    end
                    
                end
                
            end
        end
        
        
        function Result = supported()
            % Return true if ds9 is supported (currently only Linux and Mac)
            Result = isunix || ismac;
        end
    end

    
    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for ds9

    end
    
    
end % end class
            

function makeMovie(Images, FileName, Args)
    % Display and create a movie file from images in AstroImage or cube.
    % Input  : - Either a cube in which the image index is in the 3rd
    %            dimension, or an AstroImage object.
    %          - FileName to save.
    %            If empty, only display movie.
    %            Default is [].
    %          * ...,key,val,...
    %            'DataProp' - Data property in the AstroImage from which to
    %                   create the movie. Default is 'Image'.
    %            'ColorMap' -colormap option. Default is 'gray'.
    %            'RemoveTicks' - Remove ticks from plot. Default is true.
    %            'Colorbar' - Add colorbar. Default is true.
    %            'VideoType' - See VideoWriter for options.
    %                   Default is 'Motion JPEG AVI'.
    %            'VideoType' - Frame rate. Default is 30.
    %            'VideoArgs' - A cell array of additional arguments to pass
    %                   to VideoWriter. Default is {}.
    % Output : null
    % Author : Eran Ofek (Sep 2022)
    % Example: imProc.stack.makeMovie(AI);
    
    arguments
        Images    % either a cube or an AstroImage object
        FileName      = [];  %'Movie.avi';
        Args.Scale    = [-10 20];
        Args.DataProp = 'Image';
        Args.ColorMap = 'gray';
        Args.RemoveTicks logical  = true;
        Args.Colorbar logical     = true;
        Args.VideoType            = 'Motion JPEG AVI';
        Args.FrameRate            = 30;
        Args.VideoArgs            = {};
    end
    Dim = 3;
    
    if isa(Images, 'AstroImage')
        IsAI = true;
        Nim = numel(Images);
    else
        IsAI = false;
        Nim = size(Images, Dim);
    end
    
    if ~isempty(FileName)
        VideoObj = VideoWriter(FileName, Args.VideoType, Args.VideoArgs{:}, 'FrameRate',Args.FrameRate);
        open(VideoObj);
    end
    
    for Iim=1:1:Nim
        if IsAI
            Matrix = Images(Iim).(Args.DataProp);
        else
            Matrix = Images(:,:,Dim);
        end
            
        imagesc(Matrix, Args.Scale);
        colormap(Args.ColorMap);
        drawnow;
        if Args.RemoveTicks
            H = gca;
            H.XTick = [];
            H.YTick = [];
        end
        if Args.Colorbar
            Hcb = colorbar;
        end
        
        if ~isempty(FileName)
            Im = getframe;
            writeVideo(VideoObj, Im);
        end
    end
    
    if ~isempty(FileName)
        close(VideoObj);
    end
    
end

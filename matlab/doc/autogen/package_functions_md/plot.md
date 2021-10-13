# Package: plot


### plot.axis01

Plot an axis with grid on X=0 and Y=0 only. Package: plot Description: Create an axis that have only an the 0 X and Y axes.


    
    Plot an axis with grid on X=0 and Y=0 only.  
    Package: plot  
    Description: Create an axis that have only an the 0 X and Y axes.  
    Input  : * Arbitrary pairs of arguments, ...,keyword,value,...  
    Options:  
    'MaxX'          - X range max, default is 1.  
    'MaxY'          - Y range max, default is 1.  
    'MinX'          - X range min, default is -1.  
    'MinY'          - Y range min, default is -1.  
    'NumberOfTickX' - Number of Ticks between 0 and MaxX,  
    default is 10.  
    'NumberOfTickY' - Number of Ticks between 0 and MaxY,  
    default is 10.  
    'TickSizeFrac'  - Ticks Size fraction as measured relative  
    to MaxX/MaxY, default is 0.025.  
    Output : null  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: aplot.xis01; hold on; plot(rand(100,1),rand(100,1),'.');  
    plot.axis01('MaxX',2.5,'NumberOfTickY',5); hold on;  
    Reliable: 2  
      
      
### plot.cell_contourc

cell_contourc function                                          plotting Description: A contourc-like program that return a cell array of the contour levels.


    
      
    cell_contourc function                                          plotting  
    Description: A contourc-like program that return a cell array of the  
    contour levels.  
    Input  : * See contourc for available input arguments.  
    Output : - Structure containing two elements:  
    .L - containing the contour levels  
    .C - Cell array, for each level, containing [X,Y] for the  
    contours.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                   January 2008  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
      
### plot.contour_levels

Break a contour map into seperate contour levels. Package: plot Description: Given a contour map (the first output argument of contour) return a structure array in which each element contains one seperated contour.


    
    Break a contour map into seperate contour levels.  
    Package: plot  
    Description: Given a contour map (the first output argument of contour)  
    return a structure array in which each element contains one  
    seperated contour.  
    Input  : - Countour map.  
    Output : - Structure array of leveles.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: L=plot.contour_levels(C)  
    Reliable:  
      
      
      
      
### plot.contour_percentile

Contour plot in which the contours are percentiles of the matrix sum. Package: plot Description: Given a matrix, generate a contour plot in which the contours represent the percentiles of the total matrix sum. I.e., the region encompassed within the contour contains the given


    
    Contour plot in which the contours are percentiles of the matrix sum.  
    Package: plot  
    Description: Given a matrix, generate a contour plot in which the contours  
    represent the percentiles of the total matrix sum. I.e., the  
    region encompassed within the contour contains the given  
    percentile of the matrix total sum.  
    Input  : - Vector of X  
    - Vector of Y  
    - Matrix containing the number of counts in each cell.  
    - Percentiles levels, default is [0.6827 0.9545 0.9973].  
    - Optional line type (e.g., 'k-'), default is to use  
    contour.m defaults.  
    Output : - Matrix containing the cumulative probability in each cell.  
    - Contour matrix as described in contourc.m  
    - Handle to a contourgroup object. This handle can  
    be used as input to clabel.m  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                    Jun 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Mat,C,H]=plot.contour_percentile((1:1:10),(1:1:10),randi(100,10))  
    Reliable: 2  
      
      
### plot.date_axis

Add date upper axis. Package: plot Description: Given a graph in which the lower x-axis shows the Julian day, add a date-axis in the upper x-axis that corresponds to the Julian day.


    
    Add date upper axis.  
    Package: plot  
    Description: Given a graph in which the lower x-axis shows the Julian day,  
    add a date-axis in the upper x-axis that corresponds to the  
    Julian day.  
    Input  : - Number of dates to label (default is 6).  
    - JD Offset (i.e. If MJD are used then this should be 2400000.5  
    - Font Size (default is 10).  
    - Rotation (default is 90).  
    - Color, default is 'k'.  
    Output : null  
    See also: julday.m, julday1.m jd2date.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                   January 2000  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: plot(MJD,Mag); date_axis(10,2450000,12,45);  
    Reliable: 1  
      
### plot.distBetweenPoints

Interactively calculate distances between points on 2D plot


    
    Interactively calculate distances between points on 2D plot  
    Input  : - Optional number of mouse keys to strike.  
    If not provided, then will stop when "return" key is  
    pressed.  
    Output : - Distance  
    - Theta radians (defined as atan2(DY, DX))  
    - X  
    - Y  
    Author : Eran Ofek (Aug 2021)  
    Example: plot(1,1); [Dist, Theta, X, Y] = plot.distBetweenPoints;  
      
### plot.draw_iline

Draw line interactively. Package: plot Description: Draw line interactively. Click the mouse right bottom in start point and at end point. Click left mouse button to exit this program.


    
    Draw line interactively.  
    Package: plot  
    Description: Draw line interactively. Click the mouse right bottom in  
    start point and at end point. Click left mouse button  
    to exit this program.  
    Input  : - Line type and color (default is 'r-').  
    - Line width (default is 0.5).  
    Output : - Column vector of Start points [x,y].  
    - Column vector of End points [x,y].  
    - Vector of line handels.  
    Tested : Matlab 5.1  
    By : Eran O. Ofek                    Oct 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
      
### plot.errorxy

Plot graphs with error bars in both axes. Package: plot Description: Plot graphs with error bars in both axes.


    
    Plot graphs with error bars in both axes.  
    Package: plot  
    Description: Plot graphs with error bars in both axes.  
    Input  : - Data matrix.  
    * an arbitrary pairs of keyword and value parameters, where  
    keyword is one of the followings:  
    (In case that the second parameter is a number then call the  
    old version errorxy_o).  
    'ColX'     - Column number containing X values, default is 1.  
    'ColY'     - Column number containing Y values, default is 2.  
    'ColXe'    - Column number containing error X values.  
    If one parameter is given then assume upper  
    and lower errors are the same, if two values  
    are given then the first is the left and the  
    second is the right errors,  
    default is NaN (no X errors).  
    'ColYe'    - Column number containing error Y values.  
    If one parameter is given then assume right  
    and left errors are the same, if two values  
    are given then the first is the lower and the  
    second is the upper errors,  
    default is 3.  
    If one of the two sided error is NaN, then plot  
    an upper/lower limit arrow.  
    'Marker'   - Marker type, see plot for options, default is 'o'.  
    'MarkSize' - Marker size, default is 6.  
    'EdgeColor'- Marker edge color, default is 'b'.  
    'FaceColor'- Marker face color, default is NaN.  
    'ColorEB'  - Error bars color, default is the same as 'EdgeColor'.  
    'LineEB'   - Error bars line type, default is '-'.  
    'WidthEB'  - Error bars line width, default is 0.5.  
    'EdgeEB'   - Length [ticks] of error bar edge, default is 0.  
    'Con'      - Connect data points {'y' | 'n'}, default is 'n'.  
    'ConColor' - Color of Connecting line, default is the  
    same as 'EdgeColor'  
    'ConLine'  - Type of connecting line, default is '-'.  
    'ConWidth' - Width of connecting line, default is 0.5.  
    'Hist'     - Plot histogram {'y' | 'n'}, default is 'n'.  
    'HistFaceColor'- Histogram face color, default is the same  
    as 'EdgeColor'.  
    'HistEdgeColor'- Histogram edge color, default is the same  
    as 'EdgeColor'.  
    'ScaleX'   - X scale {'linear' | 'log'}, default is 'linear'.  
    'ScaleY'   - Y scale {'linear' | 'log'}, default is 'linear'.  
    'DirX'     - X direction {'normal' | 'reverse'},  
    default is 'normal'.  
    'DirY'     - Y direction {'normal' | 'reverse'},  
    default is 'normal'.  
    Output : null  
    Plot   : errorxy plot  
    See also: errorxy_o.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Mar 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: errorxy(Data,'ColYe',[5 6],'Marker','^');  
    Reliable: 2  
      
### plot.generate_colors

Generate equally spaced N colors from a given color map. Package: plot Description: Generate equally spaced N colors from a given color map.


    
    Generate equally spaced N colors from a given color map.  
    Package: plot  
    Description: Generate equally spaced N colors from a given color map.  
    Input  : - Number of requested colors.  
    - Color map. Default is 'jet'.  
    Output : - A matrix in which each row is an RGB color triplet.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Colors=generate_colors(5);  
    Reliable: 2  
      
      
      
### plot.gno

Get the nearest object handle Package: plot Description: Get the nearest object handle. The user click near a a figure objects, and the function return their handles and coordinates.


    
    Get the nearest object handle  
    Package: plot  
    Description: Get the nearest object handle. The user click near a  
    a figure objects, and the function return their handles  
    and coordinates.  
    Left click for object selection.  
    Right click for exiting program.  
    Input  : - Handle to axis from which to select objects. If empty,  
    use current axis. Default is empty matrix.  
    * If empty than the nearest selected object will not be marked.  
    Otherwise, this can be symbol and color properties that  
    will be transfered to the plot function  
    (e.g., 'rx','MarkerSize',12). Default is empty matrix.  
    Output : - Row vector of all objects handle.  
    - Row vector of X coordinates of selected objects.  
    - Row vector of Y coordinates of selected objects.  
    Tested : Matlab 5.2  
    By : Eran O. Ofek                    Oct 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Ho,Xo,Yo]=gno;  
    [Ho,Xo,Yo]=gno(gca);  
    [Ho,Xo,Yo]=gno([],'rx','MarkerSize',12);  
    Reliable: 2  
      
### plot.graph

Plot second column as a function first column. Package: plot Description: Given a two column matrix, plot the second column as a function of the first column. The function may get additional argument like the plot command.


    
    Plot second column as a function first column.  
    Package: plot  
    Description: Given a two column matrix, plot the second column as a  
    function of the first column. The function may get additional  
    argument like the plot command.  
    Input  : - Matrix containing at least two columns.  
    * Arbitrary number of parameters to send to the plot command.  
    Output : - Handle for the objects plotted.  
    Plot   : A plot of the second column of the input matrix as a  
    function of the first column.  
    Tested : Matlab 3.5  
    By : Eran O. Ofek                    Dec 1993  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: graph(rand(10,2));  
    Reliable: 1  
      
      
### plot.hist_ofplot

calculate histograms of the X and Y axis of the data in current figure. Package: plot Description: Given an existing plot, calculate histograms of the X and Y axis of the data in current figure axis. The x-axis histogram is displayed below the current plot, and the y-axis histogram


    
    calculate histograms of the X and Y axis of the data in current figure.  
    Package: plot  
    Description: Given an existing plot, calculate histograms of the X and Y  
    axis of the data in current figure axis. The x-axis histogram  
    is displayed below the current plot, and the y-axis histogram  
    is dispalyed on the right side.  
    Input  : * Arbitrary number of pairs of ...,keyword,value,...  
    Avaliable keywordsare:  
    'NbinX'     - Number of histograms bin in X axis.  
    Default is 30.  
    'NbinY'     - Number of histograms bin in X axis.  
    Default is 30.  
    'HistHight' - Fraction of figure occupying histogram hight.  
    Default is 0.15.  
    'Gap'       - Gap between histograms and plot, default is 0.01.  
    'Scale'     - Scale of histogram N-axis.  
    'linear' - linear scale (default).  
    'log'    - log scale.  
    'logdata'- plot log of N.  
    'Norm'      - Histogram normalization:  
    'no'  - no normalization (default).  
    'sum' - by sum.  
    'max' - by max.  
    'Color'     - A flag indicating if to use the color of each  
    children object or to use the same color  
    for all the childrens.  
    If empty matrix then will use the color of each  
    children (default). Otherwise this is the color  
    to use for all the histograms.  
    'Plot'      - Plot hisogram (true) or only plot axes (false).  
    Default is true.  
    Output : - Three element vectors containing handles to:  
    [main plot, X-axis hist, Y-axis hist].  
    Output : - Handle to axes [main plot, x histogram, y histogram]  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jul 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: plot(randn(10000,1).*0.2+round(rand(10000,1)),randn(10000,1).*0.5,'k.','MarkerSize',4);  
    hold on  
    plot(rand(1000,1).*0.3,randn(1000,1).*0.5,'r.','MarkerSize',4);  
    [H]=hist_ofplot('Norm','sum');  
    set(get(H(2),'Ylabel'),'String','Frac.','FontSize',16);  
    set(get(H(3),'Xlabel'),'String','Frac.','FontSize',16);  
    Web example: http://adsabs.harvard.edu/abs/2012PASP..124...62O  
    Reliable: 1  
      
      
### plot.hist_stairs

Plot an histogram using stairs plot. Package: plot Description: Plot an histogram using stairs plot.


    
    Plot an histogram using stairs plot.  
    Package: plot  
    Description: Plot an histogram using stairs plot.  
    Input  : - X.  
    - Y.  
    - Marker, default is 'k-'.  
    * Arbitrary number of pairs of ...,keyword,value,...  
    Avaliable keywords are:  
    'Type'  - {'v'|'h'} - for horizontal or vertical histogram.  
    Default is 'v'.  
    Output : - Handle to stairs plot.  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jul 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    -  
### plot.insert_image

Insert an image to a matlab figure in a given position. Package: plot Description: Insert an image to a matlab figure in a given position.


    
    Insert an image to a matlab figure in a given position.  
    Package: plot  
    Description: Insert an image to a matlab figure in a given position.  
    Input  : - X image position.  
    - Y image position.  
    - Image name or matrix.  
    - Image Siez [X Y].  
    - Display range [LOW HIGH], default is [0 1].  
    - Position scheme:  
    'cen'  : place the image center at the X/Y position (default).  
    'cor'  : place the image bottom left corner at the X/Y position.  
    - Optional alpah image, default is no alpha image.  
    Output : - Handle for the axis containing the image.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
    -  
      
### plot.invy

Invert the y-axis of the current axis. Package: plot Description: Invert the y-axis of the current axis. This is a shortcut command to axis('ij') and set(gca,'YDir','Inverse').


    
    Invert the y-axis of the current axis.  
    Package: plot  
    Description: Invert the y-axis of the current axis.  
    This is a shortcut command to axis('ij') and  
    set(gca,'YDir','Inverse').  
    Input  : null  
    Output : null  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                    Feb 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
### plot.loghist

Plot an histogram in log space. Package: plot Description: Plot an histogram in log space.


    
    Plot an histogram in log space.  
    Package: plot  
    Description: Plot an histogram in log space.  
    Input  : - Vector of values for which to plot the histogram.  
    - log10 of histogram starting value  
    - log10 of histogram ending value  
    - Number of bins.  
    - Color, default is 'w'.  
    * Additional patch properties (...,keyword,value,...)  
    Output : - X Edges of bins.  
    - Number of events in each bin.  
    - Vector of handles to each patch.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=logspace(5,10,1000)'.*(randn(1000,1)+1).*100;  
    loghist(X,5,12,8,'r');  
    [Edges,N,H]=plot.loghist(10.^(rand(10000,1)),-5, 1,10,'w',...  
    'FaceColor','r');  
    -  
### plot.loglogneg

A loglog plot in which negative values are ploted at -log(abs(val)) Package: plot Description: A log log plot that allows plotting negative numbers.


    
    A loglog plot in which negative values are ploted at -log(abs(val))  
    Package: plot  
    Description: A log log plot that allows plotting negative numbers.  
    For example 1,-1 will be plotted at log10(1), -log10(abs(-1))  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [H]=plot.loglogneg(AccR,AccD,'.','MarkerSize',1);  
    Reliable:  
      
      
      
      
      
### plot.multi_axis

Create additional x or y axis related by function existing axes. Package: plot Description: Create additional x or y axis which is related to the exsiting axis by a given function.


    
    Create additional x or y axis related by function existing axes.  
    Package: plot  
    Description: Create additional x or y axis which is related to  
    the exsiting axis by a given function.  
    Input  : - Which axis to add: {'x' | 'y'}.  
    If 'x', then new axis is added on top.  
    If 'y', then new axis is added on right.  
    - Function (inline object or function name as string),  
    that relates the two axes.  
    (The function should be monotonic!).  
    * Arbitrary number of additional (optional) parameters  
    to pass to "Function".  
    Output : - Handle for new axis.  
    - Handle for current (old) axis.  
    - Handle for the completed box without ticks around figure.  
    Plot   : Add upper x-axis or left y-axis to the plot.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Sep 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: F=inline('1./x','x');    monotonic function  
    plot(rand(10,2));  
    [H2,H1]=multi_axis('x',F);  
    F=inline('(x.*10).^2+5+a','x','a');  
    [H3,H4]=multi_axis('y',F,100);  
    xlabel(H1,'axis1')  
    xlabel(H2,'axis2')  
    ylabel(H3,'axis3')  
    ylabel(H4,'axis4')  
    Reliable: 1  
      
      
### plot.noncont_axis

- noncont_axis function                                    plotting Description: Create non continuous axis.


    
    -  
    noncont_axis function                                    plotting  
    Description: Create non continuous axis.  
    Input  : - Axis type {'x' | 'y'}, default is 'x'. Set the  
    non continuous axis.  
    - Data range in each one of the noncontinuus axes  
    [Min Max; Min Max;...].  
    - Data range of axes of continuus axis [Min Max].  
    * Aribtrary number of pairs of input arguments,  
    ...,keyword,value,...  
    'XScale'    - {'linear','log'}, default is 'linear'.  
    'YScale'    - {'linear','log'}, default is 'linear'.  
    'AxesPosX'  - position of X-axes bounderies,  
    default is [0.130 0.905].  
    'AxesPosY'  - position of Y-axes bounderies,  
    default is [0.110 0.925].  
    'Space'     - Spaceing between axes, default is 0.03.  
    Output : - Vector of handles for axes.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek        October 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: Hax=noncont_axis('x',[10   70;  872 890],...  
    [1 100],'YScale','log');  
    To change current axis use:  
    set(gcf,'CurrentAxes',Hax(1))  
    -  
### plot.patch_band

patch_band function                                             plotting Description: Given X and Y positions describing a line, plot a band (i.e., using the patch command), around the line, where the patch is defined to have a given height below and


    
      
    patch_band function                                             plotting  
    Description: Given X and Y positions describing a line, plot a band  
    (i.e., using the patch command), around the line, where  
    the patch is defined to have a given height below and  
    above the line.  
    Input  : - X  
    - Y  
    - Semi width of patch in Y direction  
    - Color, default is [0.8 0.8 0.8].  
    Output : - Patch handle  
    Tested : Matlab 7.8  
    By : Eran O. Ofek                    Nov 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=[0:1:10]'; Y=X.^2; WidthY = 1;  H=patch_band(X,Y,WidthY);  
    Reliable: 2  
      
### plot.plot3bar

- plot3bar function                                            plotting Description: plot 3-Dim graph with line connecting the points to the z=0 surface.


    
    -  
    plot3bar function                                            plotting  
    Description: plot 3-Dim graph with line connecting the points to  
    the z=0 surface.  
    Input  : - vector of x data.  
    - vector of y data.  
    - vector of z data.  
    - the point symbol.  
    Output : null  
    Tested : Matlab 5.1  
    By : Eran O. Ofek           June 1997  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    -  
### plot.plot_compass

- plot_compass function                                        plotting Description: Plon a compass on a plot or image.


    
    -  
    plot_compass function                                        plotting  
    Description: Plon a compass on a plot or image.  
    Input  : - Compass position [X, Y].  
    - Compass arms length [pixels].  
    - Color, default is 'k';  
    - Angle of compass north in deg.  
    Default is 0. (north is up)  
    - Compass north east direction:  
    -1 : north-east direction is clockwise.  
    +1 : north-east direction is counter-clockwise.  
    Default is +1.  
    Output : - Handle for the compass line.  
    - Handles for the text.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
    -  
### plot.plot_corner

plot_corner function                                              plotting Description: Plot corners which lines are parallel to the axes.


    
      
    plot_corner function                                              plotting  
    Description: Plot corners which lines are parallel to the axes.  
    Input  : - Vector of X points of the corners.  
    - Vector of Y points of the corners.  
    - Vector of X length of the corners.  
    - Vector of Y length of the corners.  
    - Line type, default is 'k-'.  
    * Arbitrary of number of input arguments to be passed to the  
    plot command.  
    Output : - Vector of handels for each corner lines.  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                  November 2010  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
      
### plot.plot_cs

plot_cs function                                                plotting Description: Plot colored symbols.


    
      
    plot_cs function                                                plotting  
    Description: Plot colored symbols.  
    Input  : - Vector of X.  
    - Vector of Y.  
    * Arbitrary number of pairs of input parameters: key,val,...  
    Available keywords are:  
    'Color' - Symbols edge color. This may be a 3-element  
    raw vector of a single color to apply to all  
    the data points; A 3-column matrix in which each  
    raw is the color of one data point; or a column  
    vector of numbers in the range 0 to 1 that will  
    be map to colors using the provided ColorMap.  
    Default is [0 0 1].  
    'MarkerFaceColor' - Symbols face colors. Like Color, but for  
    the symbol face. Default is [0 0 1].  
    'ColorMap' - 3 column matrix of color map. Default is the  
    default color map.  
    'Marker' - Marker type, or a cell array of marker types  
    per data point. Default is 'o'. Alternatively, this  
    can be an index for a marker type indicated by  
    'MarkerMap'.  
    'MarkerMap' - If 'Marker' is numeric than it will be  
    taken from MarkerMap. Default is  
    {'.'; 'o'; 'x'; '+'; '*'; 's'; 'd'; 'v'; '^'; '<';  
    '>'; 'p'; 'h'}.  
    For Example Marker=2 will set the Marker to 'o'.  
    'MarkerSize' - Marker size, or a vector of marker sizes per  
    element. Default is 8.  
    'InterpMethod' - Color map interpolation method.  
    Default is 'linear'.  
    'Hold' - hold on current figure. Default is false. This will  
    not override hold on made prior to the function call.  
    'HoldEnd' - hold on the final figure. Default is false.  
    Output : - Vecor of handels for each symbol.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: H=plot_cs(rand(100,1),rand(100,1),'Color',rand(100,3),'MarkerFaceColor',rand(100,3),'MarkerSize',rand(100,1).*10)  
    Reliable: 2  
      
      
### plot.plot_ellipse

plot_ellipse function                                           plotting Description: Plot an ellipse with given properties.


    
      
    plot_ellipse function                                           plotting  
    Description: Plot an ellipse with given properties.  
    Input  : - Position [X,Y] of ellipse center within current axis.  
    - [Semi Major axis, Semi Minor axis]  
    - Eccentricity. If given (e.g., not empty []) then Minor axis  
    is calculated from major axis and eccentricity.  
    - PA [rad], measured from X axis counterclocwise.  
    - Color, default is [0 0 0] (e.g., 'k', black).  
    - Line Width, default is 1.  
    - if cos(Dec) is given then stretch X axis by 1/cos(Dec),  
    default is 1.  
    - Ellipse face color, default is 'none'.  
    Output : - Ellipse handle  
    Plot   : - An ellipse plot  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Mar 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: H=plot_ellipse([1,1],[1 0.5],[],1,'r',2);  
    Reliable: 2  
      
### plot.plot_find_ind

- plot_find_ind function                                           plotting Description: Plot data points and let the user select nearest points to mouse position. Return the indices of selected points.


    
    -  
    plot_find_ind function                                           plotting  
    Description: Plot data points and let the user select nearest points  
    to mouse position. Return the indices of selected points.  
    Input  : (see errorxy.m for input options)  
    Output : - Indices of selected points.  
    Tested : Matlab 7.8  
    By : Eran O. Ofek                          September 2009  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    -  
### plot.plot_findind

- plot_findind function                                            plotting Description: Select data points on a plot using the mouse Return the data points and indices.


    
    -  
    plot_findind function                                            plotting  
    Description: Select data points on a plot using the mouse  
    Return the data points and indices.  
    Input  : - Optional [X,Y] to plot.  
    If not given or empty (i.e., []), then get the data points  
    from the current axis.  
    * Arbitrary number of arguments to pass to the plot command.  
    Output : - Indices of selected data points.  
    - [X, Y] of the input data points  
    - The mouse selected positions.  
    Tested : Matlab 7.8  
    By : Eran O. Ofek               November 2009  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: [AllInd,Data,Select]=plot_findind(rand(100,2));  
    -  
### plot.plot_hr

plot_hr function                                          plotting Description: Given a set of B and R calibrated magnitudes, plot a color magnitude diagram with overlayed main sequence line assuming a given distance.


    
      
    plot_hr function                                          plotting  
    Description: Given a set of B and R calibrated magnitudes, plot  
    a color magnitude diagram with overlayed main  
    sequence line assuming a given distance.  
    Input  : - B mag. vector  
    - R mag. vector  
    - B error mag. vector.  
    - R error mag. vector  
    - distance vector. (in pc).  
    - symbol, default is 'o'.  
    Output : B-R vs. R graph with main sequence on distances  
    defined by Dist.  
    Tested : Matlab 5.1  
    By : Eran O. Ofek           August 1998  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
### plot.plot_int

plot_int function                                                   plotting Description: Plot a 2-dimensional graph and interactively remove and restore data points from the plot.


    
      
    plot_int function                                                   plotting  
    Description: Plot a 2-dimensional graph and interactively remove and  
    restore data points from the plot.  
    Input  : - Handle for current figure in which the data points are ploted.  
    If empty matrix, then use gca. Default is [].  
    The program will read the X and Y of the data points from  
    the figure handle.  
    Alternatively this can be a cell array of the data to plot.  
    The cell array should contain the following information:  
    {X,Y,PlotPars,GCApar,XLabel,YLabel}  
    where X and Y are vectors of data to plot.  
    PlotPars: is a cell array of additional parameters to pass to  
    the plot function (e.g., {'^','MarkerSize',12}, or a string  
    containing the marker type. Default is 'ko'.  
    GCApar: is a cell array of keyword, value,... to pass to  
    the set(gca,...) command. Default is {}.  
    XLabel and YLabel are strings containing the X and Y labels.  
    Default is ''.  
    Alteranatively this can be a character for internal recursive  
    use.  
    - Indices of data points to mark as deleted when the data  
    is first presented on the screen. Default is empty matrix.  
    Alternativey if this is the string 'all' then all the points  
    are marked as deleted.  
    This is useful if you like to mark some or all the points  
    as deleted and let the user return the points to the sample.  
    - Optional function to call when the 'f' option is  
    selected (see menu). The Function has the form:  
    [...] = Fun(X,Y,FunPar{:}); where X and Y are the current  
    X and Y which are not deleted and not marked by red cross.  
    Default is empty matrix (i.e., no function).  
    - Cell array of parameters to pass to the function (FunPar).  
    * Pairs of ...,key,val,... input arguments.  
    The following keywords are avialble:  
    'FunParInd' - The index of the parameter in FunPar that  
    can be modified by typing 'g' on the plot.  
    If not given, or empty, then this parameter  
    will be ignored. Default is []. For example,  
    if CallFun is 'fitgenpoly' then setting this  
    parameter to 2 will enable the user to modify  
    the degree of the polynomial fit.  
    'FunBehav'  - One of the following behaviours:  
    'c' - run the function only when the user  
    click 'f'.  
    'i' - run the function immidetly after each  
    user click with the exception of  
    'q' (default).  
    'DispFit'   - Display best info when calling the function  
    {'y'|'n'}. This option works only when the  
    call function is fitgenpoly.m. Default is 'n'.  
    Output : - Result structure containing the following fields.  
    .OrigX   - Original X in the list, including artificial data.  
    .OrigY   - Original Y in the list, including artificial data.  
    .X       - Final X left in the list.  
    .Y       - Final Y left in the list.  
    .Ind     - Indices of the final X/Y left in the list.  
    .IndRM   - Indices of the X/Y taken out of the list.  
    .FlagAll - A flag indicating if the point was  
    deleted (0) or not (1).  
    .CallFun - The call function.  
    .FunPar  - Cell array of additional parameters passed to  
    the call function.  
    .FunOut  - Cell array of arbitrary number of output parameters  
    from the last function call.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: plot_int1.m  
    Example: X = [1:1:20].';  Y=1+2.*X+0.5.*X.^2+randn(size(X)).*0.5;  
    Res=plot_int({X,Y,'bo',{},'X','Y'},[],'polyfit',{2});  
    Res=plot_int({X,Y,'bo',{},'X','Y'},[],'fitgenpoly',{1,2,'Plot','fitonly'},...  
    'FunParInd',2,'FunBehav','i');  
    Res=plot_int({X,Y,'bo',{},'X','Y'},[],'fitgenpoly',...  
    {1,2,'Plot','fitonly','Xplot',[0:1:25]'},'FunParInd',2,'FunBehav','i');  
    or  
    plot(X,Y,'bo');  
    Res=plot_int([],[],'fitgenpoly',{1,2,'Plot','fitonly','Xplot',[0:1:25]'},...  
    'FunParInd',2,'FunBehav','i');  
    or  
    plot(X,Y,'bo');  
    Res=plot_int([],'all','fitgenpoly',{1,2,'Plot','fitonly','Xplot',[0:1:25]'},...  
    'FunParInd',2,'FunBehav','i');  
    in order to wait until the function will return use:  
    waitfor(gcf,'KeyPressFcn','');  
    Reliable: 2  
      
### plot.plot_int1

plot_int1 function                                                  plotting Description: Given a plot wait for the use to click a keyboard key or a mouse click on the plot, and return the key and click position.


    
      
    plot_int1 function                                                  plotting  
    Description: Given a plot wait for the use to click a keyboard key or  
    a mouse click on the plot, and return the key and click  
    position.  
    Input  : - Handle for current figure in which the data points are ploted.  
    If empty matrix, then use gca. Default is [].  
    - Type of key to wait for:  
    'key'   - wait for a keyboard click on the plot (default).  
    'mouse' - wait for a single mouse click on the plot.  
    'rect'  - wait for a mouse selection of a rectanguar region.  
    'mousem'- wait for a multiple left mouse clicks, and use  
    right click to abort.  
    - Waitfor action {'y'|'n'}. Will return only after the user  
    clicked a key/mouse. Default is 'y'.  
    Output : - Result structure containing the following fields.  
    .Key    - Keyborad key entered.  
    .Pos    - Mouse position [X,Y] or rectangule position  
    [xmin ymin width height].  
    .MB     - Mouse botton (1-left; 2-middle; 3-right).  
    Note that if Type is 'mousem', the last .MB value  
    will be always "3" corresponding to the exit click.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jan 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res = plot_int1(gcf,'mouse');  
    Res = plot_int([],'key');  
    Reliable: 2  
      
### plot.plot_invchildren

plot_invchildren function                                       plotting Description: Invert the order of the childrens under a given handle.


    
      
    plot_invchildren function                                       plotting  
    Description: Invert the order of the childrens under a given handle.  
    Input  : - Handle. Default is gca.  
    Output : null  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Sep 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: plot_invchildren;  
    Reliable: 2  
      
      
### plot.plot_lineprof

- plot_lineprof function                                             plotting Description: Clicking on two points in current image the script return the intensity as function of position along the line between the two points.


    
    -  
    plot_lineprof function                                             plotting  
    Description: Clicking on two points in current image the script return  
    the intensity as function of position along the line  
    between the two points.  
    Input  : - Width of the line (odd integer), default is 1.  
    - Approximate step size along the line [pixels], default is 1.  
    The actual step size is adjusted so nultiply it by an integer  
    will give the length of the line.  
    - Value to calculate:  
    'sum' | 'mean' | 'median' | 'std' | 'min' | 'max',  
    default is 'mean'.  
    - Interpolation method (see interp1), default is 'linear'  
    Output : - Vector of profile.  
    - Vector of position along the line [pixels]  
    - Corresponding X position [pixels]  
    - Corresponding Y position [pixels]  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                       Feb 2004  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
    -  
### plot.plot_polygonselect

plot_polygonselect function                                     plotting Description: Plot and let the use


    
      
    plot_polygonselect function                                     plotting  
    Description: Plot and let the use  
    Input  : -  
    Output : -  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    May 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
### plot.plot_rm

plot_rm function                                              plotting Description: Plot a 2-dimensional graph and interactively remove and restore data points from the plot. This function is being replaced by plot_int.m


    
      
    plot_rm function                                              plotting  
    Description: Plot a 2-dimensional graph and interactively remove and  
    restore data points from the plot.  
    This function is being replaced by plot_int.m  
    Input  : - Vector of X data to plot.  
    - Vector of Y data to plot.  
    - Plot marker, default is 'bo'. If empty, use default.  
    - Optional function to call where the 'f' option is  
    selected (see menu). The Function has the form:  
    [...] = Fun(X,Y); where X and Y are the current  
    X and Y which are not deleted and not marked by red cross.  
    Default is empty matrix (i.e., no function).  
    - The index of varargout of Fun in which a graphic handle  
    is stored. This graphic handle will be deleted before  
    the function is called again. Default is empty matrix  
    (i.e., do noting).  
    * Arbitrary number of input arguments to pass to function  
    to call.  
    Output : - [X Y] list of points that were not deleted or marked with  
    red crosses.  
    - [X Y] list of points that were marked as red crosses.  
    - [X Y] list of points that were deleted.  
    * Arbitrary number of output arguments, which are the  
    output of the function to call (CallFun).  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                          May 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See also: plot_int.m  
    Example: X = [1:1:20].';  Y=1+2.*X+0.5.*X.^2+randn(size(X)).*5;  
    [XY2,XY1,XY0,PolyPar]=plot_rm(X,Y,'bo','polyfit',[],2);  
    Reliable: 2  
      
### plot.plot_scale

- plot_scale function                                      Plotting Description: Add a scale bar on a plot or image.


    
    -  
    plot_scale function                                      Plotting  
    Description: Add a scale bar on a plot or image.  
    Input  : - Scale bar position [X, Y].  
    - Scale [Units per pixel].  
    - Scale bar length in units (e.g., arcsec).  
    - Color, default is 'k';  
    - Units name, default is 'arcsec'.  
    - Scale bar orientation:  
    'h' - Horizontal (default).  
    'v' - Vertical.  
    Output : - Handle for the scale line.  
    - Handle for the text.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                      July 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
    -  
### plot.plot_select_points

plot_select_points function                                     plotting Description: Given an open plot, and X/Y vectors, let the use select using the left-mouse-click arbitrary number of points. For each points return the clicked position, and the


    
      
    plot_select_points function                                     plotting  
    Description: Given an open plot, and X/Y vectors, let the use select  
    using the left-mouse-click arbitrary number of points.  
    For each points return the clicked position, and the  
    nearest in the [X,Y] vectors.  
    Right-mouse click to terminate.  
    Use middle click to remove a point.  
    Input  : - X vector.  
    - Y vector.  
    - Nearest point algorithm:  
    'X' - nearest in x position (default).  
    'D' - nearest in distance.  
    Output : - A structure array of the selected points.  
    The following fields are available:  
    .ClickX  - The user X click position.  
    .ClickY  - The user Y click position.  
    .X       - X position of nearest point.  
    .Y       - Y position of nearest point.  
    .I       - Index of nearest point.  
    Tested : Matlab R2011b  
    By : Eran Ofek / Ofer Yaron          Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=(1:1:100)'; Y=sin(2.*pi.*X./30); plot(X,Y)  
    Points=plot_select_points(X,Y);  
    Reliable: 2  
      
### plot.plot_slit

- plot_slit function                                           plotting Description: Plot a slit (box) on a plot or image.


    
    -  
    plot_slit function                                           plotting  
    Description: Plot a slit (box) on a plot or image.  
    Input  : - Slit center position [X, Y].  
    - Slit length [pixels].  
    - Slit Width [pixels].  
    - Slit position angle [deg], default is 0.  
    - Edge Color, default is 'k';  
    - Face Color, default is 'w';  
    - Slit is open {'y' | 'n'}, default is 'n'.  
    Output : - Handle for slit patch.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
    -  
### plot.plot_speclines

plot_speclines function                                         plotting Description: Overplot series of spectral lines on top of a spectrum.


    
      
    plot_speclines function                                         plotting  
    Description: Overplot series of spectral lines on top of a spectrum.  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    - Additional parameters  
    Any additional key,val, that are recognized by one of the  
    following programs:  
    Output : -  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jun 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
### plot.plotline

plotline function                                                   plotting Description: Plot a line given the position of its start point, length, and angle as measured in respect to the x-axis.


    
      
    plotline function                                                   plotting  
    Description: Plot a line given the position of its start point, length,  
    and angle as measured in respect to the x-axis.  
    Input  : - X (start)  
    - Y (start)  
    - Line length.  
    - Angle [deg], default is 90 (i.e. vertical line).  
    - Color and line type string, see plot for more details.  
    Default is 'b-',  
    * Additional arguments to pass to plot.  
    Output : - Object handle.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                       May 2006  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 1  
      
### plot.quiver1

quiver1 function                                                plotting Description: An improved quiver function. Allows to control the arrows properties and shape.


    
      
    quiver1 function                                                plotting  
    Description: An improved quiver function. Allows to control the arrows  
    properties and shape.  
    Input  : - Two or three column matrix containing [X, Y] or [X, Y, Z]  
    coordinates of the arrow base point.  
    - Two or three column matrix containing [X, Y] or [X, Y, Z]  
    coordinates of the arrow length component in each axis.  
    * Arrow properties. Pairs of ...,keyword,value,... containing  
    one or serveral of the following keywords:  
    'Length'       - Length of arrow head, see arrow for details.  
    'BaseAngle'    - See arrow for details.  
    'TipAngle'     - See arrow for details.  
    'Width'        - Arrow width, see arrow for details.  
    'CrossDir'     - See arrow for details.  
    'NormalDir'    - See arrow for details.  
    'LineStyle'    - Arrow base line style {'-' | '' | '-.' | ':'}  
    'FaceColor'    - Arrow head face color.  
    'EdgeColor'    - Arrow edge color.  
    Output : - Vector of handles for each arrow.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Mar 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### plot.splabel

splabel function                                          plotting Description: Draw a spectral type labeles on an axis of a plot. Assuming the current axis values are B-V color index.


    
      
    splabel function                                          plotting  
    Description: Draw a spectral type labeles on an axis of a plot.  
    Assuming the current axis values are B-V color index.  
    Input  : - Label type:  
    's' : spectral type labels. (default).  
    't' : effective temperature labels.  
    - Labels dimension.  
    1 : X axis. (default).  
    2 : Y axis.  
    3 : Z axis.  
    - Font size, (default is 12).  
    Output : - axis handle.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek         November 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
### plot.subplot1

An improved subplot function. Package: Description: An improved subplot function. Allows to control the gaps between sub plots and to define common axes.


    
    An improved subplot function.  
    Package:  
    Description: An improved subplot function. Allows to control the  
    gaps between sub plots and to define common axes.  
    Input  : - If more than one input argumenst are given,  
    then the first parameter is the number of rows.  
    If single input argument is given, then this is the  
    subplot-number for which to set focus.  
    This could be a scalar (running number) or  
    two element vector [Row, Column].  
    - Number of columns.  
    * variable number of parameters  
    (in pairs: ...,Keywoard, Value,...)  
    - 'Min'    : X, Y lower position of lowest subplot,  
    default is [0.10 0.10].  
    - 'Max'    : X, Y largest position of highest subplot,  
    default is [0.95 0.95].  
    - 'Gap'    : X,Y gaps between subplots,  
    default is [0.01 0.01].  
    - 'XTickL' : x ticks labels option,  
    'Margin' : plot only XTickLabels in the  
    subplot of the lowest  row (default).  
    'All'    : plot XTickLabels in all subplots.  
    'None'   : don't plot XTickLabels in subplots.  
    - 'YTickL' : y ticks labels option,  
    'Margin' : plot only YTickLabels in the  
    subplot of the lowest  row (defailt).  
    'All'    : plot YTickLabels in all subplots.  
    'None'   : don't plot YTickLabels in subplots.  
    -  'FontS'  : axis font size, default is 10.  
    -  'XScale' : scale of x axis:  
    'linear', default.  
    'log'  
    -  'YScale' : scale of y axis:  
    'linear', default.  
    'log'  
    Output : - matrix of handles of subplots axes: Hax(Row,Col).  
    See also : subplot1c.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek           June 2002  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: subplot1(2,2,'Gap',[0.02 0.02]);  
    subplot1(2,3,'Gap',[0.02 0.02],'XTickL','None','YTickL','All','FontS',16);  
    Reliable: 1  
      
### plot.subplot1c

subplot1c function                                                  plotting Description: Given a matrix with N-columns show subplots, of all the combinations of one column plotted against another column. Also return the correlations matrix between all


    
      
    subplot1c function                                                  plotting  
    Description: Given a matrix with N-columns show subplots, of all the  
    combinations of one column plotted against another  
    column. Also return the correlations matrix between all  
    the columns.  
    Input  : - If multi-column matrix is given, then for each pairs of  
    columns (i,j), plot a graph in position i,j in the  
    subplot1 area.  
    If single parameter is given, then this is the  
    subplot-number for which to move focus.  
    - Cell array of columns header. If empty matrix then no  
    labels are plotted.  
    * variable number of parameters  
    (in pairs: ...,Keywoard, Value,...)  
    - 'Min'    : X, Y lower position of lowest subplot,  
    default is [0.10 0.10].  
    - 'Min'    : X, Y largest position of highest subplot,  
    default is [0.95 0.95].  
    - 'Gap'    : X,Y gaps between subplots,  
    default is [0.01 0.01].  
    - 'XTickL' : x ticks labels option,  
    'Margin' : plot only XTickLabels in the  
    subplot of the lowest axes (defailt).  
    'All'    : plot XTickLabels in all subplots.  
    'None'   : don't plot XTickLabels in subplots.  
    - 'YTickL' : y ticks labels option,  
    'Margin' : plot only YTickLabels in the  
    subplot of the single-left axes (defailt).  
    'All'    : plot YTickLabels in all subplots.  
    'None'   : don't plot YTickLabels in subplots.  
    - 'FontS'  : axis font size, default is 10.  
    - 'Symbol' : plot symbol, default is '.'  
    - 'MarkerFaceC' : marker face colr, default is [0 0 0].  
    Output : - A structure with information about the correlation between  
    the various columns.  
    This structure return the following fields:  
    .Corr - A matrix with the Pearson correlation between  
    the various columns.  
    .Prob - A matrix with the probability to get value larger  
    than the correlation, betweem the various columns,  
    as estimated using bootstrap simulations  
    (see corrsim.m for details).  
    See also : subplot1.m, corrsim.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                     Jun 2002  
    URL : http://weizmanna.ac.il/home/eofek/matlab/  
    Example: Corr=subplot1c(rand(100,4))  
    Corr=subplot1c(rand(100,3),{'a','b','c'})  
    subplot1c(1);   will change focus to plot number 1  
    Reliable: 2  
      
      
### plot.textrel

textrel function                                                plotting Description: Write a text in current axis, where the position of the text is specified as a relative position in respect to the axis limits.


    
      
    textrel function                                                plotting  
    Description: Write a text in current axis, where the position of the  
    text is specified as a relative position in respect to  
    the axis limits.  
    Input  : - Relative X position.  
    - Relative Y position.  
    - Text  
    * Arbitrary number of input arguments to pass to the text.m  
    command.  
    Output : - Text handle.  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                     May 2013  
    URL : http://weizamann.ac.il/home/eofek/matlab/  
    Example: H=textrel(0.1,0.9,'Hello');  
    Reliable: 2  
      
      
### plot.value2color

value2color function                                            plotting Description:


    
      
    value2color function                                            plotting  
    Description:  
    Input  : - A coloumn vector of values.  
    - A color map. Default is 'jet'.  
    Output : - A color corresponding to the value, by a linear  
    transformation between the ...  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Feb 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      

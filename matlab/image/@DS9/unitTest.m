function Result = unitTest()
	% unitTest for DS9
    
	io.msgStyle(LogLevel.Test, '@start', 'ds9 test started');
     
	if ~isunix && ~ismac
		io.msgStyle(LogLevel.Test, 'red', 'ds9 - Windows is not supported yet !!!');
        Result = false;
        return;
    end
    
    % create a DS9 object
    D = DS9;
    D.exit;
    D = DS9(rand(100,100),1);
    % open a window
    D.open
    % open another window
    D.open(true);
    D.exit
    % open again
    D.open(true);
    D.isOpen
    D.isWindowExist
    D.isOpen
    [Result,AllMethods] = DS9.getAllWindows;
    D.mode([]) 
    
    % switch ds9 window
    D.Frame=1;
    D.Frame=2;
    pause(0.5)
    if D.nframe~=2
        error('Number of frames suppose to be 2');
    end
    R = D.frame('delete'); % delete current frame
    if D.Frame~=1
        error('Frame number was supposed to be 1');
    end
    
    R = D.frame('center') % center current frame
    R = D.frame('clear') % clear current frame
    R = D.frame('new') % create new frame
    R = D.frame('new rgb') % create new rgb frame
    R = D.frame('reset') % reset current frame
    R = D.frame('refresh') % refresh current frame
    R = D.frame('hide') % hide current frame
    R = D.frame('first') % goto first frame
    R = D.frame('prev') % goto previous frame
    R = D.frame('last') % goto last frame
    R = D.frame('next') % goto next frame
    R = D.frame('match wcs') % 
    R = D.frame('lock wcs') % 
    
    D.clearFrame(2);
    D.clearFrame('all');
   
    D.frame('new');
    D.frame('new');
    D.frame('new');
    D.deleteFrame(2)
    D.deleteFrame('all');
    
    % xpasetFrame
    D = DS9(rand(100,100),1);
    D.load(rand(200,200),2)
    D.xpasetFrame(true,'zoom to 2')
    D.xpasetFrame(true,'zoom to 4')
    
    % zoom
    D.exit
    D.load(rand(100,100));
    FN = D.save2fits;
    delete(FN);
    
    D = DS9(rand(100,100),1);
    D.load(rand(100,100),2);
    D.zoom(5)
    D.zoom(-0.5)
    D.zoom(4,true)
    D.zoom   % zoom to fit
    D.zoom('to 5')
    D.zoom('in')
    D.zoom('out',2)
    
    % scale
    D = DS9(rand(100,100));
    [L,ST] = D.scale;
    D.scale([0 0.5]);
    
    % cmapInvert
    D.cmapInvert
    
    % colorbar control
    D.colorbar
    
    % plot
    D.plot(50,50)
    D.plot(rand(100,2).*100,[],'rs')
    D.plot(rand(100,2).*100,[],'ws','size',3)
    D.regionDelete;
    
    % plotLine
    D.plotLine([1 10],[1 20])
    D.plotLine([1 20],[1 50],'g-','Width',3)
   
    % plotLineSlope
    D.plotLineSlope(10,10,100,100,'r-','Width',4)
    D.plotLineSlope(10,10,[0;10;20],100,'r-','Width',4)
            
    % plotText
    D.plotText(30,30,'Hello')
    D.plotText(30,50,'Hello','FontSize',30, 'Color','blue')
    
    % tile/single
    D.exit
    D = DS9(rand(100,1000,1);
    D.load(rand(100,100),2)
    D.tile(true)
    D.tile(false)
    D.tile([2 1])
    D.tile([2 1],30)
    D.tile  % toggle
    D.tile  % toggle
    
    % blink
    D.exit
    D = DS9(rand(100,100));
    D.load(rand(100,100));
    D.blink
    D.blink
    D.blink(1)
    D.blink(0.2)
    D.blink(false)
    
    % lock
    D = DS9(rand(100,100));
    D.load(rand(100,100),2);
    D.tile
    D.zoom(5,'all')
    D.lock
    D.lock
    
    % match
    D = DS9(rand(100,100));
    D.load(rand(100,100),2);
    D.tile
    D.zoom(5,'all')
    D.match
    D.match
    
    % skyNVSS
    D = DS9;
    D.skyNVSS('m51',[],30)
    D.skyNVSS('12:00:00','+21:10:10',10,'Frame',[])
    D.skyNVSS(1,1)  % deg
    
    % skyDSS
    D = DS9;
    D.skyDSS('m31',[],30)
    D.skyDSS('12:00:00','+21:10:10',10,'Frame',[])
    D.skyDSS(1,1)  % deg
    
    % skyFIRST
    D = DS9;
    D.skyFIRST('m51',[],30)
    D.skyFIRST('12:00:00','+21:10:10',10,'Frame',[])
    D.skyFIRST(1,1)  % deg
    
	io.msgStyle(LogLevel.Test, '@passed', 'ds9 test passed');
	Result = true;
end

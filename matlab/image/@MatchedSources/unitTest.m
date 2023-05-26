function Result = unitTest()
	% unitTest for MatchedSources class
	
	% write
	io.msgLog(LogLevel.Test, 'testing MatchedSources write');
	MS = MatchedSources;
	MS.addMatrix({rand(100,200),rand(100,200)},{'FLUX','MAG'});
	MS.write1('try.hdf5')
	delete('try.hdf5');
	
	% read
	io.msgLog(LogLevel.Test, 'testing MatchedSources read');
	clear MS
	MS = MatchedSources;
	MS.addMatrix({rand(100,200),rand(100,200)},{'FLUX','MAG'});
	MS.write1('try.hdf5');
	clear MS;
	% read all Fields
	MS = MatchedSources.read('try.hdf5');
	% read some fields
	MS1 = MatchedSources.read('try.hdf5','Fields','FLUX');
	delete('try.hdf5');

	% addMatrix
	io.msgLog(LogLevel.Test, 'testing MatchedSources addMatrix');
	MS = MatchedSources;
	MS.addMatrix(rand(100,200),'FLUX');
	MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
	
	St.X2=rand(100,200);
	MS.addMatrix(St);
	
	% deleteMatrix
	io.msgLog(LogLevel.Test, 'testing MatchedSources deleteMatrix');
	MS.deleteMatrix('X2')
				
	% getFieldNameDic
	Obj = MatchedSources;
	Obj.addMatrix(rand(30,40),'RA');
	Obj.addMatrix(rand(30,40),'Dec');
	[FieldName] = getFieldNameDic(Obj, MatchedSources.DefNamesDec);

	% getLonLat
	Obj = MatchedSources;
	Obj.addMatrix(rand(30,40),'RA');
	Obj.addMatrix(rand(30,40),'Dec');
	[MatRA, MatDec] = getLonLat(Obj)           
	
	% match sources for addMatrix:
	AC = AstroCatalog;
	AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
	AC.ColNames = {'RA','Dec'};
	AC.ColUnits = {'rad','rad'};
	AC2 = AstroCatalog;
	AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
	AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
	[MC,UM,TUM] = imProc.match.matchOld(AC,AC2,'Radius',0.01,'RadiusUnits','rad');
	[MC,UM,TUM] = imProc.match.matchOld([AC;AC2; AC; AC2],AC2,'Radius',0.01,'RadiusUnits','rad');
	% run addMatrix with AstroCatalog input
	MS = MatchedSources;
	MS.addMatrix(MC,{'RA','Dec'});
	
	% summary
	io.msgLog(LogLevel.Test, 'testing MatchedSources summary');
	MS=MatchedSources;                
	MS.addMatrix(rand(100,200),'FLUX');
	MS.summary
	MS.summary('FLUX')
	
	% design matrix
	io.msgLog(LogLevel.Test, 'testing MatchedSources designMatrix');
	clear MS
	MS = MatchedSources;
	MS.addMatrix(rand(100,200),'FLUX');
	MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
	St.X2=rand(100,200);
	MS.addMatrix(St);
	[H, Y] = MS.designMatrix({'FLUX','X','Y'},{@sin, 1, 2},'MAG',1);
	[H, Y] = MS.designMatrix({[],'X','Y'},{[], 1, 2},'MAG',1);
	[H, Y, ErrY] = MS.designMatrix({[],'X','Y'},{[], 1, 2},'MAG',1, 'MAG',2);

	% notNanSources
	io.msgLog(LogLevel.Test, 'testing MatchedSources notNanSources');
	MS = MatchedSources;
	MS.addMatrix(rand(100,200),'FLUX');
	MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
	St.X2=rand(100,200);
	MS.addMatrix(St);
	MS.Data.FLUX(1,1)=NaN;
	Flag = notNanSources(MS, 'FLUX');
	if Flag(1,1) || ~all(Flag(2:end))
		error('Problem with notNanSources');
	end
	Flag = notNanSources(MS, []); % use all fields
	io.msgLog(LogLevel.Test, 'testing MatchedSources notNanEpochs');
	MS = MatchedSources;
	MS.addMatrix(rand(100,200),'FLUX')
	MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
	St.X2=rand(100,200);
	MS.addMatrix(St);
	MS.Data.FLUX(1,1)=NaN;
	Flag = notNanEpochs(MS, 'FLUX');
	Flag = notNanEpochs(MS, []); % use all fields

	% getMatrix
	io.msgLog(LogLevel.Test, 'testing MatchedSources getMatrix');
	MS = MatchedSources; 
	MS.Data.FLUX = rand(100,200);
	A = MS.getMatrix('FLUX');
	
	% plotRMS
	io.msgLog(LogLevel.Test, 'testing MatchedSources plotRMS');
	MS = MatchedSources;
	MS.addMatrix(rand(100,200),'MAG_PSF');
	MS.plotRMS
	MS.plotRMS('BinSize',0.1)
	
	MS = MatchedSources;
	MS.addMatrix(rand(100,200),'FLUX');
	MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'});
	[JD, Mag] = getLC_ind(MS, [2 3], {'FLUX'});
	
	
	% designMatrixCalib
	H=MatchedSources.designMatrixCalib(2,4);
	H=MatchedSources.designMatrixCalib(2,4,'UseSparse',true);
	H=MatchedSources.designMatrixCalib(2,4,'SrcProp',{3.*ones(1,4)});
	H=MatchedSources.designMatrixCalib(2,4,'SrcProp',{3.*ones(1,4)},'SrcPropCoefType',2);

    
    MS = MatchedSources;
    MS.addMatrix(rand(100,200),'FLUX')
    MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
    MS.applyZP(ones(100,1))
    %if ~(all(MS.Data.MAG<0,'all') && all(MS.Data.MAG>-1,'all'))
    %    error('Problem with applyZP');
    %end
       
    % rmsMag
	MS = MatchedSources;
    MS.addMatrix(rand(100,200).*10,'MAG')
    [Result] = MS.rmsMag
    
    
	io.msgStyle(LogLevel.Test, '@passed', 'MatchedSources test passed');
	Result = true;
end

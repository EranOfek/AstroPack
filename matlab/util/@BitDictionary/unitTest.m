function Result = unitTest(Obj)
	%
	io.msgStyle(LogLevel.Test, '@start', 'BitDictionary test started');

	Obj = BitDictionary;
	Tbl = cell2table({'Saturation', 'Saturated pixel', 0; ...
					  'DeadPix',    'Pixel level at Flat is low', 1; ...
					  'NoisyPix',   'High noise in dark image', 3;...
					  'Spike',      'Stellar spike',            11});
				  
	Tbl.Properties.VariableNames = {'BitName','BitDescription','BitInd'};
	Obj.Dic = Tbl;
	
	
	[BitName,BitDescription,BitInd] = bitind2name(Obj,[0 1 17]);
	
	% @FAILED - @Eran
	[BitName,BitDesc,BitInd] = bitdec2name(Obj,[3,1,2^11+7; 1 1 1]);
	
	[BitInd,BitDec,SumBitDec,BitDescription]=name2bit(Obj,{'Spike','DeadPix'});
	
	io.msgStyle(LogLevel.Test, '@passed', 'BitDictionary test passed');
	Result = true;
end

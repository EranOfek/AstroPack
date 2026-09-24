function Result = unitTest()
    % unitTest for CubeComponent class
    
    Im{1}=rand(100,100); Im{2}=rand(100,100);
    C=CubeComponent.images2cube(Im);
    Im{1}=rand(100,100); Im{2}=rand(102,100);
    C=CubeComponent.images2cube(Im, 'CCDSEC',[1 90 1 90]);
    Im = AstroImage({rand(100,100), rand(100,100)});
    C=CubeComponent.images2cube(Im);
    C=CubeComponent.images2cube(Im, 'CCDSEC',[11 90 1 100]);

    C = CubeComponent.cutouts(rand(100,100),[1 16 17]',[45 17 98]');
    C = CubeComponent.cutouts(AstroImage({rand(100,100)}),[1 16 17]',[45 17 98]');
            
    C=CubeComponent;
    C.Data = rand(3,3,2);
    if C.numImages~=2
        error('Error in CubeComponent numImages');
    end
    
    C=CubeComponent;
    C.Data = rand(3,3,2);
    if ~all(C.dimXY == [1 2])
        error('Error in CubeComponent dimXY');
    end
    
    Im = AstroImage({rand(10,10), rand(10,10)});        
    C=CubeComponent.images2cube(Im);
    C.XY = [30 30; 60 70];
    Result = C.cutouts2image(zeros(100,100));
    Result = C.cutouts2image([100 100]);

    Im = AstroImage({ones(10,10), 2.*ones(10,10)}); 
    C=CubeComponent.images2cube(Im);             
    R = unaryFunImage(C, @sum);
    if ~all(squeeze(R)==[100;200])
        error('Error in CubeComponent unaryFunImage');
    end
    if ~all(unaryFunImage(C, @sum, 'Squeeze',1)==[100;200])
        error('Error in CubeComponent unaryFunImage');
    end
    unaryFunImage(C, @std, 'FunPreDimArgs',{[]});

    
    Im = AstroImage({ones(10,10), 2.*ones(10,10)}); 
    C=CubeComponent.images2cube(Im);             
    R=unaryFunCollapse(C, @sum);
    if ~all(R==3)
        error('Error in CubeComponent unaryFunCollapse');
    end
    R=unaryFunCollapse(C, @sum, 'OutType','AstroImage');
    R=unaryFunCollapse(C, @sum, 'OutType','AstroImage', 'DataProp','Back');
    R=unaryFunCollapse(C, @sum, 'OutType','ImageComponent');
    R=unaryFunCollapse(C, @std, 'FunPreDimArgs',{[]});
    R=unaryFunCollapse(C, @sum, 'OutType','CubeComponent');

    
    
    
    Result = true;
end
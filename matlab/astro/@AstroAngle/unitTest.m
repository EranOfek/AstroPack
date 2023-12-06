function Result = unitTest
    % unitTest for the AstroAngle class
    io.msgStyle(LogLevel.Test, '@start', 'AstroAngle test started')
    
    
    % Constructor
    A = AstroAngle(rand(10,10),'deg');
    A = AstroAngle('10:15:20.1','h');
    A = AstroAngle({'+10:15:20.1','-01:00:01.1'},'d');    
    
    % convert
    A = AstroAngle(rand(10,10),'arcsec');
    A.convert('deg');
    if ~strcmp(A.Units,'deg')
        error('Convert of arcsec to deg failed');
    end
    
    % convert2array
    A = AstroAngle(rand(10,10),'arcmin');
    D = A.convert2array('arcsec');
    
    % tpi
    A = AstroAngle(rand(10,10).*400,'deg');
    A.tpi;
    if min(min(A.Angle)) < 0 || max(max(A.Angle)) > 360
        error('tpi failed');
    end
        
    % mpi
    A = AstroAngle(rand(10,10).*400,'deg');
    A.mpi;
    if min(min(A.Angle)) < -180 || max(max(A.Angle)) > 180
        error('mpi failed');
    end
     
    % dsex
    A = AstroAngle([10,-0.1],'deg');
    R = A.dsex;
    
    % hsex
    A = AstroAngle([10,0.1],'deg');
    R = A.hsex;
    
    % dms
    A = AstroAngle([10,-0.1],'deg');
    R = A.dms;
    
    % hms
    A = AstroAngle([10,0.1],'deg');
    R = A.hms;
    
    % fun
    A = AstroAngle(rand(10,10),'deg');
    D = A.fun(@sind);
    
    % sin
    A = AstroAngle(rand(10,10),'mas');
    D = A.sin;
    
    % cos
    A = AstroAngle(rand(10,10),'rad');
    D = A.cos;
    
    % tan
    A = AstroAngle(rand(10,10),'deg');
    D = A.tan;    
    
    
    io.msgStyle(LogLevel.Test, '@passed', 'AstroAngle test passed')
    Result = true;
end
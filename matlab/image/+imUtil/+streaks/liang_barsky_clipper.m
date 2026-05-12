function [clippedX]=liang_barsky_clipper(Xrect,Xi)
% taken from https://en.wikipedia.org/wiki/Liang%E2%80%93Barsky_algorithm
%  and translated into matlab

% Xrect, Xi, clippedX: 4 elements array, [x1 y1, x2 y2]

  xmin=Xrect(1);
  ymin=Xrect(2);
  xmax=Xrect(3);
  ymax=Xrect(4);
  
  x1=Xi(1);
  y1=Xi(2);
  x2=Xi(3);
  y2=Xi(4);
  
  clippedX=nan(1,4); % default no clipped segment, unless found

  % defining variables
  p1 = -(x2 - x1);
  p2 = -p1;
  p3 = -(y2 - y1);
  p4 = -p3;

  q1 = x1 - xmin;
  q2 = xmax - x1;
  q3 = y1 - ymin;
  q4 = ymax - y1;

  exitParams=[0 0 0];
  entryParams=[0 0 0];
  exitIndex = 1;
  entryIndex = 1;
  exitParams(1) = 1;
  entryParams(1) = 0;


  if ((p1 == 0 && q1 < 0) || (p2 == 0 && q2 < 0) ||...
          (p3 == 0 && q3 < 0) || (p4 == 0 && q4 < 0))
      %fprintf("Line is parallel to clipping window!\n");
      return;
  end
  
  if p1 ~= 0
    r1 = q1 / p1;
    r2 = q2 / p2;
    entryIndex=entryIndex+1;
    exitIndex=exitIndex+1;    
    if p1 < 0
      entryParams(entryIndex) = r1;
      exitParams(exitIndex) = r2;
    else
      entryParams(entryIndex) = r2;
      exitParams(exitIndex) = r1;
    end
  end
  if p3 ~= 0
    r3 = q3 / p3;
    r4 = q4 / p4;
    entryIndex=entryIndex+1;
    exitIndex=exitIndex+1;    
    if p3 < 0
      entryParams(entryIndex) = r3;
      exitParams(exitIndex) = r4;
    else
      entryParams(entryIndex) = r4;
      exitParams(exitIndex) = r3;
    end
  end

  u1 = max(entryParams(1:entryIndex)); % maximum of entry points
  u2 = min(exitParams(1:exitIndex));   % minimum of exit points

  if u1 > u2
    %fprintf("Line is outside the clipping window!\n");
    return;
  end
  
  clippedX(1) = x1 + (x2 - x1) * u1;
  clippedX(2) = y1 + (y2 - y1) * u1;
  clippedX(3) = x1 + (x2 - x1) * u2;
  clippedX(4) = y1 + (y2 - y1) * u2;

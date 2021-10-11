%% <Topic Title>
%% Description (H1)
% <*Topic*> is the field of doing ... with ... for ... Topic name in *bold*.
% 
% SHORT PARAGRAPH with primary capabilities/functionality. 
% 
% For additional help see |manuals.main|
%% Overview (H1)
% LIST OF PROPERTIES with ONE LINE per property. Property name in *bold*, alphabetically 
% sorted.
%% 
% * *ColX*  - X/RA/Lon column index or column name.
% * *ColY* - Y/Dec/Lat column index or column name.
% * *Sorted* - Indicating if the list is sorted.
% * 
%% 
% 
% MethodA (H3)
% *MethodA* is used to ...

% populate the CooType and CooUnits properties:
[CooType, NameX, NameY, IndInCellX, IndInCellY] = getCooTypeAuto(AC(1));
% convert the units in the coordinate columns from 'rad' to 'deg'
AC=AstroCatalog({'asu.fit','asu.fit'},'HDU',2)
AC.convertCooUnits('deg')
Obj.convertCooUnits('deg')
% Methods for retrieving coordinate columns - TOPIC (H3)
%% 
% * *getCoo* - Get coordinates columns from a single element AstroCatalog object.
% * *getLonLat* - Get Lon/Lat columns from AstroCatalog.

% Get RA/Dec columns
AC=AstroCatalog({'asu.fit'},'HDU',2);
% get RA, Dec, or X, Y (according to CooType) in deg in seperate variables
[RA, Dec] = AC.getCoo('deg');
% get RA, Dec in rad in a single two column ar  ray
[RADec]   = AC.getCoo('rad');

AC=AstroCatalog({'asu.fit'},'HDU',2);
% get RA, Dec
[Lon,Lat] = getLonLat(AC);
[Lon,Lat] = getLonLat(AC,'rad');
%% See Also (H1)
%% 
% * AVOID using duplicate words, such as Example, ... use the structure and 
% formatting of the document to visually provide information and clues about the 
% contents of the text.
%% Notes (H1)
% ADD HERE your notes, learnings, and any usefull information that you think 
% is relevent to the subject.
% 
% Use H2, H3, Bold, and Code blocks if required.
% 
%
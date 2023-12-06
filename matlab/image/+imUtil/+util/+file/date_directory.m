function [Y, M, D] = date_directory(JD,TimeZone)
	% Return the D/M/Y for a directory given a date
	% Package: +imUtil.util.file
	% Description: When associating a date with a file/directory name, the
	%              nearest day at the previous noon is chosen.
	% Input  : - A JD, [D M Y F], [D M Y H M S], 'YYYYMMDDTHHMMSS.FFF'.
	%            If empty or not provided than will use current UTC date.
	%          - Time zone [hours]. Default is 2.
	% Output : - Year
	%          - Month
	%          - Day
	% Example: [Y,M,D]=imUtil.util.file.date_directory


	if nargin<2
		TimeZone = 2;
		if nargin<1
			JD = celestial.time.julday;
		end
	end
	if isempty(JD)
		JD = celestial.time.julday;
	end
	if numel(JD)>1
		JD = celestial.time.julday(JD);
	end

	LocalJD = JD + TimeZone./24;
	FloorLocalJD = floor(LocalJD);
	FloorDate = celestial.time.jd2date(FloorLocalJD);
		   
	Y = FloorDate(3);
	M = FloorDate(2);
	D = FloorDate(1);

end

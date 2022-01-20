function startup_LAST


Home   = char(java.lang.System.getProperty('user.home'));
Base   = sprintf('%s%s%s',Home, filesep, 'LAST');


List = {'LAST_OCS',...
	'LAST_configuration'};

Nlist = numel(List);
for Ilist=1:1:Nlist
	    addpath(sprintf('%s%s%s',Base, filesep, List{Ilist}));
end




function startup_LAST
    % startup for LAST directories

    Home   = char(java.lang.System.getProperty('user.home'));
    Base   = sprintf('%s%s%s%s%s',Home, filesep,'natlab',filesep, 'LAST');

    List = {'LAST_OCS',...
            'LAST_configuration'};

    Nlist = numel(List);
    for Ilist=1:1:Nlist
        sprintf('%s%s%s',Base, filesep, List{Ilist})
        %addpath(sprintf('%s%s%s',Base, filesep, List{Ilist}));
    end

end
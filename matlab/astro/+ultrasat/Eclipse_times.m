function [EclipseTimes,inEclipse,tEndEclipse]=Eclipse_times(Args)
% return a table fo eclipse times at 4W, and can optionally query by time.
% Package: ultrasat
% Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'times' - vector of datetimes to query if in eclipse 
% Output : - Table of eclipse time with the following columns  {'StartTimeUTCG'   }    {'StopTimeUTCG'    }
%                                       {'Durationsec'     }    {'Obstruction'     }    {'CurrentCondition'} 
%                                       {'WorstCondition'  }    {'TotalDurationsec'}
%           - Optionally, a boolean vector if in eclipse
%           - Optionally, a vector with times until end of Eclipse
% License: GNU general public license version 3
%     By : Yossi Shvartzvald                    updated May 2026
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: t = datetime('14-Jan-2029 05:00:00'); t2 = datetime('26-Feb-2029 00:30:00');
%          [EclipseTimes,inEclipse,tEndEclipse]=ultrasat.Eclipse_times('Times',[t t t2]');
%          
% Reliable: 
%--------------------------------------------------------------------------
 
    arguments
        Args.BaseDataDir = '~/matlab/data/ULTRASAT/'; % Base directory for data needed for uplanner
        Args.EclipseTableFile  = 'EclipseTimes.mat'; 
        Args.UniqueEclipseTableFile  = 'UniqueEclipse.mat';
        Args.Times datetime = NaT(0,0);
    end

    
    EclipseTimes = load(fullfile(Args.BaseDataDir, Args.EclipseTableFile)); 
    EclipseTimes = EclipseTimes.EclipseTimes; 

    UniqueEclipse = load(fullfile(Args.BaseDataDir, Args.UniqueEclipseTableFile)); 
    UniqueEclipse = UniqueEclipse.UniqueEclipse; 

    inEclipse = false(size(Args.Times));
    tEndEclipse = duration.empty(0,numel(Args.Times));

    for i = 1:numel(Args.Times)
        % Check if the provided times are within the eclipse periods
        indEclipse = find(Args.Times(i) >= UniqueEclipse.StartTimeUTCG & Args.Times(i) <= UniqueEclipse.StopTimeUTCG);
        if indEclipse
            inEclipse(i) =true;
            tEndEclipse(i) = UniqueEclipse.StopTimeUTCG(indEclipse)-Args.Times(i);
        end
    end
end

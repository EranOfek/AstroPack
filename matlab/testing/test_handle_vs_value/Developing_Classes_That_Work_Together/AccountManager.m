classdef AccountManager

   methods (Static)
   
      function assignStatus(BA)
         if BA.AccountBalance < 0
            if BA.AccountBalance < -200
               BA.AccountStatus = 'closed';
            else
               BA.AccountStatus = 'overdrawn';
            end
         end
      end

	  
      function lh = addAccount(BA)
         lh = addlistener(BA, 'InsufficientFunds', ...
            @(src, ~)AccountManager.assignStatus(src));
      end
   end
   
end

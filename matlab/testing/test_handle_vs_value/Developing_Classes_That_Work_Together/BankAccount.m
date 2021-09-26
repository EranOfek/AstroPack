classdef BankAccount < handle

   properties (Access = ?AccountManager)
      AccountStatus = 'open'
   end
   
   properties (SetAccess = private)
      AccountNumber
      AccountBalance
   end
   
   properties (Transient)
      AccountListener
   end
   
   events
      InsufficientFunds
   end
   
   
   methods
   
      function BA = BankAccount(accNum,initBal)
         BA.AccountNumber = accNum;
         BA.AccountBalance = initBal;
         BA.AccountListener =  AccountManager.addAccount(BA);
      end
	  
	  
      function deposit(BA,amt)
         BA.AccountBalance = BA.AccountBalance + amt;
         if BA.AccountBalance > 0
            BA.AccountStatus = 'open';
         end
      end
	  
	  
      function withdraw(BA,amt)
         if (strcmp(BA.AccountStatus,'closed')&& BA.AccountBalance <= 0)
            disp(['Account ',num2str(BA.AccountNumber),' has been closed.'])
            return
         end
         newbal = BA.AccountBalance - amt;
         BA.AccountBalance = newbal;
         if newbal < 0
            notify(BA,'InsufficientFunds')
         end
      end
	  
	  
      function getStatement(BA)
         disp('-------------------------')
         disp(['Account: ',num2str(BA.AccountNumber)])
         ab = sprintf('%0.2f',BA.AccountBalance);
         disp(['CurrentBalance: ',ab])
         disp(['Account Status: ',BA.AccountStatus])
         disp('-------------------------')
      end
   end
   
   
   methods (Static)
      function obj = loadobj(s)
         if isstruct(s)
            accNum = s.AccountNumber;
            initBal = s.AccountBalance;
            obj = BankAccount(accNum,initBal);
         else
            obj.AccountListener = AccountManager.addAccount(s);
         end
      end
   end
end


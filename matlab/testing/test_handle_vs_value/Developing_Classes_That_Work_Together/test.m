% https://www.mathworks.com/help/matlab/matlab_oop/developing-classes-typical-workflow.html

BA = BankAccount(1234567,500)

getStatement(BA)

withdraw(BA,600)
getStatement(BA)

withdraw(BA,200)
getStatement(BA)


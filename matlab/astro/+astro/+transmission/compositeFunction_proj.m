function compositeFunction_proj

Eran's project:

each component function is added as:
methods:
Funs(i).Name
       .Handle(X, ParVect, Options (these are Args))  % Y = fun(X, Par, Options); if Args = 0 function returns all about itself - OR getParams
       .Par
       .FitPar = []
       .ParCalc logical [0,0,0,1,0]
       .ParNames
       .ParMapping 
       .Options/Args

addComponent
preCalc (all .ParCalc = 0 and preCalc empty - fill in, otherwise - use)
getParNames
wrapping(crossed out)
parMapping [AirMass, Alpha, Beta] - dynamically added, one row to one function Par[i,j]

operator = @* [+, etc]
evalute(X, Par) - calculation of the purpose




Funs_i 'airmass'  'beta'
1      AM    gamma delta [1 NaN NaN]
2      AM    alpha beta  [1 2 3]
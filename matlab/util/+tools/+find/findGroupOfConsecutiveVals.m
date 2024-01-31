function Index = findGroupOfConsecutiveVals(A, M, N, Val)
    % in vector A find Mth group of N consecutive values of Val
    % Input: - a vector of values
    %        - the number of identical value group to be indexed
    %        - the length of identical value groups 
    %        - the value (we are looking for consecutive groups of this value)
    % Output: - a vector of indices of the Mth group of N consecutive values of Val
    % Author: A.M. Krassilchtchikov (Jan 2024)
    % Example: A = [0 0 1 1 1 0 0 1 1 0 1 1 1 0 0 0 1 0 1 1 1];
    %          Ind = tools.find.findGroupOfConsecutiveVals(A, 2, 3, 1); 
    %          [will give the indices of the M = 2nd group of N = 3 values Val = 1 in vector A]
    %          B = [1 2 7 8 7 8 9 0 7 7 1 6 7 7 6 5 7 7 1 0 7 8];
    %          Ind = tools.find.findGroupOfConsecutiveVals(B, 3, 2, 7); 
    %          [will give the indices of the M = 3rd group of N = 2 values Val = 7 in vector B]
    %          Ind = tools.find.findGroupOfConsecutiveVals(B, 4, 2, 7);
    %          will be empty, as there is no 4th group of two 7th
    Ind0 = find(A == Val);
    for i = 1:length(Ind0)-N+1
        ConsecutiveGroup = Ind0(i:i+N-1);
        if all(diff(ConsecutiveGroup) == 1)
            M = M - 1;
            Index = ConsecutiveGroup;
            if M == 0
                return;
            end
        end
    end
    Index = []; % nothing is found 
end
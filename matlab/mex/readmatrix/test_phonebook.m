% https://www.mathworks.com/help/matlab/matlab_external/passing-structures-and-cell-arrays.html

function test_phonebook()

    friends(1).name  = 'Jordan Robert';
    friends(1).phone = 3386;
    %friends(1).ex    = 1;
    friends(2).name  = 'Mary Smith';
    friends(2).phone = 3912;
    %friends(1).ex    = 2;
    friends(3).name  = 'Stacy Flora';
    friends(3).phone = 3238;
    %friends(3).ex    = 3;
    friends(4).name  = 'Harry Alpert';
    friends(4).phone = 3077;
    %friends(4).ex    = 4;

    phonebook(friends)

end

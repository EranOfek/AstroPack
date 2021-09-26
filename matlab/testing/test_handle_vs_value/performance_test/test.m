
cprintf('blue', 'STARTED\n\n');

Iters = 100;
Outers = 1;

for Loop=1:Outers
    
    cprintf('blue', 'Iters: %d\n\n', Iters);

    % Value Class
    ValueCl = ValueClass(1);
    cprintf('blue', 'Value started\n');
    tic
    for I=1:Iters
        ValueC2 = ValueCl.sin();
    end
    cprintf('red', 'Value: %0.4f\n\n', toc);


    % Handle Class
    HandleCl = HandleClass(1);
    cprintf('blue', 'Handle started\n');
    tic
    for I=1:Iters
        HandleCl.sin();
    end
    cprintf('red', 'Handle: %0.4f\n\n', toc);


    % 
    cprintf('blue', 'Handle copy started\n');
    tic
    for I=1:Iters
        HC2 = HandleCl.copy();
        HC2 = HC2.sin();
    end
    cprintf('red', 'Handle with Copy: %0.4f\n\n', toc);


    % 
    SER = false;
    if SER
        cprintf('blue', 'Handle serCopy started\n');
        tic
        for I=1:Iters
            HC2 = HandleCl.serCopy();
            HC2 = HC2.sin();
        end
        cprintf('red', 'Handle with serCopy: %0.4f\n\n', toc);
    end
    
    %
    cprintf('blue', '\nDone Iters: %d\n\n', Iters);

    Iters = Iters * 10;
end


cprintf('blue', '\nDONE\n\n');
    


fprintf('Hello\n');
wrtest();


function tst()

    cprintf('blue', 'STARTED\n\n');

    Iters = 100;
    Outers = 1;

    for Loop=1:Outers

        cprintf('blue', 'Iters: %d\n\n', Iters);

        % Value Class
        cprintf('blue', 'Value started\n');
        ValueCl = ValueClass(1);
        tic
        for I=1:Iters
            ValueC1 = ValueCl.sin();
        end
        cprintf('red', 'Value: %0.4f\n\n', toc);


        % Handle Class
        cprintf('blue', 'Handle started\n');
        HandleCl = HandleClass(1);    
        tic
        for I=1:Iters
            HandleCl.sin();
        end
        cprintf('red', 'Handle: %0.4f\n\n', toc);


        % Handle Class with copy
        cprintf('blue', 'Handle copy started\n');
        HandleCl = HandleClass(1);
        tic
        for I=1:Iters
            % This copies ALL properties of the class
            HandleC2 = HandleCl.copy();       
            HandleC2.sin();
        end
        cprintf('red', 'Handle with Copy: %0.4f\n\n', toc);


        % 
        cprintf('blue', 'Handle copy modify started\n');
        HandleCl = HandleClass(1);
        tic
        for I=1:Iters
            HandleC2 = HandleCl.copy();

            % Replace values in copied object
            HandleC2.Mat(1) = 10;
            %HandleC2.Mat1(1) = 11;
            %HandleC2.Mat2(1) = 12;
            %HandleC2.Mat3(1) = 13;

            % Make sure that objects are not the same
            %assert(HandleC2.Mat(1,1)  ~= HandleCl.Mat(1,1));
            %assert(HandleC2.Mat1(1,1) ~= HandleCl.Mat1(1,1));
            %assert(HandleC2.Mat2(1,1) ~= HandleCl.Mat2(1,1));
            %assert(HandleC2.Mat3(1,1) ~= HandleCl.Mat3(1,1));

            HandleC2.sin();
        end
        cprintf('red', 'Handle with Copy modify: %0.4f\n\n', toc);

        % 
        SER = false;
        if SER
            cprintf('blue', 'Handle serCopy started\n');
            HandleCl = HandleClass(1);
            tic
            for I=1:Iters
                HC2 = HandleCl.serCopy();
                HC2.sin();
            end
            cprintf('red', 'Handle with serCopy: %0.4f\n\n', toc);
        end

        %
        cprintf('blue', '\nDone Iters: %d\n\n', Iters);

        Iters = Iters * 10;
    end


    cprintf('blue', '\nDONE\n\n');
    
end

%==========================================================================

function wrtest()

    cprintf('blue', 'wrtest STARTED\n\n');

    Iters = 100;
    Outers = 1;

    for Loop=1:Outers

        cprintf('blue', 'Iters: %d\n\n', Iters);


        % 
        cprintf('blue', 'Modify Mat1(1,1) WITHOUT copy started\n');
        HandleCl = HandleClass(1);
        tic
        for I=1:Iters
            HandleC1.Mat1(1,1) = 11;
        end
        cprintf('red', 'Modify Mat1(1,1) WITHOUT copy: %0.4f\n\n', toc);
                
        % 
        cprintf('blue', 'Modify Mat1(1,1) with Copy started\n');
        HandleCl = HandleClass(1);
        tic
        for I=1:Iters
            HandleC2 = HandleCl.copy();
            HandleC2.Mat1(1,1) = 11;
        end
        cprintf('red', 'Modify Mat1(1,1) with Copy: %0.4f\n\n', toc);
        
        
        cprintf('blue', 'Modify Small(1,1) started\n');
        HandleCl = HandleClass(1);
        tic
        for I=1:Iters
            HandleC2 = HandleCl.copy();
            HandleC2.Small(1,1) = 11;
        end
        cprintf('red', 'Modify Small(1,1): %0.4f\n\n', toc);        

        
        %
        cprintf('blue', '\nDone Iters: %d\n\n', Iters);

        Iters = Iters * 10;
    end

    cprintf('blue', '\nwrtest DONE\n\n');
    
end

%==========================================================================

% 
% Im = AstroImage;
% Flat = AstroImage;
% 
% De = deflat(Im, Flat);
% deflat(Im, Flat);
% 
% Im.funBinaryProp(Flat, @rdivide);
% 
% De = Im.funBinaryProp(Flat, @rdivide);
% Im = Im.funBinaryProp(Flat, @rdivide);
% 
% 
%                                             



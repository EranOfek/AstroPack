
fprintf('Hello\n');
%perfTest();
copyElementTest();
fprintf('\nThe end.\n');

function perfTest()
    % Conclusions: Handle class is x3 faster than value class!!!
    % because there is no need to internally copy objects
    %
    % Value: 6.4440, Handle: 2.3910, Handle with Copy: 6.2118, Handle with Copy modify: 8.8589
    %

    cprintf('blue', 'STARTED\n\n');

    Iters = 100;
    Outers = 1;

    for Loop=1:Outers

        cprintf('blue', 'Iters: %d\n\n', Iters);

        % Value Class: The sin() function returns the modified object
        cprintf('blue', 'Value started\n');
        ValueCl = ValueClass(1);
        tic
        for I=1:Iters
            ValueC1 = ValueCl.sin();
        end
        cprintf('red', 'Value: %0.4f\n\n', toc);


        % Handle Class: The sin() function modifies the object
        cprintf('blue', 'Handle started\n');
        HandleC = HandleClass(1);    
        tic
        for I=1:Iters
            HandleC.sin();
        end
        cprintf('red', 'Handle: %0.4f\n\n', toc);


        % Handle Class with copy: The sin() function modifies a copy of the object
        cprintf('blue', 'Handle copy started\n');
        HandleC = HandleClass(1);
        tic
        for I=1:Iters
            % This copies ALL properties of the class
            HandleC2 = HandleC.copy();       
            HandleC2.sin();
        end
        cprintf('red', 'Handle with Copy: %0.4f\n\n', toc);


        % Copy handle object, modify its Mat, and then call sin() to modify Mat
        cprintf('blue', 'Handle copy modify started\n');
        HandleC = HandleClass(1);
        tic
        for I=1:Iters
            HandleC2 = HandleC.copy();

            % Replace values in copied object
            HandleC2.Mat(1) = 10;
            %HandleC2.Mat1(1) = 11;
            %HandleC2.Mat2(1) = 12;
            %HandleC2.Mat3(1) = 13;

            % Make sure that objects are not the same
            %assert(HandleC2.Mat(1,1)  ~= HandleC.Mat(1,1));
            %assert(HandleC2.Mat1(1,1) ~= HandleC.Mat1(1,1));
            %assert(HandleC2.Mat2(1,1) ~= HandleC.Mat2(1,1));
            %assert(HandleC2.Mat3(1,1) ~= HandleC.Mat3(1,1));

            HandleC2.sin();
        end
        cprintf('red', 'Handle with Copy modify: %0.4f\n\n', toc);

        % 
        SER = false;
        if SER
            cprintf('blue', 'Handle serCopy started\n');
            HandleC = HandleClass(1);
            tic
            for I=1:Iters
                HC2 = HandleC.serCopy();
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

function perfTest2()

    cprintf('blue', 'wrtest1 STARTED\n\n');

    Iters = 100;
    Outers = 1;

    for Loop=1:Outers

        cprintf('blue', 'Iters: %d\n\n', Iters);

        % 
        cprintf('blue', 'Modify Mat1(1,1) WITHOUT copy started\n');
        HandleC = HandleClass(1);
        tic
        for I=1:Iters
            HandleC.Mat1(1,1) = 11;
        end
        cprintf('red', 'Modify Mat1(1,1) WITHOUT copy: %0.4f\n\n', toc);
                
        % 
        cprintf('blue', 'Modify Mat1(1,1) with Copy started\n');
        HandleC = HandleClass(1);
        tic
        for I=1:Iters
            HandleC2 = HandleC.copy();
            HandleC2.Mat1(1,1) = 11;
        end
        cprintf('red', 'Modify Mat1(1,1) with Copy: %0.4f\n\n', toc);
        
        
        cprintf('blue', 'Modify Small(1,1) started\n');
        HandleC = HandleClass(1);
        tic
        for I=1:Iters
            HandleC2 = HandleC.copy();
            HandleC2.Small(1,1) = 11;
        end
        cprintf('red', 'Modify Small(1,1): %0.4f\n\n', toc);        

        
        %
        cprintf('blue', '\nDone Iters: %d\n\n', Iters);

        Iters = Iters * 10;
    end

    cprintf('blue', '\nwrtest1 DONE\n\n');
    
end

%==========================================================================

function copyElementTest()

    cprintf('blue', 'copyElementTest STARTED\n\n');

    % 
    cprintf('blue', 'Modify Mat1(1,1) WITHOUT copy started\n');
    HandleC = HandleClassCopy(1);
    HandleC.Mat1(1,1) = 11;
    cprintf('red', 'Modify Mat1(1,1) WITHOUT copy: %0.4f\n\n', toc);

    % 
    cprintf('blue', 'Modify Mat1(1,1) with Copy started\n');
    HandleC = HandleClassCopy(1);
    tic
    HandleC2 = HandleC.copy();
    HandleC.Mat1(1,1) = 10;
    HandleC2.Mat1(1,1) = 20;
    
    HandleC.InObj.InMat1(1,1) = 101;
    HandleC2.InObj.InMat1(1,1) = 202;
    
    assert(~strcmp(HandleC.InObj.Uuid, HandleC2.InObj.Uuid));
    assert(~strcmp(HandleC.Uuid, HandleC2.Uuid));
    
    assert(HandleC.Mat1(1,1) ~= HandleC2.Mat1(1,1));
    assert(HandleC.InObj.InMat1(1,1) ~= HandleC2.InObj.InMat1(1,1));
    
    cprintf('red', 'Modify Mat1(1,1) with Copy: %0.4f\n\n', toc);

    % Create array of objects
    HandleArray(1) = HandleClassCopy(1);
    HandleArray(2) = HandleClassCopy(1);
    HandleArray(3) = HandleClassCopy(1);
    
    HandleArrayCopy = HandleArray.copy();
    
    
    cprintf('blue', 'copyElement: Modify Small(1,1) started\n');
    HandleC = HandleClassCopy(1);
    tic
    HandleC2 = HandleC.copy();
    HandleC2.Small(1,1) = 11;
    cprintf('red', 'copyElement: Modify Small(1,1): %0.4f\n\n', toc);        

        
    cprintf('blue', '\copyElementTest DONE\n\n');   
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



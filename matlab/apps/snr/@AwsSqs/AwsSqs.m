
classdef AwsSqs < Component
    % 

    % Properties
    properties (SetAccess = public)
        ProcessMsgHandler = []        
    end

    %--------------------------------------------------------
    methods % Constructor

        function Obj = AwsSqs(varargin)
            % Constructor
            
            Obj.setName('AwsSqs');
            Obj.msgLog(LogLevel.Debug, 'AwsSqs created: %s', Obj.Name);
                        
        end
        
        
        function delete(Obj)
            % Destructor
            Obj.msgLog(LogLevel.Debug, 'AwsSqs deleted: %s', Obj.Name);
            Obj.release();
        end        
    end


    methods

        function Result = init(Obj)
            %
            
            Result = true;            
        end
        
        
        function Result = register(Obj)
            %
            
            Result = true;            
        end
        
        
        function Result = processQueue(Obj)
            % To be overloaded by derrived class or set as event
            
            Result = true;            
        end
        
        
        function Result = processMsg(Obj, Msg)
            %
            
            Result = true;
        end
        
        
        function Result = sendMsg(Obj, Msg)
            %
            
            Result = true;            
        end

        
        
        function Result = demo(Obj)
            
            %
            % https://github.com/mathworks-ref-arch/matlab-aws-sqs
            %

            % Create the client called sqs
            sqs = aws.sqs.Client();
            % use a JSON credentials file
            sqs.useCredentialsProviderChain = false;
            sqs.initialize();

            % create a Queue, note AWS provides naming guidelines
            QueueName = 'com-example-myQueue';
            createQueueResult = sqs.createQueue(QueueName);
            queueUrl = createQueueResult.getQueueUrl();

            % get a list of the Queues and see that com-example-myQueue appears
            listQueueResult = sqs.listQueues();
            urlList = listQueueResult.getQueueUrls();

            % send a message to the queue
            sendMessageResult = sqs.sendMessage(queueUrl, 'My SQS Message');
            sentMessageId = sendMessageResult.getMessageId();

            % Receive messages from the queue
            receiveMessageResult = sqs.receiveMessage(queueUrl);
            messages = receiveMessageResult.getMessages();

            % Get Id, receiptHandle and body of each message
            for n = 1:numel(messages)
              messageId = messages{n}.getMessageId();
              receiptHandle = messages{n}.getReceiptHandle();
              body = messages{n}.getBody();
            end

            % cleanup by deleting the Queue and shutting down the client
            deleteQueueResult = sqs.deleteQueue(queueUrl);
            sqs.shutdown;

        end
        
   

    end


    methods(Static)
        
    end


    methods(Static) % Unit test
        Result = unitTest()
            % unitTest
    end
end


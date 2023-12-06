#  Amazon Simple Queue Service (SQS)

### What is SQS

https://aws.amazon.com/sqs/

Amazon Simple Queue Service (SQS) is a fully managed message queuing service 
that enables you to decouple and scale microservices, distributed systems, 
and serverless applications. SQS eliminates the complexity and overhead 
associated with managing and operating message-oriented middleware, and empowers 
developers to focus on differentiating work. Using SQS, you can send, store, 
and receive messages between software components at any volume, without losing 
messages or requiring other services to be available. Get started with SQS in 
minutes using the AWS console, Command Line Interface or SDK of your choice, 
and three simple commands.

SQS offers two types of message queues. Standard queues offer maximum throughput, 
best-effort ordering, and at-least-once delivery. SQS FIFO queues are designed to 
guarantee that messages are processed exactly once, in the exact order that 
they are sent.


### MATLAB SQS Library

https://github.com/mathworks-ref-arch/matlab-aws-sqs


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


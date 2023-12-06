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


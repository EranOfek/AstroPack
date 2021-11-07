


https://stackoverflow.com/questions/28687295/sqs-vs-rabbitmq

https://stackoverflow.com/questions/13681213/what-is-the-difference-between-amazon-sns-and-amazon-sqs/13692720#13692720





SQS would be my preference over RabbitMQ, here is why.

    SQS is a managed service. So you don't have to worry about operational aspects of running a messaging system including administration, security, monitoring etc. Amazon will do this for you and will provide support if something were to go wrong.
    SQS is Elastic and can scale to very large rate/volumes (unlimited according to AWS ;))
    Availability of SQS has a lot of 9's in it and is backed by Amazon, which is one less thing to worry about in your application.

However RabbitMQ might provide faster response times for puts and gets, typically in 10s of thousands of TPS from my testing. For SQS to provide that kind of throughput, you will have to scale up horizontally with multiple instances. So if you are looking for under 5ms puts , RabbitMQ might be an option to consider because i have seen close to 20ms-30ms put time from my SQS testing at 1000s of TPS, which is slightly higher than RabbitMQ.

We just moved our messaging infrastructure from ActiveMQ to SQS and can't be any more happier. We have found it to be cheaper than maintaining our own ActiveMQ cluster in the cloud.

Hope this helps! Let us know how it goes..





41

I actually used both in a commercial environment with reasonable.

The short answer is unless there are specific corner cases, it's better to go with AWS SQS. (You can skip to the bottom for simple summary)

Coding (Tie): RabbitMQ and AWS SQS both have establish libraries and plenty of examples.

Visibility timeout (SQS): One thing that SQS offers over RabbitMQ is a broader notion of visibility timeout. In RabbitMQ, if a consumer dies before it acks, the messages is put back into the queue. But SQS has a broader notion of visibility timeout that is not tied to a specific caller. So you can start a unit of work, set the visibility with large timeout (up to 12 hours), disconnect, have another worker finish and ack it. In my design, we leverage this extensively and eliminated additional service/storage to manage potentially large 'in progress' payloads.

Dead letter handling (RabbitMQ - by a 'hare') SQS provides basic dead letter handing in what they call "Re-drive policy" that dumps messages into Dead Letter Queue (just another queue). It's basic and only has a notion of message count. RabbitMQ has Dead Letter Exchanges that provides messages getting pushed int DLE when they expire. But this is sort of moot as the idea of "If you aren't watching your services and messages expire, then it will land in the DLE". It's a slight win for RabbitMQ as I find that argument counter intuitive. Why would you monitor your queue and not your services? (If anything, it's the other way around)

Administration (SQS): There is no administration to SQS. You just pay for API calls. All usual headaches like OS/app security patches, scale (add more nodes), disk are handled by AWS teams. It is also FedRamp compliant (for government use). It is truly a 'setup and forget' system. Where as RabbitMQ requires usual OS/service patches, AMIs, clustering, security hardening, etc. While it is extremely rare, AMIs can go down, or sometimes require to be moved around so clustering is needed out of box. SQS eliminates all those headaches.

COST (SQS): A single SQS API call can include 'batch up to 10 messages/256k size' and 'long polling' can drastically cut the cost down. Furthermore, there are strategies like message compression to shove dozens (some claim hundreds or more) of messages can be sent in a single payload to reduce cost further. And this is before we consider time people spend monitoring/patching/fixing issues. SQS is also great for 'poc projects' as if it sit idle, there's no cost.

FIFO (TIE): In 2016, AWS introduced FIFO support at a additional cost of ~$0.01/million api calls ($0.05 vs $0.04 per million API alls - before discounts). You can choose to use FIFO or not. For non-FIFO we rarely see duplicate messages.

Storage (SQS): AWS does not charge for storage but you do have a limit of 14 days. On RabbitMQ, you will have to allocate, expand, and manage disk space that require peak storage capacity plus extra buffers. It's just more headaches.

Metrics (SQS): SQS provides out of box metrics. And while you could add them to AWS, it's just more work.

Local dev (tie): Most modern shops like to have local environment. There are several options that allow dockers of RabbitMQ and SQS now.

High throughput/very large message (RabbitMQ - sort of) As you push SQS > 1000 requests/sec, SQS's latency will go up. There are several strategies to get around it. But I find these cases to be extremely rare as most work can be partitioned to multiple queues. But for these types of cases where 100k/sec is required, I think Kafka is better. (We also use Kafka at my work) It is rare to have a single unit of work that requires 1000+ request/second with low latency. *See more below for this explanation

Summary: If you are going to be in AWS and willing to be married to SQS, then SQS is a no brainer. But you should read on as there are important things to consider.

The classic strategy for RabbitMQ (and other queues) are to create several types of queues optimized for certain types of work. Then fine tune each of these queues and group similar work into a small number of these (often very large in size) queues. Since SQS has no administrative overhead, it is actually better to allocate dedicated queue for each work. By doing so, it allows for scale but also eliminates queue saturation (offending work saturating the queue and drowning out other workers), better view into the work (default metrics), and such.

The new strategy has allowed my teams to have better view of how work is distributed. Gone are the days of 'upgrading instance for more load'. In the past, we would see a large unexplained spike that would cause side effects to other services or just guessed that the cumulative numbers looks abut right'. Now that traffic is separated, we actually uncovered many issues that went unnoticed before and can clearly explain how much traffic is going where. And while it is very possible to implement metrics and tooling, SQS provides all of these out of the box.

There are still great cases RabbitMQ should be seriously considered





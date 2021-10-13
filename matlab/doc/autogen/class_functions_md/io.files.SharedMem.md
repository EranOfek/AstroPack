# Class: io.files.SharedMem



    
    https://www.mathworks.com/help/matlab/import_export/share-memory-between-applications.html  
    Learn how to implement Mutex for ipc  
    Background threads (Java or other solution?)  
      
      
      
      
    // Shared memory header, at offset 0  
    struct MemHeader {  
    uint32_t    signature;      // Unique signature  
    uint32_t    memsize;        // Size of entire shared memory block  
    uint32_t    flags;          // Flags  
    uint32_t    item_size;      // Image buffer size  
    uint32_t    item_alloc;     // Number of buffers currently in queue  
    uint32_t    head;           // Head index  
    uint32_t    tail;           // Tail index  
    uint32_t    count;          // Number of items in buffer  
    };  
      
    struct BufHeader {  
    uint32_t    put_counter;    // Running counter by put_buf()  
    uint32_t    buf_len;        // Data length  
    uint32_t    width;          // Image width  
    uint32_t    height;         // Image height  
    uint32_t    flags;          // Flags, BUFH_...  
    };  
      
      

### Functions List

    SharedMem - 
    answer - Respond to SEND using memmapfile class.
    close - Close file
    delete - 
    doGetBuf - Get next buffer from shared-memory, return bytes object or None if queue is empty Return buf, put_counter, width, height, flags
    getBuf - Return buf, put_counter, w, h with m = Locker(...)
    open - 
    read - Read buffer from shared-memory
    send - Interactively send a message to ANSWER using memmapfile class.
    unitTest - 
    unitTestClientSide - Simple test - read string from shared memory
    unitTestClientSideBuf - Read next buffer from shared-memory queue
    unitTestServerSide - Simple test (without header) - write current time string to shared memory
    write - Write buffer to shared-memory

### SharedMem




    


### answer

Respond to SEND using memmapfile class.


    
    Respond to SEND using memmapfile class.  
      


### close

Close file


    
    Close file  


### delete




    


### doGetBuf

Get next buffer from shared-memory, return bytes object or None if queue is empty Return buf, put_counter, width, height, flags


    
    Get next buffer from shared-memory, return bytes object or None if queue is empty  
    Return buf, put_counter, width, height, flags  
      
    Read header to array of unsigned integers  


### getBuf

Return buf, put_counter, w, h with m = Locker(...)


    
    Return buf, put_counter, w, h  
    with m = Locker(...)  


### open




    


### read

Read buffer from shared-memory


    
    Read buffer from shared-memory  


### send

Interactively send a message to ANSWER using memmapfile class.


    
    Interactively send a message to ANSWER using memmapfile class.  
      


### unitTest




    


### unitTestClientSide

Simple test - read string from shared memory


    
    Simple test - read string from shared memory  


### unitTestClientSideBuf

Read next buffer from shared-memory queue


    
    Read next buffer from shared-memory queue  


### unitTestServerSide

Simple test (without header) - write current time string to shared memory


    
    Simple test (without header) - write current time string to shared memory  
      


### write

Write buffer to shared-memory


    
    Write buffer to shared-memory  



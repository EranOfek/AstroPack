# Package: imUtil.tests


### imUtil.tests.worker_comm

PoolObj=parpool F=parfeval(@imUtil.tests.worker_comm,2,5) delete(PoolObj)


    
      
    PoolObj=parpool  
    F=parfeval(@imUtil.tests.worker_comm,2,5)  
    delete(PoolObj)  
      
      
      

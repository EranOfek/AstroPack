# https://www.programmersought.com/article/2145850619/


import mmap
import contextlib
import time
 
with open("test.dat", "w") as f:
    f.write('\x00' * 1024)
 
with open('test.dat', 'r+') as f:
    with contextlib.closing(mmap.mmap(f.fileno(), 1024, access=mmap.ACCESS_WRITE)) as m:
        for i in range(1, 10001):
            m.seek(0)
            s = "msg " + str(i)
            s.rjust(1024, '\x00')
            m.write(s)
            m.flush()
            time.sleep(1)
            
            
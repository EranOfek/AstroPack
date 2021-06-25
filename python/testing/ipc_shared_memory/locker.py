

class Locker:
    def __enter__ (self):
        self.fp = open("./lockfile.lck")
        fcntl.flock(self.fp.fileno(), fcntl.LOCK_EX)

    def __exit__ (self, _type, value, tb):
        fcntl.flock(self.fp.fileno(), fcntl.LOCK_UN)
        self.fp.close()

And then use it as:

print("waiting for lock")
with Locker():
    print("obtained lock")
    time.sleep(5.0)

	
	
	
	
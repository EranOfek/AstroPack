# Dummy wrapper

from loglevel import LogLevel
from dbcomponent import DbComponent

class DbDriver(DbComponent):

    def __init__(self):
        self.IsOpen = False
        self.needUuid();
        self.msgLog(LogLevel.Debug, 'DbDriver created: %s', self.Uuid)


    # Destructor
    def __del__(self):
        self.msgLog(LogLevel.Debug, 'DbDriver deleted: %s', Obj.Uuid);


    def open(self):
        self.msgLog(LogLevel.Info, 'DbDriver: open');
        return True


    def close(self)
        return True;


    @staticmethod
    def unitTest():
        io.msgStyle(LogLevel.Test, '@start', 'DbDriver test started\n');
        return True


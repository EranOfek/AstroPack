
class MonitorStatusData:

    # Constructor
    def __init__(self):
        self.timestamp = None
        self.status_code = 0
        self.status_str = ''


    # Destructor
    def __del__(self):
        # Deleted
        pass




# Resource monitor base
class MonitorBase(Component):

    # Constructor
    def __init__(self):
        self.monitor_name = ''
        self.database_name = ''
        self.table_name = ''
        self.data = None


    # Destructor
    def __del__(self):
        # Deleted
        pass


    # Monitor
    def monitor(self):
        pass


    # Clear current status
    def clearStatus(self):
        pass


    # Set status
    def setStatus(self, status):
        pass





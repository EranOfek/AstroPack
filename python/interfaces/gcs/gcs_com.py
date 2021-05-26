# IAI Interface

class GcsComm:

    # Constructor
    def __init__(self):
        self.interface_name = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass


    # Send message to GCS
    def send_msg(self, msg):
        pass

    # Send Target-Of-Opertunity
    def send_target_oo(self, task):
        pass

    # Send task for validatation
    def send_task_validation(self, task):
        pass

    # Send task
    def send_task(self, task):
        pass

    # Handle incoming message from GCS
    def handle_msg(self, msg):
        pass

    def handle_telemetry(self):
        pass



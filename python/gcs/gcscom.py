


# ===========================================================================

# ===========================================================================

# GCS Communication Manager

class GcsComm(Component):

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


# ===========================================================================

# ===========================================================================



# GCS Message
class ImageProcessor(Component):

    # Constructor
    def __init__(self):
        self.msg_id = ''
        self.rcv_time = 0
        self.send_time = 0
        self.process_time = 0

    # Destructor
    def __del__(self):
        # Deleted
        pass

    # Load from XML
    def load_from_xml(self):
        pass

    # Save to XML
    def save_to_xml(self):
        pass

    def to_str(self):
        pass

    def from_str(self):
        pass





# ===========================================================================

# ===========================================================================


# GCS Message
class TelemetryProcessor(Component):

    # Constructor
    def __init__(self):
        self.msg_id = ''
        self.rcv_time = 0
        self.send_time = 0
        self.process_time = 0

    # Destructor
    def __del__(self):
        # Deleted
        pass

    # Load from XML
    def load_from_xml(self):
        pass

    # Save to XML
    def save_to_xml(self):
        pass

    def to_str(self):
        pass

    def from_str(self):
        pass




# All GCS Messages


from gcs_msg import GcsMsg

# Common Messages Types
MSG_ACK = ''
MSG_KEEP_ALIVE = ''

# SOC -> GCS Message Types

MSG_IMAGING_TASK = ''
MSG_OBRD_TASK = ''

# GCS -> SOC Message Types

MSG_



#------------------------------------------------------------------
#
class GcsMsgTask(GcsMsg):

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

#------------------------------------------------------------------



#------------------------------------------------------------------


#------------------------------------------------------------------


#------------------------------------------------------------------


#------------------------------------------------------------------



#------------------------------------------------------------------



#------------------------------------------------------------------



#------------------------------------------------------------------



#------------------------------------------------------------------






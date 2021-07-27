# Incoming Alerts Manager

class IncomingAlertsManager(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''
        self.msg_log('')


    # Destructor
    def __del__(self):
        # Deleted
        self.msg_log('')


    # Process all new records in incoming alerts table
    def process_new_incoming_alerts(self):
        self.msg_log('')
        # 'SELECT * FROM incoming_alerts WHERE new_flag = 1 ORDER BY rcv_time'


    # Run filter
    def run_filter(self, alert_data):
        self.msg_log('')

        # Return result




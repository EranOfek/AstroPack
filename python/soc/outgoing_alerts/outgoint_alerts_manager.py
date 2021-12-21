# Outgoing Alerts Manager
#
# Author: Chen Tishler (Sep 2021)
#

class OutgingAlertsManager(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''
        self.msg_log('')

    # Destructor
    def __del__(self):
        # Deleted
        self.msg_log('')

    def manage(self):
        self.msg_log('')

        # Select new (unsent) alerts
        # 'SELECT * FROM OUTGOING_ALERTS WHERE SENT = 0'

        # Iterate all records


    # Process result set
    def process_outgoing_alert(self):
        self.msg_log('')


    # Send single alert
    def send_alert(self, alert):
        self.msg_log('')



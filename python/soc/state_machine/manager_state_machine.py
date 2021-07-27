#

# State-machine states

SM_NONE = 0
SM_IDLE = 1

SM_

#
class ManagerStateMachine(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''

    # Destructor
    def __del__(self):
        # Deleted
        self.msg_log('')

    #
    def init(self):
        self.msg_log('')

    #
    def start(self):
        self.msg_log('')

    #
    def timer_proc(self):
        try:
            self.manage()
        except:
            self.msg_log('')


    #
    def manage(self):

        self.process_new_incoming_alerts()

        self.process_



    # Process new incoming alerts
    def process_new_incoming_alerts(self):
        self.msg_log('')

    #
    def process_new_target(self):
        self.msg_log('')

    #
    def process_monitors(self):
        self.msg_log('')


    def run_pipeline(self):
        self.msg_log('')

    #
    def process_new_outgoing_alerts(self):
        self.msg_log('')


    #

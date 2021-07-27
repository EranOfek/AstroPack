# Incoming Alerts Filter:

from incoming_alerts_filter_params import IncomingAlertFilterParams
from incoming_alerts_filter_result import IncomingAlertFilterResult

import incoming_alerts_filter_params

# Filter single incoming alerts
class IncomingAlertFilter(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''
        self.params = None


    # Destructor
    def __del__(self):
        # Deleted
        pass


    def filter(self, alert):
        result = IncomingAlertFilterResult()

        # Check if
        return result


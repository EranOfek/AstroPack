# GCS Observation Plan Data

# Obrd entry
class ObrdEntry:

    # Constructor
    def __init__(self):
        self.interface_name = ''


# Obrd data
class ObrdData:

    # Constructor
    def __init__(self):
        self.jtime = 0



# Obrd Manager
class ObrdManager:

    # Constructor
    def __init__(self):
        self.task_uuid = ''
        self.point_list = []        # List of ObservationPoint


    # Destructor
    def __del__(self):
        # Deleted
        pass


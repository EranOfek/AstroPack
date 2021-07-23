# GCS Observation Plan Data

# Sky Coordination
class SkyCoord:

    # Constructor
    def __init__(self):
        self.interface_name = ''


# Sky Time
class SkyTime:

    # Constructor
    def __init__(self):
        self.jtime = 0


# single Observation Point
class ObservationPoint:

    # Constructor
    def __init__(self):
        self.point_uuid = ''
        self.coord = SkyCoord
        self.start_time = SkyTime
        self.stop_time = SkyTime

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


# Observation Task
class ObservationTask:

    # Constructor
    def __init__(self):
        self.task_uuid = ''
        self.point_list = []        # List of ObservationPoint


    # Destructor
    def __del__(self):
        # Deleted
        pass


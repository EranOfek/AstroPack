from gcsbase import Component

# Classes in this file:
#   SkyCoord    -
#   SkyTime     -
#   ImageId     -
#   TaskId      -

# ===========================================================================
#
# ===========================================================================
# GCS Observation Plan Data

# Sky Coordination
class SkyCoord:

    # Constructor
    def __init__(self):
        self.ra = 0
        self.dec = 0
        self.roll = 0


# Sky Time
class SkyTime:

    # Constructor
    def __init__(self):
        self.time = 0


# ImageId
class ImageId:
    def __init__(self):
        self.format_str = 'IMG%05d'
        self.num = 1

    def get_id(self):
        s = self.format_str % self.num
        return s


class TaskId:
    def __init__(self):
        self.format_str = 'TSK_%s%05d'
        self.type = 'IMG'
        self.num = 1

    def get_id(self):
        s = self.format_str % (self.type, self.num)
        return s


# ===========================================================================
#
# ===========================================================================

# single Observation Point
class ObservationPoint:

    # Constructor
    def __init__(self):
        self.point_uuid = ''
        self.coord = SkyCoord
        self.start_time = SkyTime
        self.stop_time = SkyTime


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
#
# ===========================================================================

# Observation Task
class ObservationTask:

    # Constructor
    def __init__(self):
        self.task_uuid = ''
        self.point_list = []        # List of ObservationPoint




# ===========================================================================
#
# ===========================================================================

# Observation Task
class ObservationPlan:

    # Constructor
    def __init__(self):
        self.task_uuid = ''
        self.point_list = []        # List of ObservationPoint



# ===========================================================================
#
# ===========================================================================

# Observation Manager
class ObservationManager:

    # Constructor
    def __init__(self):
        self.task_uuid = ''
        self.point_list = []        # List of ObservationPoint


    def gen_image_id(self):
        new_id = ''
        return

# ===========================================================================
#
# ===========================================================================


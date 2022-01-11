import time
from gcsbase import Component
from gcsgui import GuiHandler, GuiMsg

# ===========================================================================

# ===========================================================================

# GCS Communication Manager

class GcsSimulator(Component):

    # Constructor
    def __init__(self, args):
        super().__init__()
        self.interface_name = ''
        self.image_gen = ImageGenerator()
        self.tele_gen = TelemetryGenerator()
        self.gui = GuiHandler()
        self.terminated = False
        self.last_image_time = 0
        self.new_image_interval = 10


    #
    def run(self):
        self.setup()
        while not self.terminated:
            self.manage()
            time.sleep(0.01)

        self.shutdown()


    #
    def setup(self):
        pass

    #
    def shutdown(self):
        pass

    #
    def manage(self):
        self.handle_incoming_msgs()
        self.generate_images()
        self.handle_gui()

    #
    def handle_incoming_msgs(self):
        self.handle_incoming_msg(msg)
        pass


    def handle_incoming_msg(self, msg):
        pass

    #
    def generate_images(self):
        t = time.time()
        elapsed = self.last_image_time
        if elapsed > self.new_image_interval:
            self.last_image_time = t
            self.generate_new_image()
        pass


    def generate_new_image(self):
        pass

    # Send message to GCS
    def send_imaging_task_response(self, msg):
        pass


    # Send message to GCS
    def send_obrd_task_response(self, msg):
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


    #
    def handle_gui(self):
        pass

# ===========================================================================

# ===========================================================================

# Generator of new images
class SimImageGenerator(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.msg_id = ''

    # Create new image, return file name
    def create_image(self):
        pass

# ===========================================================================

# ===========================================================================

# GCS Message
class SimTelemetryGenerator(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.msg_id = ''

    # Create new telemetry data, return file name
    def create_telemetry(self):
        pass

# ===========================================================================

# ===========================================================================

#
class SimPlanValidator(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.msg_id = ''

    # Create new telemetry data, return file name
    def validate_plan(self):
        pass




# ===========================================================================
#                                       OBRD
# ===========================================================================


# Obrd entry
class SimObrdEntry:

    # Constructor
    def __init__(self):
        self.interface_name = ''

# ===========================================================================
#
# ===========================================================================

# Obrd data
class SimObrdData:

    # Constructor
    def __init__(self):
        self.jtime = 0


#
class SimObrdManager(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.msg_id = ''

    #
    def manage(self):
        pass



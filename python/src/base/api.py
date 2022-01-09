
from base import Base

class ApiMsg(Base):

    def __init__(self):
        self.msg_text = ''
        self.fields = {}
        pass

    def __del__(self):
        pass

    def to_text(self):
        pass

    def from_text(self, text):
        pass


# ---------------------------------------------------------------------------

class Api(Base):

    def __init__(self):
        pass



    def __del__(self):
        pass


    def send_msg(self, msg):
        pass

# ---------------------------------------------------------------------------


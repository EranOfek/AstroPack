# Component base class
#       This is the base class from which all the classes in AstroPack
#       hinerits.

import LogLevel, io

# Base class for all objects
class Base:

    def __init__(self):
        self.UserData = None

    # unitTest for Base class
    @staticmethod
    def unitTest():

        io.msgLog(LogLevel.Test, 'Base test started')

        # Test copyObject()
        a = Base()
        a.UserData = 123
        b = a.copyObject()
        assert(a.UserData == b.UserData)
        b.UserData = 0
        assert(a.UserData != b.UserData)

        # Test copyProp()
        c = Base()
        a.copyProp(c, {'UserData'})
        assert(a.UserData == c.UserData)

        io.msgLog(LogLevel.Test, 'Base test passed')
        return True

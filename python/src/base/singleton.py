
from typing import Any

class Singleton(type):
    """
    A Singleton class
    """
    _instances = {}

    def __call__(cls, *args, **kwargs) -> Any:
        if cls not in cls._instances:
            cls._instances[cls] = super(Singleton, cls).__call__(*args, **kwargs)
        return cls._instances[cls]


if __name__ == '__main__':
    # "Unit Tests"
    class TestObject(object, metaclass=Singleton):
        def __init__(self, **kwargs):
            self.a = 5
            super(TestObject, self).__init__(**kwargs)

    object1 = TestObject()
    object1.a = 10
    assert TestObject().a == 10, "Singleton not working!"

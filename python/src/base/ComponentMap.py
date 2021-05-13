

class ComponentMap(Base):

    # Map object is a data structure that allows you to retrieve values
    # using a corresponding key. Keys can be real numbers or character
    # vectors. As a result, they provide more flexibility for data
    # access than array indices, which must be positive integers.
    # Values can be scalar or nonscalar arrays.
    def __init__(self):
        self.Map = []            # containers.Map - List of CompDirMap
        self.Name = []

    def ComponentMap(varargin):
        if numel(varargin) > 0
            Obj.Name = varargin{1};
        else
            Obj.Name = '(unnamed)';

        Obj.msgLog(LogLevel.Debug, 'ComponentMap created: %s', Obj.Name);
        Obj.Map = containers.Map();


    # Destructor
    def delete(self):
        self.msgLog(LogLevel.Debug, 'ComponentMap deleted: %s', Obj.Name);
        self.release()


    def add(self, Comp):
        Key = self.getKey(Comp);
        self.msgLog(LogLevel.Info, 'ComponentMap.add: %s', Key);

        if ~self.Map.isKey(Key)
            self.Map(Key) = Comp;
        else
            self.msgLog(ObjLevel.Warning, 'ComponentMap.add: Component already exists in map: %s', Key);




    def remove(self, Comp):
        Key = self.getKey(Comp);
        self.msgLog(LogLevel.Debug, 'ComponentMap.remove: %s', Key);

        if self.Map.isKey(Key):
            self.Map.remove(Key);
        else:
            self.msgLog(ObjLevel.Warning, 'ComponentMap.remove: Component does not exist in map: %s', Key);


    # Return component by key
    def find(self, CompKey):

        if self.Map.isKey(CompKey):
            return self.Map(CompKey)
        else:
            return None


    # Get/make component key
    def getKey(self, Comp):
        return Comp.needMapKey()


    def getCount(self):
        return self.Map.Count


    # Release all components from map
    def release(self):

        for Key = self.Map.keys:
            Comp = self.Map(Key)

            # @TODO: Need release or delete??
            #Comp.release();
            self.Map.remove(Key)

        # Make sure that everything was removed
        assert(self.Map.Count == 0)



    # Write message to log
    def msgLog(Obj, Level, varargin):

        #Obj.Log.msgLog(Level, varargin{:});
        io.msgLog(Level, varargin{:});


    # Return singleton object
    @staticmethod
    def getSingleton():
        persistent PersObj
        if isempty(PersObj):
            PersObj = ComponentMap;

        Result = PersObj;



    # Unit test
    @staticmethod
    def unitTest():
        io.msgLog(LogLevel.Test, 'ComponentMap test started')

        Map = ComponentMap
        assert(Map.getCount() == 0)

        Comp1 = Component
        Map.add(Comp1)
        assert(Map.getCount() == 1)
        assert(~isempty(Map.find(Comp1.MapKey)))

        Map.remove(Comp1)
        assert(isempty(Map.find(Comp1.MapKey)))

        io.msgLog(LogLevel.Test, 'ComponentMap test passed');
        return True

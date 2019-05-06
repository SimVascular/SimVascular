class AbstractPreProcessor(object):
    def __init__(self, config):
        self.config = config
        self.setup()

    def setup(self):
        pass

    def __call__(self,x):
        pass

    def preprocess_label(self,y):
        return y

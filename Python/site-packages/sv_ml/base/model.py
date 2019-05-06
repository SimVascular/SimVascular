class AbstractModel(object):
    def __init__(self, config):
        self.config=config
        self.setup()
    def setup(self):
        pass
    def predict(self,x):
        pass
    def save(self):
        pass
    def load(self):
        pass
    def train(self):
        pass

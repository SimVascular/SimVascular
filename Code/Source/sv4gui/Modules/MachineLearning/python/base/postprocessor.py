class AbstractPostProcessor(object):
    def __init__(self, config):
        self.config = config
        self.setup()

    def setup(self):
        pass

    def set_inputs(self, T):
        pass

    def __call__(self, x):
        pass

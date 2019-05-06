import components.common as common
#from components.seg import SegPostProcessor

def get(config, key="TRAIN"):
    if "POST_PROCESSOR" in config:
        pp = config['POST_PROCESSOR']

        if pp == "EDGE":
            return common.EdgePostProcessor(config)
        elif pp == 'SEG':
            return SegPostProcessor(config)
        elif pp == 'POINT':
            return common.PointPostProcessor(config)

    return common.BasePostProcessor(config)

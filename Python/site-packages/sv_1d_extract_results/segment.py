#!/usr/bin/env python

from os import path
import logging
from manage import get_logger_name

class Segment(object):
    def __init__(self, id, name, node1, node2, bc_type):
        self.id = id
        self.name = name
        self.node1 = node1
        self.node2 = node2
        self.bc_type = bc_type
        self.data_name = None
        self.data = None


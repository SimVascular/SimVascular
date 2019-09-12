#!/usr/bin/env python

from os import path
import logging
from manage import get_logger_name

class Node(object):
    def __init__(self, id, x, y, z):
        self.id = id
        self.x = x
        self.y = y
        self.z = z


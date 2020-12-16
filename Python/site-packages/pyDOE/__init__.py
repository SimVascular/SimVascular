"""
================================================================================
pyDOE: Design of Experiments for Python
================================================================================

This code was originally published by the following individuals for use with
Scilab:
    Copyright (C) 2012 - 2013 - Michael Baudin
    Copyright (C) 2012 - Maria Christopoulou
    Copyright (C) 2010 - 2011 - INRIA - Michael Baudin
    Copyright (C) 2009 - Yann Collette
    Copyright (C) 2009 - CEA - Jean-Marc Martinez
    
    website: forge.scilab.org/index.php/p/scidoe/sourcetree/master/macros

Much thanks goes to these individuals. It has been converted to Python by 
Abraham Lee.
"""

# from __future__ import absolute_import

__author__ = 'Abraham Lee'
__version__ = '0.3.8'

from pyDOE.doe_box_behnken import *
from pyDOE.doe_composite import *
from pyDOE.doe_factorial import *
from pyDOE.doe_lhs import *
from pyDOE.doe_fold import *
from pyDOE.doe_plackett_burman import *
from pyDOE.var_regression_matrix import *
    

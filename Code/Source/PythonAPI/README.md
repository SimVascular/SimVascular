
# SimVascular Python API 

The SimVascular Python API is implemented using Python and C++. The files in this directory implement most of the SimVascular Python API. The following modules have been implemented 

- geometry
- meshing
- modeling
- pathplanning
- segmentation
- vmtk

The module names reflect the names of the SV application tools. The C++ code for these modules and classes is compiled into the **lib_simvascular_python_api** shared library. 

The code for the **dmg** module is located in **SimVascular/Code/Source/sv4gui/Plugins/org.sv.pythondatanodes**. The  **dmg** module must be impemented within a project so it can interact with the SV Data Manager (MITK) framework. The C++ code for these modules and classes is compiled into the **liborg_sv_pythondatanodes** shared library.

The code for the SimVascular Python API **sv** package is locted in **SimVascular/Python/site-packages/sv**. This directory also contains Python code implementing the following classes

- MeshSimOptions 
- Project
- Visualization

The **__init__.py** script loads modules from the **lib_simvascular_python_api** shared library. 

# Code Organization

The code is organized using seprate files for modules and classes defined for those modules

  - Modules are implemented in *ModuleName*\_PyModule.(cxx,h) files
  
  - Classes are implemented in *ModuleNameClassName*\_PyClass.cxx file. 

For example the **pathplanning** module code is contained in the files
```
PathPlanningGroup_PyClass.cxx
PathPlanningPath_PyClass.cxx
PathPlanningSubdivMethod_PyClass.cxx
PathPlanning_PyModule.cxx
PathPlanning_PyModule.h
```

A module's class code is included directly by the module's source. For example the **pathplanning** module source code **PathPlanning_PyModule.cxx** includes its class definitions using
```
#include "PathPlanningSubdivMethod_PyClass.cxx"
#include "PathPlanningPathFrame_PyClass.cxx"
#include "PathPlanningPath_PyClass.cxx"
#include "PathPlanningGroup_PyClass.cxx"
```

# Implementing Python Extensions in C++

Python extension modules and classes have been implemented using certain coding and naming conventions

### Modules
- Name: static char* *ModuleName*_MODULE = ...
- Methods: static PyMethodDef *ModuleName*ModuleMethods[] = { ... }
- Module definition: static struct PyModuleDef Py*ModuleName*Module = { ... }

### Classes
- Name: static char* *ClassName*_CLASS = ...
- Methods: static PyMethodDef *ClassName*Methods[] = { ... }

## Defining a Module

A Python module is defined using specific data structures and initialization functions. 

- **PyModuleDef** struct: Creates a module definition instance.
- **PyMODINIT_FUNC**: Initialization function. This function is exported from the shared library which loads the API and is executed when the module is loaded from \_\_init\_\_py. It initializes the sructures used by classes to store data and adds the class name to the module.


## Defining a Class

A Python class is defined using specific data structures and initialization functions. 

- **PyTypeObject** struct: Defines the class type. The class member data is defined by a struct that is passed to each class method. 
- **Init()** function: Defines the class \_\_init__() method used to initialize an object after it is created.
- **New()** function: Defines the class \_\_new__() method used to create a new instance of an object. 
- **Dealloc()** function: Defines the class \_\_del__() method called as soon as all references of the object are deleted.
- **SetPyPathTypeFields()** function: Set the PyTypeObject fields that stores class data.

# Modules

## _pathplanning_ Module

The **pathplanning** module code is contained in the files
```
PathPlanningGroup_PyClass.cxx
PathPlanningPathFrame_PyClass.cxx
PathPlanningPath_PyClass.cxx
PathPlanningSubdivMethod_PyClass.cxx
PathPlanning_PyModule.cxx
PathPlanning_PyModule.h
```


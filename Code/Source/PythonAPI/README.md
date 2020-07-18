
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

# Code Organization

The code is organized using seprate files for modules and classes defined for those modules

  - Modules are implemented in *ModuleName*\_PyModule.(cxx,h) files
  
  - Classes are implemented in *ModuleNameClassName*\_PyClass.cxx file. 

For example the **pathplanning** module code is contained in the files
```
PathPlanning_PyModule.cxx
PathPlanning_PyModule.h
PathPlanningPath_PyClass.cxx
PathPlanningGroup_PyClass.cxx
PathPlanningGroup_PyClass.h
PathPlanningSubdivMethod_PyClass.cxx
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
- **PyMODINIT_FUNC**: Initialization function. This function is exported from the shared libraries which loads the API.


## Defining a Class


# Modules

## _pathplanning_ Module

The **path** module code is contained in the files
```
PathPlanningGroup_PyClass.cxx
PathPlanningPathFrame_PyClass.cxx
PathPlanningPath_PyClass.cxx
PathPlanningSubdivMethod_PyClass.cxx
PathPlanning_PyModule.cxx
PathPlanning_PyModule.h
```


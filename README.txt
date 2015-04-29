== What is SimVascular?

A software application for patient-specific cardiovascular modeling
and simulation. It integrates best-in-class commercial components and
custom open-source code, including an integrated flow solver with
outflow boundary conditions and fluid-structure interaction for
cardiovascular problems. SimVascular includes:

1) Image processing and visualization using VTK and ITK from KitWare, Inc.
2) Patient-specific geometric modeling using the Parasolid(r) solid
     modeling kernel from UGS;
3) Automatic mesh generation using MeshSim from Simmetrix, Inc.;
4) Parallel finite element flow solver, developed jointly by RPI and
     Stanford, which incorporates an iterative solver library (LesLib) from
     AcuSim, Inc.

== How do I build SimVascular?

Please consult the README-build-with-Make.

If you are a user who plans on only making simple GUI changes with the Tcl/Tk interface,
you might want to download the binary tarballs instead of building from source.

Please consult the SimVascular wiki:
http://wiki.simtk.org/simvascular/docs

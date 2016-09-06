#
#  untar tcl/tk
#

tar xvf Originals/tcltk/tcl8.5.18-src.tar.gz
tar xvf Originals/tcltk/tk8.5.18-src.tar.gz  
tar xvf Originals/tcltk/tcl8.6.4-src.tar.gz
tar xvf Originals/tcltk/tk8.6.4-src.tar.gz
tar xvf Originals/tcltk/tcllib-1.17.tar.gz
tar xvf Originals/tcltk/tklib-0.6.tar.tgz

#
# move and rename tcl/tk
#

mv tcl8.5.18 ../tcl-8.5.18
mv tk8.5.18 ../tk-8.5.18
mv tcl8.6.4 ../tcl-8.6.4
mv tk8.6.4 ../tk-8.6.4
mv tcllib-1.17 ../tcllib-1.17
mv tklib-0.6 ../tklib-0.6

#  python
rm -Rf ../python-2.7.11
tar xvf Originals/python/python-2.7.11-cmakebuild.tar.gz
mv python-2.7.11-cmakebuild ../python-2.7.11
tar xvf Originals/python/Python-2.7.11.tgz
mv Python-2.7.11 ../python-2.7.11

#  gdcm
tar xvf Originals/gdcm/gdcm-2.6.1.tar.gz
mv gdcm-2.6.1 ..

# freetype2
unzip Originals/freetype/ft263.zip
mv freetype-2.6.3 ..

# vtk
tar xvf Originals/vtk/VTK-6.2.0.tar.gz
mv VTK-6.2.0 ../vtk-6.2.0
tar xvf Originals/vtk/VTK-6.3.0.tar.gz
mv VTK-6.3.0 ../vtk-6.3.0

# itk
tar xvf Originals/itk/InsightToolkit-4.7.1.tar.gz
mv InsightToolkit-4.7.1 ../itk-4.7.1

# opencascade
tar xvf Originals/opencascade/opencascade-7.0.0.tgz
mv opencascade-7.0.0 ..

# mmg
tar xvf Originals/mmg/mmg-5.1.0.tar.gz
mv mmg-5.1.0 ..

# numpy
tar xvf Originals/numpy/numpy-1.11.1.tar.gz
mv numpy-1.11.1 ..

# mitk
tar xvf Originals/mitk/mitk-v2016.03.0.tar.gz
#cd ..
mv mitk-2016.03 ..

# patches
source Patches/patch-source-tcltk-8.5.sh
source Patches/patch-source-vtk-6.2.sh
source Patches/patch-source-vtk-6.3.sh
source Patches/patch-source-mmg-5.1.0.sh
source Patches/patch-source-opencascade-7.0.0.sh
source Patches/patch-source-mitk-2016.03.sh

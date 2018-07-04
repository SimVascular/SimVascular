/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef sv4guiImageProcessing_H
#define sv4guiImageProcessing_H

#include <string>
#include <QString>
#include <sv4gui_QmitkFunctionality.h>

#include "sv4gui_ImageProcessingUtils.h"
#include "sv4gui_DataNodeOperationInterface.h"
#include "sv4gui_DataNodeOperation.h"
#include <sv4gui_ImageSeedContainer.h>
#include <sv4gui_ImageSeedInteractor.h>
#include <sv4gui_ImageSeedMapper.h>

#include <mitkImage.h>


namespace Ui {
class sv4guiImageProcessing;
}

class sv4guiImageProcessing : public sv4guiQmitkFunctionality
{
    Q_OBJECT

public:

    sv4guiImageProcessing();

    virtual ~sv4guiImageProcessing();

    virtual void CreateQtPartControl(QWidget *parent) override;

    static const QString EXTENSION_ID;

    void UpdateImageList();

    std::string getImageName(int imageIndex);

    mitk::Image::Pointer getImage(std::string image_name);

    virtual void OnSelectionChanged(std::vector<mitk::DataNode*> nodes) override;

    sv4guiImageProcessingUtils::itkImPoint getItkImage(int index);

    void addNode(mitk::DataNode::Pointer child_node, mitk::DataNode::Pointer parent_node);

    void storeImage(sv4guiImageProcessingUtils::itkImPoint image);

    void storePolyData(vtkSmartPointer<vtkPolyData> vtkPd);

public slots:

    //display
    void seedSize();
    void displayGuide(bool state);
    void displaySeeds(bool state);

    //displaay buttons
    void imageEditingTabSelected();
    void filteringTabSelected();
    void segmentationTabSelected();
    void pipelinesTabSelected();

    //run buttons
    void runFullCollidingFronts();

    void runThreshold();

    void runBinaryThreshold();

    void runCollidingFronts();

    void runGradientMagnitude();

    void runEditImage();

    void runCropImage();

    void runResampleImage();

    void runZeroLevel();

    void runSmoothing();

    void runAnisotropic();

    void runIsovalue();

    void runGeodesicLevelSet();

public:

protected:

  QString hello_str;

  Ui::sv4guiImageProcessing *ui;

  QWidget *m_parent;

  QmitkStdMultiWidget* m_DisplayWidget;

  std::string m_selectedAlgorithm;

  sv4guiDataNodeOperationInterface* m_Interface;

  sv4guiImageSeedContainer::Pointer m_SeedContainer;

  bool m_init = true;

  sv4guiImageSeedInteractor::Pointer m_SeedInteractor;

  sv4guiImageSeedMapper::Pointer m_SeedMapper;

  sv4guiImageProcessingUtils::itkImPoint CombinedCollidingFronts(
    sv4guiImageProcessingUtils::itkImPoint, double lower, double upper);

  mitk::DataNode::Pointer m_SeedNode;
};

#endif // sv4guiImageProcessing_H

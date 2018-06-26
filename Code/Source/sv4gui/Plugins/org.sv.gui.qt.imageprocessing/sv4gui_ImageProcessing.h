#ifndef sv4guiImageProcessing_H
#define sv4guiImageProcessing_H

#include <QmitkFunctionality.h>
#include <QString>
#include <string>

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

class sv4guiImageProcessing : public QmitkFunctionality
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
    void tabSelected();
    void seedSize();

    //displaay buttons
    void displayEditImageTab();
    void displayCropImageTab();
    void displayResampleImageTab();
    void displaySmoothTab();
    void displayAnisotropicSmoothTab();
    void displayThresholdTab();
    void displayBinaryThresholdTab();
    void displayConnectedThresholdTab();
    void displayCollidingFrontsTab();
    void displayZeroLevelTab();
    void displayFillHolesTab();
    void displayOpenCloseTab();
    void displayGradientMagnitudeTab();
    void displayLevelSetTab();
    void displayIsoSurfaceTab();
    void displayFullCFTab();

    //run buttons
    void runSeedIsovalue();

    void runFullCollidingFronts();

    void runThreshold();

    void runBinaryThreshold();

    void runConnectedThreshold();

    void runCollidingFronts();

    void runGradientMagnitude();

    void runEditImage();

    void runCropImage();

    void runResampleImage();

    void runZeroLevel();

    void runOpenClose();

    void runSmoothing();

    void runAnisotropic();

    void runIsovalue();

    void runFillHoles();

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
};

#endif // sv4guiImageProcessing_H

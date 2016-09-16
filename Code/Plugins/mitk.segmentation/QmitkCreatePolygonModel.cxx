/*===================================================================

The Medical Imaging Interaction Toolkit (MITK)

Copyright (c) German Cancer Research Center,
Division of Medical and Biological Informatics.
All rights reserved.

This software is distributed WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.

See LICENSE.txt or http://www.mitk.org for details.

===================================================================*/
#include "QmitkCreatePolygonModel.h"

// MITK
#include <mitkShowSegmentationAsSmoothedSurface.h>
#include <mitkShowSegmentationAsSurface.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

const QString QmitkCreatePolygonModel::EXTENSION_ID="mitk.createpolygonmodel";

QmitkCreatePolygonModel::QmitkCreatePolygonModel()
{
    QmitkNodeDescriptor* dataNodeDescriptor = getNodeDescriptorManager()->GetDescriptor("ImageMask");
    QAction* action = new QAction(QIcon(":xxx.png"), "Create polygon model", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( Create() ) );
    dataNodeDescriptor->AddAction(action);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(dataNodeDescriptor, action));
    action = new QAction(QIcon(":xxx.png"), "Create smoothed polygon model", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( CreateSmoothed() ) );
    dataNodeDescriptor->AddAction(action);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(dataNodeDescriptor, action));
}

QmitkCreatePolygonModel::~QmitkCreatePolygonModel()
{
    for (std::vector< std::pair< QmitkNodeDescriptor*, QAction* > >::iterator it = mDescriptorActionList.begin();it != mDescriptorActionList.end(); it++)
    {
        // first== the NodeDescriptor; second== the registered QAction
        (it->first)->RemoveAction(it->second);
    }
}

void QmitkCreatePolygonModel::Create()
{
    SetSmoothed(false);
    Exec();
}

void QmitkCreatePolygonModel::CreateSmoothed()
{
    SetSmoothed(true);
    Exec();
}

void QmitkCreatePolygonModel::Exec()
{
    CreatePolygonModel(GetCurrentSelection());
}

void QmitkCreatePolygonModel::CreatePolygonModel(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];
    mitk::Image::Pointer image = dynamic_cast<mitk::Image *>(selectedNode->GetData());

    if (image.IsNull())
    {
        return;
    }

    try
    {
        // Get preference properties for smoothing and decimation
        //    IPreferencesService* prefService = Platform::GetPreferencesService();
        //    IPreferences::Pointer segPref = prefService->GetSystemPreferences()->Node("/org.mitk.views.segmentation");

        //    bool smoothingHint = segPref->GetBool("smoothing hint", true);
        //    ScalarType smoothing = segPref->GetDouble("smoothing value", 1.0);
        //    ScalarType decimation = segPref->GetDouble("decimation rate", 0.5);

        bool smoothingHint = true;
        mitk::ScalarType smoothing = 1.0;
        mitk::ScalarType decimation = 0.5;

        if (smoothingHint)
        {
            smoothing = 0.0;
            mitk::Vector3D spacing = image->GetGeometry()->GetSpacing();

            for (mitk::Vector3D::Iterator iter = spacing.Begin(); iter != spacing.End(); ++iter)
                smoothing = std::max(smoothing, *iter);
        }

        mitk::ShowSegmentationAsSurface::Pointer surfaceFilter = mitk::ShowSegmentationAsSurface::New();

        // Activate callback functions
        itk::SimpleMemberCommand<QmitkCreatePolygonModel>::Pointer successCommand = itk::SimpleMemberCommand<QmitkCreatePolygonModel>::New();
        successCommand->SetCallbackFunction(this, &QmitkCreatePolygonModel::OnSurfaceCalculationDone);
        surfaceFilter->AddObserver(mitk::ResultAvailable(), successCommand);

        itk::SimpleMemberCommand<QmitkCreatePolygonModel>::Pointer errorCommand = itk::SimpleMemberCommand<QmitkCreatePolygonModel>::New();
        errorCommand->SetCallbackFunction(this, &QmitkCreatePolygonModel::OnSurfaceCalculationDone);
        surfaceFilter->AddObserver(mitk::ProcessingError(), errorCommand);

        // set filter parameter
        surfaceFilter->SetDataStorage(*(GetDataStorage()));
        surfaceFilter->SetPointerParameter("Input", image);
        surfaceFilter->SetPointerParameter("Group node", selectedNode);
        surfaceFilter->SetParameter("Show result", true);
        surfaceFilter->SetParameter("Sync visibility", false);
        surfaceFilter->SetParameter("Median kernel size", 3u);
        surfaceFilter->SetParameter("Decimate mesh", m_IsDecimated);
        surfaceFilter->SetParameter("Decimation rate", (float) decimation);

        if (m_IsSmoothed)
        {
            surfaceFilter->SetParameter("Apply median", true);
            surfaceFilter->SetParameter("Smooth", true);
            surfaceFilter->SetParameter("Gaussian SD", sqrtf(smoothing)); // use sqrt to account for setting of variance in preferences
            mitk::StatusBar::GetInstance()->DisplayText("Smoothed surface creation started in background...");
        }
        else
        {
            surfaceFilter->SetParameter("Apply median", false);
            surfaceFilter->SetParameter("Smooth", false);
            mitk::StatusBar::GetInstance()->DisplayText("Surface creation started in background...");
        }

        surfaceFilter->StartAlgorithm();
    }
    catch(...)
    {
        MITK_ERROR << "Surface creation failed!";
    }
}

void QmitkCreatePolygonModel::OnSurfaceCalculationDone()
{
    mitk::StatusBar::GetInstance()->Clear();
}

void QmitkCreatePolygonModel::SetSmoothed(bool smoothed)
{
    m_IsSmoothed = smoothed;
}

void QmitkCreatePolygonModel::SetDecimated(bool decimated)
{
    m_IsDecimated = decimated;
}



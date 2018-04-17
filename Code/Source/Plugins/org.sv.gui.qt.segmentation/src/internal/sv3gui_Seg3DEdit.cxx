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

#include "sv3gui_Seg3DEdit.h"
#include "ui_sv3gui_Seg3DEdit.h"

#include "sv3gui_VtkUtils.h"
#include "sv3gui_Seg3DUtils.h"
#include "sv3gui_MitkSeg3DOperation.h"

#include <QmitkStdMultiWidgetEditor.h>
#include <mitkImage.h>
#include <mitkOperationEvent.h>
#include <mitkUndoController.h>
#include <mitkStatusBar.h>
#include <mitkProgressBar.h>
#include <mitkNodePredicateDataType.h>

#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <usModuleRegistry.h>

// Qt
#include <QMessageBox>
#include <QShortcut>
#include <QInputDialog>
#include <QWheelEvent>
#include <QScrollArea>
#include <QVBoxLayout>
#include <QApplication>

#include <iostream>
using namespace std;

#include <math.h>

const QString svSeg3DEdit::EXTENSION_ID = "org.sv.views.segmentation3d";

svSeg3DEdit::svSeg3DEdit() :
    ui(new Ui::svSeg3DEdit)
{
    m_MitkSeg3DNode=NULL;
    m_MitkSeg3D=NULL;
    m_VtkImage=NULL;
}

svSeg3DEdit::~svSeg3DEdit()
{
    delete ui;
}

void svSeg3DEdit::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);

    m_DisplayWidget=GetActiveStdMultiWidget();

    if(m_DisplayWidget==NULL)
    {
        parent->setEnabled(false);
        MITK_ERROR << "Plugin PathEdit Init Error: No QmitkStdMultiWidget!";
        return;
    }

    connect(ui->checkBoxShowSeeds, SIGNAL(toggled(bool)), this, SLOT(SetSeedVisibility(bool)));

    //for colliding fronts
    ui->widgetThresholdCF->setDecimals(1);
    ui->widgetThresholdCF->setRange(0,200);
    ui->widgetThresholdCF->setValues(50,100);

    connect(ui->btnCreateCF, SIGNAL(clicked()), this, SLOT(CreateByCollidingFronts()) );
}

void svSeg3DEdit::Visible()
{
    OnSelectionChanged(GetDataManagerSelection());
}

void svSeg3DEdit::Hidden()
{
    ClearAll();
}

//bool svSeg3DEdit::IsExclusiveFunctionality() const
//{
//    return true;
//}

void svSeg3DEdit::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
//    if(!IsActivated())
    if(!IsVisible())
    {
        return;
    }

    if(nodes.size()==0)
    {
        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    mitk::DataNode::Pointer mitkSeg3DNode=nodes.front();

    if(m_MitkSeg3DNode==mitkSeg3DNode)
    {
        return;
    }

    ClearAll();

    m_MitkSeg3DNode=mitkSeg3DNode;
    m_MitkSeg3D=dynamic_cast<svMitkSeg3D*>(mitkSeg3DNode->GetData());
    if(!m_MitkSeg3D)
    {
        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    m_Parent->setEnabled(true);

    mitk::DataNode::Pointer imageNode=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_MitkSeg3DNode,isProjFolder,false);

    mitk::Image* mImage=NULL;
    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svImageFolder"));
        if(rs->size()>0)
        {

            mitk::DataNode::Pointer imageFolderNode=rs->GetElement(0);
            rs=GetDataStorage()->GetDerivations(imageFolderNode);
            if(rs->size()<1) return;
            imageNode=rs->GetElement(0);
            mImage= dynamic_cast<mitk::Image*>(imageNode->GetData());
        }
    }

    if(!mImage){
        QMessageBox::warning(NULL,"No image found for this project","Make sure the image is loaded!");
//        return;
    }

    ui->labelSegName->setText(QString::fromStdString(m_MitkSeg3DNode->GetName()));

    if(mImage)
        m_VtkImage=svVtkUtils::MitkImage2VtkImage(mImage);
    else
        m_VtkImage=NULL;

    double range[2]={0,100};
    if(m_VtkImage)
        m_VtkImage->GetScalarRange(range);

    m_DataInteractor = svMitkSeg3DDataInteractor::New();
    if(m_VtkImage)
    {
        double spacing[3];
        m_VtkImage->GetSpacing(spacing);
        double minSpacing=std::min(spacing[0],std::min(spacing[1],spacing[2]));
        m_DataInteractor->SetMinRadius(minSpacing);
    }

    m_DataInteractor->LoadStateMachine("svMitkSeg3DInteraction.xml", us::ModuleRegistry::GetModule("svSegmentation"));
    m_DataInteractor->SetEventConfig("svSegmentationConfig.xml", us::ModuleRegistry::GetModule("svSegmentation"));
    m_DataInteractor->SetDataNode(m_MitkSeg3DNode);

    svSeg3D* seg3D=m_MitkSeg3D->GetSeg3D();
    svSeg3DParam* param=NULL;
    if(seg3D)
        param=&(seg3D->GetParam());

    bool seedVisible=false;
    m_MitkSeg3DNode->GetBoolProperty("seed.visible", seedVisible);
    ui->checkBoxShowSeeds->setChecked(seedVisible);

    std::string method="";
    if(param)
        method=param->method;

    //set ui for colliding fronts
    ui->widgetThresholdCF->setRange(range[0],range[1]);
    ui->widgetThresholdCF->setValues(range[0]+0.25*(range[1]-range[0]),range[0]+0.75*(range[1]-range[0]));
    //update value if the method are used;
    if(method=="colliding fronts")
        ui->widgetThresholdCF->setValues(param->lowerThreshold,param->upperThreshold);

}

void svSeg3DEdit::SetSeedVisibility(bool checked)
{
    if(m_MitkSeg3DNode.IsNotNull())
    {
        m_MitkSeg3DNode->SetProperty( "seed.visible", mitk::BoolProperty::New(checked));
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

void svSeg3DEdit::CreateByCollidingFronts()
{
    if(!m_MitkSeg3D)
        return;

    svSeg3D* seg3D=m_MitkSeg3D->GetSeg3D();
    if(!seg3D)
        return;

    if(!m_VtkImage){
        QMessageBox::warning(NULL,"No image found for this project","Make sure the image is loaded!");
        return;
    }

    svSeg3DParam param=seg3D->GetParam();
    std::map<int,svSeed> seedMap=param.GetSeedMap();

    double spacing[3], origin[3];
    m_VtkImage->GetOrigin(origin);
    m_VtkImage->GetSpacing(spacing);

    std::vector<std::vector<int>> startSeeds;
    std::vector<std::vector<int>> endSeeds;

    for(auto s:seedMap)
    {
        svSeed seed=s.second;
        int ix=(seed.x-origin[0])/spacing[0];
        int iy=(seed.y-origin[1])/spacing[1];
        int iz=(seed.z-origin[2])/spacing[2];

        std::vector<int> iseed{ix,iy,iz};

        if(seed.type=="end")
            endSeeds.push_back(iseed);
        else
            startSeeds.push_back(iseed);
    }

    if(startSeeds.size()==0 || endSeeds.size()==0)
    {
        QMessageBox::warning(NULL,"Seeds Missing","Please add seeds before segmenting!");
        return;
    }

    double lowerThreshold=ui->widgetThresholdCF->minimumValue();
    double upperThreshold=ui->widgetThresholdCF->maximumValue();

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(3);
    mitk::StatusBar::GetInstance()->DisplayText("Creating 3D segmentation...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    vtkSmartPointer<vtkPolyData> vpdSeg=svSeg3DUtils::collidingFronts(m_VtkImage,startSeeds,endSeeds,lowerThreshold,upperThreshold);

    svSeg3D* newSeg3D=new svSeg3D();
    svSeg3DParam newParam;
    newParam.method="colliding fronts";
    newParam.lowerThreshold=lowerThreshold;
    newParam.upperThreshold=upperThreshold;
    newParam.seedMap=param.GetSeedMap();

    newSeg3D->SetParam(newParam);
    newSeg3D->SetVtkPolyData(vpdSeg);

    seg3D->SetParam(seg3D->GetInnerParam());//restore to the original param

    mitk::OperationEvent::IncCurrObjectEventId();
    svMitkSeg3DOperation* doOp = new svMitkSeg3DOperation(svMitkSeg3DOperation::OpSETSEG3D,newSeg3D);
    svMitkSeg3DOperation* undoOp = new svMitkSeg3DOperation(svMitkSeg3DOperation::OpSETSEG3D,seg3D);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_MitkSeg3D, doOp, undoOp, "Set 3D Segmentation");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    m_MitkSeg3D->ExecuteOperation(doOp);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    WaitCursorOff();
    mitk::ProgressBar::GetInstance()->Progress(2);
    mitk::StatusBar::GetInstance()->DisplayText("3D segmentation created.");
}

void svSeg3DEdit::NodeChanged(const mitk::DataNode* node)
{
}

void svSeg3DEdit::NodeAdded(const mitk::DataNode* node)
{
}

void svSeg3DEdit::NodeRemoved(const mitk::DataNode* node)
{
    OnSelectionChanged(GetDataManagerSelection());
}

void svSeg3DEdit::ClearAll()
{
    if(m_MitkSeg3DNode.IsNotNull())
    {
        m_MitkSeg3DNode->SetDataInteractor(NULL);
        m_DataInteractor=NULL;
    }

    m_MitkSeg3D=NULL;
    m_MitkSeg3DNode=NULL;
}

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


#include "sv4gui_svFSIBCWidget.h"
#include "ui_sv4gui_svFSIBCWidget.h"

#include <QMessageBox>
#include <QErrorMessage>
#include <QFileDialog>
#include <QDebug>
#include <QPushButton>

sv4guisvFSIBCWidget::sv4guisvFSIBCWidget(QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::sv4guisvFSIBCWidget)
    , m_RealVal(new QDoubleValidator)
    , m_TemporalFilePath("")
    , m_GeneralFilePath("")
    , m_ProfileFilePath("")
{
    ui->setupUi(this);

    setWindowTitle("Boundary Condition Setup");

    connect(ui->bcGrp, SIGNAL(currentIndexChanged(int )), this, SLOT(SelectGrp(int)));
    connect(ui->steady_box, SIGNAL(clicked(bool)), this, SLOT(SteadyBoxClicked(bool)));
    connect(ui->unsteady_box, SIGNAL(clicked(bool)), this, SLOT(UnsteadyBoxClicked(bool)));
    connect(ui->resistance_box, SIGNAL(clicked(bool)), this, SLOT(ResistanceBoxClicked(bool)));
    connect(ui->coupled_box, SIGNAL(clicked(bool)), this, SLOT(CoupledBoxClicked(bool)));
    connect(ui->general_box, SIGNAL(clicked(bool)), this, SLOT(GeneralBoxClicked(bool)));
    connect(ui->projection_box, SIGNAL(clicked(bool)), this, SLOT(ProjectionBoxClicked(bool)));

    connect(ui->flat_profile, SIGNAL(clicked(bool)), this, SLOT(FlatBoxClicked(bool)));
    connect(ui->parabolic_profile, SIGNAL(clicked(bool)), this, SLOT(ParabolicBoxClicked(bool)));
    connect(ui->user_defined_profile, SIGNAL(clicked(bool)), this, SLOT(UserBoxClicked(bool)));

    ui->steady_value->setValidator(m_RealVal);
    ui->resistance_value->setValidator(m_RealVal);

    connect(ui->btnTemporal, SIGNAL(clicked()), this, SLOT(SetTemporalFile()));
    connect(ui->btnGeneral, SIGNAL(clicked()), this, SLOT(SetGeneralFile()));
    connect(ui->btnProfile, SIGNAL(clicked()), this, SLOT(SetProfileFile()));

    connect(ui->faceList, SIGNAL(itemSelectionChanged()), this, SLOT(FaceListSelectionChanged()));

    connect(ui->flsp, SIGNAL(textChanged(const QString &)), this, SLOT(SearchPatternChanged(const QString &)));

    connect(ui->buttonBox, SIGNAL(clicked(QAbstractButton*)), this, SLOT(ButtonBoxClicked(QAbstractButton*)));
}

sv4guisvFSIBCWidget::~sv4guisvFSIBCWidget()
{
    if(m_RealVal)
        delete m_RealVal;

    delete ui;
}

void sv4guisvFSIBCWidget::Setup(sv4guiMitksvFSIJob* mitkJob, int eqIndx, bool addBC, QStringList faceList, QString jobPath)
{
    m_MitkJob=mitkJob;
    m_EqIndx=eqIndx;
    m_AddingBC=addBC;
    m_FaceList=faceList;
    m_JobPath=jobPath;

    m_Job=mitkJob->GetSimJob();

    if(m_Job==NULL || m_EqIndx<0)
        return;

    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[m_EqIndx];

    //projection list
    ui->comboBoxProjection->clear();
    if(eq.physName=="FSI")
    {
        QStringList fluidFaces;
        for(auto& d: m_Job->m_Domains)
        {
            sv4guisvFSIDomain& domain=d.second;
            if(domain.type=="fluid")
            {
                for(std::string faceName : domain.faceNames)
                    fluidFaces<<QString::fromStdString(faceName);
            }
        }
        fluidFaces.sort();
        ui->comboBoxProjection->addItems(fluidFaces);
    }

    if(m_AddingBC)
    {
        for(auto& d: m_Job->m_Domains)
        {
            sv4guisvFSIDomain& domain=d.second;
            for(std::string faceName : domain.faceNames)
                m_AvailableFaceList<<QString::fromStdString(faceName);
        }

        for(auto& f : eq.faceBCs)
            m_AvailableFaceList.removeAll(QString::fromStdString(f.first));

        m_AvailableFaceList.removeDuplicates();
        m_AvailableFaceList.sort();

        ui->faceList->addItems(m_AvailableFaceList);
    }
    else
    {
        faceList.sort();
        m_AvailableFaceList=faceList;
        ui->faceList->addItems(m_AvailableFaceList);

        ui->faceList->selectAll();
        ui->faceList->setSelectionMode(QAbstractItemView::NoSelection);
        ui->flsp_label->setDisabled(true);
        ui->flsp->setDisabled(true);
    }

    ui->directionalBc->setMaximum(m_Job->nsd);
    LoadBC();
}

void sv4guisvFSIBCWidget::LoadBC()
{
    if(m_Job==NULL || m_EqIndx<0)
        return;

    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[m_EqIndx];

    sv4guisvFSIbcClass bcDefault;
    sv4guisvFSIbcClass bc;

    if(m_AddingBC)
    {
        bc=bcDefault;
    }
    else
    {
        if(m_AvailableFaceList.size()==1)
            bc=eq.faceBCs[m_AvailableFaceList[0].toStdString()];
        else if(m_AvailableFaceList.size()>1)
        {
            bc=eq.faceBCs[m_AvailableFaceList[0].toStdString()];
            foreach(QString name, m_AvailableFaceList)
            {
                sv4guisvFSIbcClass tempbc=eq.faceBCs[name.toStdString()];

                if ( tempbc.bcGrp != bc.bcGrp ) bc.bcGrp = bcDefault.bcGrp;
                if ( tempbc.bcType != bc.bcType ) bc.bcType = bcDefault.bcType;
                if ( tempbc.profile != bc.profile ) bc.profile = bcDefault.profile;
                if ( tempbc.eDrn != bc.eDrn ) bc.eDrn = bcDefault.eDrn;
                if ( tempbc.cplBCPtr != bc.cplBCPtr ) bc.cplBCPtr = bcDefault.cplBCPtr;
                if ( tempbc.r != bc.r ) bc.r = bcDefault.r;
                if ( tempbc.g != bc.g ) bc.g = bcDefault.g;
                if ( tempbc.gmFile != bc.gmFile ) bc.gmFile = bcDefault.gmFile;
                if ( tempbc.gtFile != bc.gtFile ) bc.gtFile = bcDefault.gtFile;
                if ( tempbc.gxFile != bc.gxFile ) bc.gxFile = bcDefault.gxFile;
                if ( tempbc.zperm != bc.zperm ) bc.zperm = bcDefault.zperm;
                if ( tempbc.flux != bc.flux ) bc.flux = bcDefault.flux;
                if ( tempbc.projectionFaceName != bc.projectionFaceName ) bc.projectionFaceName = bcDefault.projectionFaceName;
            }
        }
    }

    if ( bc.eDrn > 0 ) {
        ui->directionalBc_box->setChecked(true);
        ui->directionalBc->setValue(bc.eDrn);
    } else {
        ui->directionalBc_box->setChecked(false);
    }

    int indx=ui->bcGrp->findText(bc.bcGrp);
    ui->bcGrp->setCurrentIndex(indx);
    SelectGrp(indx);

    if ( bc.bcType == "Steady" ) {
        ui->steady_box->setChecked(true);
        SteadyBoxClicked(true);
        ui->steady_value->setText(QString::number(bc.g));
    } else if ( bc.bcType == "Unsteady" ) {
        ui->unsteady_box->setChecked(true);
        UnsteadyBoxClicked(true);
        ui->lineEditTemporal->setText(bc.gtFile);
    } else if ( bc.bcType == "Coupled" ) {
        ui->coupled_box->setChecked(true);
        CoupledBoxClicked(true);
    } else if ( bc.bcType == "Resistance" ) {
        ui->resistance_box->setChecked(true);
        ResistanceBoxClicked(true);
        ui->resistance_value->setText(QString::number(bc.r));
    } else if ( bc.bcType == "General" ) {
        ui->general_box->setChecked(true);
        GeneralBoxClicked(true);
        ui->lineEditGeneral->setText(bc.gmFile);
    } else if ( bc.bcType == "Projection" ) {
        ui->projection_box->setChecked(true);
        ProjectionBoxClicked(true);
        int indx=ui->comboBoxProjection->findText(bc.projectionFaceName);
        ui->comboBoxProjection->setCurrentIndex(indx);
    }

    if ( bc.profile == "Flat" ) {
        ui->flat_profile->setChecked(true);
        FlatBoxClicked(true);
    } else if ( bc.profile == "Parabolic" ) {
        ui->parabolic_profile->setChecked(true);
        ParabolicBoxClicked(true);
    } else if ( bc.profile == "User_defined" ) {
        ui->user_defined_profile->setChecked(true);
        UserBoxClicked(true);
        ui->lineEditProfile->setText(bc.gxFile);
    }

    ui->zero_out_perimeter->setChecked(bc.zperm);
    ui->impose_flux->setChecked(bc.flux);

    ui->checkBoxImposeIntegral->setChecked(bc.imposeIntegral);
    ui->lineEditDirection->setText(bc.effectiveDirection);
}

void sv4guisvFSIBCWidget::SelectGrp(int index)
{
    if ( index == -1 ) {
        ui->time_dependance_box->setEnabled(false);
        ui->profile_box->setEnabled(false);
    } else {
        ui->time_dependance_box->setEnabled(true);
        ui->profile_box->setEnabled(true);
        if ( index == 0 ) {
            ui->zero_out_perimeter->setChecked(true);
            ui->impose_flux->setChecked(true);
        } else if ( index == 1 ) {
            ui->zero_out_perimeter->setChecked(false);
            ui->impose_flux->setChecked(false);
        }
    }
}

void sv4guisvFSIBCWidget::SteadyBoxClicked(bool checked)
{
    if ( checked ) {
        ui->unsteady_box->setChecked(false);
        ui->coupled_box->setChecked(false);
        ui->resistance_box->setChecked(false);
        ui->general_box->setChecked(false);
        ui->projection_box->setChecked(false);
    } else {
        ui->steady_box->setChecked(true);
    }
}

void sv4guisvFSIBCWidget::UnsteadyBoxClicked(bool checked)
{
    if ( checked ) {
        ui->steady_box->setChecked(false);
        ui->coupled_box->setChecked(false);
        ui->resistance_box->setChecked(false);
        ui->general_box->setChecked(false);
        ui->projection_box->setChecked(false);
    } else {
        ui->unsteady_box->setChecked(true);
    }
}

void sv4guisvFSIBCWidget::ResistanceBoxClicked(bool checked)
{
    if ( checked ) {
        ui->steady_box->setChecked(false);
        ui->unsteady_box->setChecked(false);
        ui->coupled_box->setChecked(false);
        ui->general_box->setChecked(false);
        ui->projection_box->setChecked(false);
    } else {
        ui->resistance_box->setChecked(true);
    }
}

void sv4guisvFSIBCWidget::CoupledBoxClicked(bool checked)
{
    if ( checked ) {
        ui->steady_box->setChecked(false);
        ui->unsteady_box->setChecked(false);
        ui->resistance_box->setChecked(false);
        ui->general_box->setChecked(false);
        ui->projection_box->setChecked(false);
    } else {
        ui->coupled_box->setChecked(true);
    }
}

void sv4guisvFSIBCWidget::GeneralBoxClicked(bool checked)
{
    if ( checked ) {
        ui->steady_box->setChecked(false);
        ui->unsteady_box->setChecked(false);
        ui->coupled_box->setChecked(false);
        ui->resistance_box->setChecked(false);
        ui->projection_box->setChecked(false);
    } else {
        ui->general_box->setChecked(true);
    }
}

void sv4guisvFSIBCWidget::ProjectionBoxClicked(bool checked)
{
    if ( checked ) {
        ui->steady_box->setChecked(false);
        ui->unsteady_box->setChecked(false);
        ui->coupled_box->setChecked(false);
        ui->resistance_box->setChecked(false);
        ui->general_box->setChecked(false);
    } else {
        ui->projection_box->setChecked(true);
    }
}

void sv4guisvFSIBCWidget::FlatBoxClicked(bool checked)
{
    if ( checked ) {
        ui->parabolic_profile->setChecked(false);
        ui->user_defined_profile->setChecked(false);
    } else {
        ui->flat_profile->setChecked(true);
    }
}

void sv4guisvFSIBCWidget::ParabolicBoxClicked(bool checked)
{
    if ( checked ) {
        ui->user_defined_profile->setChecked(false);
        ui->flat_profile->setChecked(false);
    } else {
        ui->parabolic_profile->setChecked(true);
    }
}

void sv4guisvFSIBCWidget::UserBoxClicked(bool checked)
{
    if ( checked ) {
        ui->parabolic_profile->setChecked(false);
        ui->flat_profile->setChecked(false);
    } else {
        ui->user_defined_profile->setChecked(true);
    }
}

void sv4guisvFSIBCWidget::ButtonBoxClicked(QAbstractButton *button)
{
    QPushButton *b = qobject_cast<QPushButton*>(button);

    if (b == ui->buttonBox->button(QDialogButtonBox::Ok)) {
        SaveBC();
        accept();
    } else if (b == ui->buttonBox->button(QDialogButtonBox::Cancel)) {
        reject();
    } else if (b == ui->buttonBox->button(QDialogButtonBox::Reset)) {
        ui->flsp->clear();
        LoadBC();
    }
}

void sv4guisvFSIBCWidget::CopyFile(QString filePath, QString fileName)
{
    QFile::copy(filePath,m_JobPath+"/"+fileName);
}

void sv4guisvFSIBCWidget::SaveBC() {
    if ( ui->faceList->selectedItems().isEmpty() )
        return;

    if(m_Job==NULL || m_EqIndx<0)
        return;

    sv4guisvFSIbcClass newBC;

    if ( ui->bcGrp->currentIndex() == -1 ) {
        return;
    } else {
        newBC.bcGrp = ui->bcGrp->currentText();
    }

    if ( ui->directionalBc_box->isChecked() ) {
        newBC.eDrn = ui->directionalBc->value();
    } else {
        newBC.eDrn = -1;
    }

    if ( ui->steady_box->isChecked() ) {
        newBC.bcType = "Steady";
        newBC.g = ui->steady_value->text().toDouble();
    } else if ( ui->unsteady_box->isChecked() ) {
        newBC.bcType = "Unsteady";
        newBC.gtFile = ui->lineEditTemporal->text();
        if(!newBC.gtFile.isEmpty())
            CopyFile(m_TemporalFilePath,newBC.gtFile);
    } else if ( ui->coupled_box->isChecked() ) {
        newBC.bcType = "Coupled";
    } else if ( ui->resistance_box->isChecked() ) {
        newBC.bcType = "Resistance";
        newBC.r = ui->resistance_value->text().toDouble();
    } else if ( ui->general_box->isChecked() ) {
        newBC.bcType = "General";
        newBC.gmFile = ui->lineEditGeneral->text();
        if(!newBC.gmFile.isEmpty())
            CopyFile(m_GeneralFilePath,newBC.gmFile);
    } else if ( ui->projection_box->isChecked() ) {
        newBC.bcType = "Projection";
        newBC.projectionFaceName = ui->comboBoxProjection->currentText();
    }

    if ( ui->flat_profile->isChecked() ) {
        newBC.profile = "Flat";
    } else if ( ui->parabolic_profile->isChecked()  ) {
        newBC.profile = "Parabolic";
    } else if ( ui->user_defined_profile->isChecked()  ) {
        newBC.profile = "User_defined";
        newBC.gxFile = ui->lineEditProfile->text();
        if(!newBC.gxFile.isEmpty())
            CopyFile(m_ProfileFilePath,newBC.gxFile);
    }

    newBC.zperm = ui->zero_out_perimeter->isChecked();
    newBC.flux = ui->impose_flux->isChecked();

    newBC.imposeIntegral=ui->checkBoxImposeIntegral->isChecked();
    newBC.effectiveDirection=ui->lineEditDirection->text();

    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[m_EqIndx];

    foreach ( QListWidgetItem* item , ui->faceList->selectedItems() ) {
        QString name=item->text();
        newBC.faceName=name;
        eq.faceBCs[name.toStdString()]=newBC;
    }

    m_MitkJob->SetDataModified();
}

void sv4guisvFSIBCWidget::SetTemporalFile()
{
    QString fileName = QFileDialog::getOpenFileName(this,tr("Open File"),QDir::currentPath(),tr("Temporal values file (*.*)"));
    if ( fileName.isEmpty())
        return;

    m_TemporalFilePath=fileName;
    QFileInfo info = QFileInfo(fileName);
    ui->lineEditTemporal->setText(info.fileName());
    QDir::setCurrent(info.path());
}

void sv4guisvFSIBCWidget::SetGeneralFile()
{
    QString fileName = QFileDialog::getOpenFileName(this,tr("Open File"),QDir::currentPath(),tr("Temporal and spatial values file (*.*)"));
    if ( fileName.isEmpty())
        return;

    m_GeneralFilePath=fileName;
    QFileInfo info = QFileInfo(fileName);
    ui->lineEditGeneral->setText(info.fileName());
    QDir::setCurrent(info.path());
}

void sv4guisvFSIBCWidget::SetProfileFile()
{
    QString fileName = QFileDialog::getOpenFileName(this,tr("Open File"),QDir::currentPath(),tr("Spatial profile file (*.*)"));
    if ( fileName.isEmpty())
        return;

    m_ProfileFilePath=fileName;
    QFileInfo info = QFileInfo(fileName);
    ui->lineEditProfile->setText(info.fileName());
    QDir::setCurrent(info.path());
}

void sv4guisvFSIBCWidget::FaceListSelectionChanged()
{
    if ( ui->faceList->selectedItems().isEmpty() ) {
        ui->bcGrp->setEnabled(false);
        ui->bcGrp->setCurrentIndex(-1);
    } else {
        ui->bcGrp->setEnabled(true);
    }
}

void sv4guisvFSIBCWidget::SearchPatternChanged(const QString &arg1)
{
    QString searchPattern(arg1);
    searchPattern.append("*");
    searchPattern.prepend("*");
    QRegExp rx(searchPattern);
    rx.setPatternSyntax(QRegExp::Wildcard);

    for ( int i=0 ; i<m_AvailableFaceList.length() ; i++ ) {
        if (rx.exactMatch(m_AvailableFaceList.at(i))) {
            // This is the case that face was not displayed before and now it must be displayed
            if ( ui->faceList->isRowHidden(i) ) {
                ui->faceList->setRowHidden(i,false);
            }
        } else {
            // This is the case that face used to be displayed and now it should be removed from the list
            if ( !ui->faceList->isRowHidden(i) ) {
                ui->faceList->setRowHidden(i,true);
            }
        }
    }
}

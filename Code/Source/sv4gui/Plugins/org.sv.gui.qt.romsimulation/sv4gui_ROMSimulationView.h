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

// The sv4guiROMSimulationView class provides an interface to the 1D Simulation Tool
// Qt GUI widgets used to create the input files needed to run a 1D simulation. 
// A 1D simulation can be run from the GUI or from the command line.
//
#ifndef SV4GUI_SIMULATIONVIEW_ROM_H
#define SV4GUI_SIMULATIONVIEW_ROM_H

#include "org_sv_gui_qt_romsimulation_Export.h"

#include <tuple>

#include "sv4gui_MitkROMSimJob.h"
#include "sv4gui_Mesh.h"
#include "sv4gui_Model.h"
#include "sv4gui_CapBCWidgetROM.h"
#include "sv4gui_SplitBCWidgetROM.h"
#include "sv4gui_QmitkFunctionality.h"
#include "sv4gui_ROMSimulationLinesContainer.h"
#include "sv4gui_ROMSimulationLinesMapper.h"
#include "sv4gui_ROMSimulationPython.h"
#include "sv4gui_ConvertWorkerROM.h"

#include "sv4gui_CapSelectionWidget.h"
#include "sv4gui_ProcessHandlerROM.h"
#include "sv4gui_SolverProcessHandlerROM.h"

#include "sv4gui_ModelDataInteractor.h"

#include <berryIBerryPreferences.h>

#include <QWidget>
#include <QStandardItemModel>
#include <QProcess>
#include <QMessageBox>
#include <QItemSelection>

namespace Ui {
class SV_QT_ROMSIMULATION sv4guiROMSimulationView;
}

typedef std::tuple<std::string,mitk::DataNode::Pointer> NameNodeTuple;

class SV_QT_ROMSIMULATION sv4guiROMSimulationView : public sv4guiQmitkFunctionality
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;
    static const QString MsgTitle;
    static const QString SOLVER_EXECUTABLE_NAME;
    static const QString SOLVER_INSTALL_DIRECTORY;
    static const QString SOLVER_INSTALL_SUB_DIRECTORY;
    static const QString SOLVER_LOG_FILE_NAME;

    // The names of files written by this class.
    static const QString CORONARY_BC_FILE_NAME;
    static const QString INLET_FACE_NAMES_FILE_NAME;
    static const QString MESH_FILE_NAME;
    static const QString MODEL_SURFACE_FILE_NAME;
    static const QString OUTLET_FACE_NAMES_FILE_NAME;
    static const QString RCR_BC_FILE_NAME;
    static const QString RESISTANCE_BC_FILE_NAME;
	static const QString SOLVER_0D_FILE_NAME;
	static const QString SOLVER_1D_FILE_NAME;

    sv4guiROMSimulationView();

    virtual ~sv4guiROMSimulationView();

    enum class DataInputStateType {
        INLET_FACE,
        CENTERLINES,
        BOUNDRY_CONDITIONS,
        SOLVER_PARAMETERS,
        SIMULATION_FILES,
        ALL
    };

    // Used to access the rows in Model Solver BC Table.
    //
    // Mimics the entries in resources/solvertemplate1d.xml.
    //
    enum TableModelSolverRow : int {
        TimeStepParameters = 0,
        NumberofTimesteps = 1,
        TimeStepSize = 2,
        OutputControl = 3,
        NumberofTimeStepsSavingData = 4
    };

    // Used to access the rows in Model Basic Table.
    enum TableModelBasicRow : int {
        Density = 0,
        Viscosity = 1,
    };

    // Used to access the columns in Model Cap BC Table.
    //
    enum TableModelCapType : int {
        Name = 0,
        BCType = 1,
        Values = 2, 
        Pressure = 3, 
        AnalyticShape = 4, 
        Period = 5, 
        PointNumber = 6,
        FourierModes = 7,
        FlipNormal = 8,
        FlowRate = 9,
        OriginalFile = 10,
        TimedPressure = 11, 
        PressurePeriod =  12,
        PressureScaling = 13, 
        RValues = 14, 
        CValues = 15
    };

    // This class defines the states associated with data input to the tool.
    //
    // [DaveP] The idea is to check states in sequence but not sure if this
    // is really needed.
    //
    class DataInputStateName {
        public:
            static const QString INLET_FACE;
            static const QString CENTERLINES;
            static const QString BOUNDRY_CONDITIONS;
            static const QString SOLVER_PARAMETERS;
            static const QString SIMULATION_FILES;
    };

    typedef std::tuple<DataInputStateType, QString, bool> DataInputStateTuple;
    std::vector<DataInputStateTuple> dataInputState = {
        std::make_tuple(DataInputStateType::INLET_FACE, DataInputStateName::INLET_FACE, false),
        std::make_tuple(DataInputStateType::CENTERLINES, DataInputStateName::CENTERLINES, false),
        std::make_tuple(DataInputStateType::BOUNDRY_CONDITIONS, DataInputStateName::BOUNDRY_CONDITIONS, false),
        std::make_tuple(DataInputStateType::SOLVER_PARAMETERS, DataInputStateName::SOLVER_PARAMETERS, false),
        std::make_tuple(DataInputStateType::SIMULATION_FILES, DataInputStateName::SIMULATION_FILES, false)
    };

    // The MaterialModel class defines the names and parameters 
    // for 1D simulation material models.
    //
    class MaterialModel {
        public:
            static const QString LINEAR;
            static const QString OLUFSEN;
            static const std::vector<QString> names;
            class LinearParameters {
                public:
                    static const QString Ehr;
                    static const QString referencePressure;
            };
            class OlufsenParameters {
                public:
                    static const QString k1;
                    static const QString k2;
                    static const QString k3;
                    static const QString exponent;
                    static const QString referencePressure;
            };
    };

    class SurfaceModelSource {
        public:
            static const QString MESH_PLUGIN;
            static const QString MODEL_PLUGIN;
            static const QString READ_FROM_FILE;
            static const std::vector<QString> types;
    };

    class SegmentExportType {
        public:
            static const QString ALL;
            static const QString OUTLET;
            static const std::vector<QString> types;
    };

   class DataExportName {
        public:
            static const QString AREA;
            static const QString FLOW;
            static const QString PRESSURE;
            static const QString RE;
            static const QString WSS;
            static const std::vector<QString> names;
    };

    class CenterlinesSource {
        public:
            static const QString CALCULATE;
            static const QString MODEL_PLUGIN;
            static const QString READ_FROM_FILE;
            static const std::vector<QString> types;
    };

public slots:

    void SaveToManager();
    void ClearAll();
    void AddObservers();
    void RemoveObservers();
    void UpdateFaceListSelection();
    void UpdateGUIBasic();
    void UpdateGUIConvertResults();
    void UpdateGUIMesh();
    void UpdateModelGUI();
    void TableViewBasicDoubleClicked(const QModelIndex& index);

    void TableCapSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );
    void TableViewCapDoubleClicked(const QModelIndex& index);
    void TableViewCapContextMenuRequested(const QPoint& pos);
    void ShowCapBCWidget( bool checked = false );
    void SetDistalPressure( bool checked = false );
    void SetCapBC();

    void ShowSplitBCWidget(QString splitTarget);
    void ShowSplitBCWidgetR( bool checked = false);
    void ShowSplitBCWidgetC( bool checked = false);
    void SplitCapBC();
    void UpdateGUICap();

    void SelectMaterialModel(int index);

    void TableVarSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );
    void TableViewVarContextMenuRequested(const QPoint& pos);
    void SetVarE( bool checked = false );
    void SetVarThickness( bool checked = false );
    void UpdateGUIWall();
    void UpdateGUISolver();
    void UpdateGUIJob();
    void UpdateGUIRunDir();
    void ToolBoxChanged(int index);

    // 1D Mesh slots.
    void Generate1DMesh();
    void Show1DMesh();
    void SetElementSize(QString);
    void ReadMesh();

    void UpdateCenterlinesSource();
    void SelectCenterlinesFile();
    void CalculateCenterlines();
    void ShowCenterlines(bool checked=false);

    void SetModelInletFaces();
    void SelectModelFile();
    void SelectModelInletFaces(bool show = true);
    void ShowModel(bool checked = false);
    void UpdateSurfaceModelSource();

    void CreateSimulationFiles();
    void ImportFiles();//like rcrt.dat, cort.dat, Qhistor.dat, impt.dat,etc.
    void RunJob();

    // Convert Results slots.
    void SelectSegmentExportType(int index);
    void ExportResults();
    void SetResultDir();
    void SetConvertDir();

    void UpdateJobStatus();
    void UpdateSimJob();
    void UpdateSurfaceMeshName();

    void SetupInternalSolverPaths();

    // Slots executed by ConvertWorkerROM signals. 
    void ShowConvertWorkerMessage(const bool errorMsg, const QString& msg);
    void ConvertWorkerError(const QString& msg);
    void ConvertWorkerFinished();

public:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void OnSelectionChanged(std::vector<mitk::DataNode*> nodes) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

//    virtual void Activated() override;

//    virtual void Deactivated() override;

    virtual void Visible() override;

    virtual void Hidden() override;

    virtual void OnPreferencesChanged(const berry::IBerryPreferences* prefs) override;

    sv4guiROMSimJob* CreateJob(std::string& msg, bool checkValidity = true);

    void UpdateCenterlines();
    vtkSmartPointer<vtkPolyData> ReadCenterlines(const std::string fileName);

    void AddConvertResultsParameters(sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface);
    void AddMeshParameters(sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface);
    void AddWallPropertiesParameters(sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface);

    bool CreateDataFiles(QString outputDir, bool outputAllFiles, bool updateJob, bool createFolder);
    std::vector<std::string> ReadInletFaceNames(const QString outputDir);
    void WriteBCFiles(const QString outputDir, sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface);
    void WriteFlowFile(const QString outputDir, sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface);
    void WriteInletFaceNames(const QString outputDir);
    void WriteOutletFaceNames(const QString outputDir, sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface);
    void WriteCoronaryFile(const QString outputDir, sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface);
    void WriteRcrFile(const QString outputDir, sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface);
    void WriteResistanceFile(const QString outputDir, sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface);

    bool IsDouble(std::string value);

    bool AreDouble(std::string values, int* count = NULL);

    bool IsInt(std::string value);

    void EnableTool(bool able);

    QString GetJobPath();

    void EnableConnection(bool able = true);

#if defined(Q_OS_WIN)
    QString FindLatestKey(QString key, QStringList keys);
    QString GetRegistryValue(QString category, QString key);
#endif

private:

    std::vector<std::string> GetSimulationNames();
    void Create1DMeshControls(QWidget *parent);
    void CreateWallPropertiesControls(QWidget *parent);
    void GetSimulationMeshPaths(const std::string& simName, std::string& volumeMeshPath, std::string& wallsMeshPath);

    QWidget* m_Parent;

    Ui::sv4guiROMSimulationView *ui;

    mitk::DataStorage::Pointer m_DataStorage;
    QString m_PluginOutputDirectory;

    sv4guiModel* m_Model;
    mitk::DataNode::Pointer m_ModelFolderNode;
    mitk::DataNode::Pointer m_ModelNode;
    std::string m_ModelNodeTimeModified;
    std::vector<NameNodeTuple> m_ModelCenterlineNodes;
    QString m_ModelFileName;
    QString m_ModelSource;
    sv4guiCapSelectionWidget* m_ModelFaceSelectionWidget;
    std::vector<std::string> m_ModelInletFaceNames;
    bool m_ModelInletFaceSelected;
    std::vector<int> m_ModelInletFaceIds;
    std::vector<std::string> m_ModelOutletFaceNames;

    sv4guiMesh* m_Mesh;
    mitk::DataNode::Pointer m_MeshFolderNode;
    mitk::DataNode::Pointer m_MeshNode;
    std::vector<NameNodeTuple> m_MeshNodes;

    bool m_CenterlinesCalculated;
    QString m_CenterlinesFileName;
    QString m_CenterlinesOutputFileName;
    QString m_CenterlinesSource;
    sv4guiSimulationLinesMapper::Pointer m_CenterlinesMapper;
    sv4guiSimulationLinesContainer::Pointer m_CenterlinesContainer;
    mitk::DataNode::Pointer m_CenterlinesNode;

    QString m_1DMeshFileName;
    mitk::DataNode::Pointer m_1DMeshNode;
    sv4guiSimulationLinesMapper::Pointer m_1DMeshMapper;
    sv4guiSimulationLinesContainer::Pointer m_1DMeshContainer;
    sv4guiMesh* m_1DMesh;
    double m_1DMeshElementSize;

    sv4guiMitkROMSimJob* m_MitkJob;
    mitk::DataNode::Pointer m_JobNode;

    sv4guiModelDataInteractor::Pointer m_DataInteractor;
    long m_ModelSelectFaceObserverTag;

    QmitkStdMultiWidget* m_DisplayWidget;
    QMenu* m_TableMenuCap;
    QMenu* m_TableMenuVar;
    QStandardItemModel* m_TableModelBasic;
    QStandardItemModel* m_TableModelCap;
    QStandardItemModel* m_TableModelVar;

    sv4guiCapBCWidgetROM* m_CapBCWidget;

    sv4guiSplitBCWidgetROM* m_SplitBCWidget;

    QStandardItemModel* m_TableModelSolver;

    QString m_SolverInputFile;

    QString m_InternalPresolverPath;
    QString m_InternalFlowsolverPath;
    QString m_InternalFlowsolverNoMPIPath;
    QString m_InternalPostsolverPath;
    QString m_InternalMPIExecPath;

    QString m_ExternalPresolverPath;
    QString m_ExternalFlowsolverPath;
    QString m_ExternalFlowsolverNoMPIPath;
    bool m_UseMPI;
    QString m_MPIExecPath;
    bool m_UseCustom;
    QString m_SolverTemplatePath;
    QString m_ExternalPostsolverPath;
    QString m_ExternalMPIExecPath;

    bool m_ConnectionEnabled;
    bool m_SimulationFilesCreated = false;

    mitk::DataNode::Pointer getProjectNode();
    mitk::DataNode::Pointer GetModelFolderDataNode();
    QString GetModelFileName();
    void ResetModel();
    void WriteModel();

    void RunOneDSimulationJob(const QString& jobPath);
    void RunZeroDSimulationJob(const QString& jobPath);

    sv4guiMesh* GetDataNodeMesh();
    mitk::DataNode::Pointer GetMeshFolderDataNode();
    std::vector<std::string> GetMeshNames();
    sv4guiMesh* GetSurfaceMesh(const std::string meshName);
    void UpdateSurfaceMesh();

    void Update1DMesh();
    vtkSmartPointer<vtkPolyData> Read1DMesh(const std::string fileName);

    void SetCenterlinesGeometry();

    bool SetBasicParameters(sv4guiROMSimJob* job, std::string& msg, bool checkValidity);
    bool SetCapBcs(sv4guiROMSimJob* job, std::string& msg, bool checkValidity);
    bool SetConvertResultsParameters(sv4guiROMSimJob* job, std::string& msg, bool checkValidity);
    bool SetMeshParameters(sv4guiROMSimJob* job, std::string& msg, bool checkValidity);
    bool SetWallProperites(sv4guiROMSimJob* job, std::string& msg, bool checkValidity);
    bool SetSolverParameters(sv4guiROMSimJob* job, std::string& msg, bool checkValidity);
    QString GetSolverExecutable();

    bool CheckBCsInputState(bool checkValidity=true);
    bool CheckInputState(DataInputStateType type = DataInputStateType::ALL);
    bool CheckSolverInputState(bool checkValidity=true);
    void SetInputState(DataInputStateType checkType, bool value);

    QString GetExportResultsDir();

    sv4guiConvertWorkerROM* m_ConvertWorker;
};

#endif // SV4GUI_ROMSIMULATIONVIEW_H

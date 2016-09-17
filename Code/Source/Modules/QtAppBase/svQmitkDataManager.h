#ifndef SVQMITKDATAMANAGER_H
#define SVQMITKDATAMANAGER_H

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include <svAbstractFunctionality.h>
#include <QmitkNodeDescriptor.h>
//#include <QmitkStdMultiWidget.h>

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateBase.h>

#include <QWidget>
#include <QItemSelection>

// Forward declarations
class QMenu;
class QAction;
class QComboBox;
class QWidgetAction;
class QSlider;
class QModelIndex;
class QTreeView;
class QPushButton;
class QToolBar;
class QMenu;
class QSignalMapper;

class QmitkDataStorageTreeModel;
class QmitkDataManagerItemDelegate;
class QmitkNumberPropertySlider;
class QmitkDataStorageFilterProxyModel;

class svDnDFrameWidget;

class SVQTAPPBASE_EXPORT svQmitkDataManager : public svAbstractFunctionality
{
    Q_OBJECT

public:

    svQmitkDataManager(QWidget *parent = 0);

    virtual ~svQmitkDataManager();

public slots:
    ///
    /// Invoked when the opacity slider changed
    ///
    void OpacityChanged(int value);
    ///
    /// Invoked when the opacity action changed
    /// In this function the the opacity slider is set to the selected nodes opacity value
    ///
    void OpacityActionChanged();
    /// Invoked when the component action changed
    /// In this function the the opacity slider is set to the selected nodes opacity value
    ///
    void ComponentActionChanged();
    ///
    /// Invoked when the color button is pressed
    ///
    void ColorChanged();
    ///
    /// Invoked when the color action changed
    ///
    void ColorActionChanged();
    ///
    /// Invoked when the color button is pressed
    ///
    void TextureInterpolationChanged();
    ///
    /// Invoked when the color action changed
    ///
    void TextureInterpolationToggled ( bool checked );
    ///
    /// \brief Agreggates available colormaps
    ///
    void ColormapMenuAboutToShow ();
    ///
    /// \brief changes the active colormap
    ///
    void ColormapActionToggled (bool);
    ///
    /// SurfaceRepresentationActionToggled
    ///
    void SurfaceRepresentationMenuAboutToShow ();
    ///
    /// SurfaceRepresentationActionToggled
    ///
    void SurfaceRepresentationActionToggled ( bool checked );
    ///
    /// \brief Shows a node context menu.
    ///
    void NodeTableViewContextMenuRequested( const QPoint & index );
    ///
    /// \brief Invoked when an element should be removed.
    ///
    void RemoveSelectedNodes( bool checked = false );
    ///
    /// \brief Invoked when an element should be reinitiliased.
    ///
    void ReinitSelectedNodes( bool checked = false );
    ///
    /// \brief Invoked when the visibility of the selected nodes should be toggled.
    ///
    void MakeAllNodesInvisible ( bool checked = false );
    ///
    /// \brief Makes all selected nodes visible, all other nodes invisible.
    ///
    void ShowOnlySelectedNodes ( bool checked = false );
    ///
    /// \brief Invoked when the visibility of the selected nodes should be toggled.
    ///
    void ToggleVisibilityOfSelectedNodes ( bool checked = false );
    ///
    /// \brief Invoked when infos of the selected nodes should be shown in a dialog.
    ///
    void ShowInfoDialogForSelectedNodes ( bool checked = false );
    ///
    /// \brief Reinits everything.
    ///
    void GlobalReinit ( bool checked = false );
    ///
    /// \brief will be toggled when a extension point context menu action is toggled
    /// this is a proxy method which will load the corresponding extension class
    /// and run IContextMenuAction
    ///
//    void ContextMenuActionTriggered( bool );

    /// When rows are inserted auto expand them
    void NodeTreeViewRowsInserted ( const QModelIndex & parent, int start, int end );

    /// will setup m_CurrentRowCount
    void NodeTreeViewRowsRemoved ( const QModelIndex & parent, int start, int end );

    /// Whenever the selection changes set the "selected" property respectively
    void NodeSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void ExportSelectedNodes( bool );

    QList<mitk::DataNode::Pointer> GetCurrentSelection();

//    void OpenPathCreateDialog(bool);

//    void OpenPathEditWidget(bool);

    QTreeView* GetTreeView();

    QMenu* GetNodeMenu();

protected:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;
    ///
    /// \brief Shows a file open dialog.
    ///
    void FileOpen( const char * fileName, mitk::DataNode* parentNode );

    QWidget* m_Parent;
    svDnDFrameWidget* m_DndFrameWidget;
    mitk::DataStorage::Pointer 	m_DataStorage;

    ///
    /// \brief A plain widget as the base pane.
    ///
    QmitkDataStorageTreeModel* m_NodeTreeModel;
    QmitkDataStorageFilterProxyModel* m_FilterModel;
    mitk::NodePredicateBase::Pointer m_HelperObjectFilterPredicate;
    mitk::NodePredicateBase::Pointer m_NodeWithNoDataFilterPredicate;
    ///
    /// \brief The Table view to show the selected nodes.
    ///
    QTreeView* m_NodeTreeView;
    ///
    /// \brief The context menu that shows up when right clicking on a node.
    ///
    QMenu* m_NodeMenu;
    ///
    /// \brief flag indicating whether a surface created from a selected decimation is decimated with vtkQuadricDecimation or not
    ///
    bool m_SurfaceDecimation;

    ///# A list of ALL actions for the Context Menu
    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > m_DescriptorActionList;

    /// A Slider widget to change the opacity of a node
    QSlider* m_OpacitySlider;
    /// A Slider widget to change the rendered vector component of an image
    QmitkNumberPropertySlider* m_ComponentSlider;
    /// button to change the color of a node
    QPushButton* m_ColorButton;
    /// TextureInterpolation action
    QAction* m_TextureInterpolation;
    /// SurfaceRepresentation action
    QAction* m_SurfaceRepresentation;
    /// Lookuptable selection action
    QAction* m_ColormapAction;

    /// Maps "Show in" actions to editor ids
    QSignalMapper* m_ShowInMapper;

    /// A list of "Show in" actions
    QList<QAction*> m_ShowInActions;

    /// saves the current amount of rows shown in the datamanager
    size_t m_CurrentRowCount;

    /// if true, GlobalReinit() is called if a node is deleted
    bool  m_GlobalReinitOnNodeDelete;

    QmitkDataManagerItemDelegate* m_ItemDelegate;

};

#endif // SVQMITKDATAMANAGER_H

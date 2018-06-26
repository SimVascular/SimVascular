
#ifndef sv4guisvFSIBCWIDGET_H
#define sv4guisvFSIBCWIDGET_H

#include "sv4gui_MitksvFSIJob.h"

#include <QDialog>
#include <QAbstractButton>
#include <QDoubleValidator>

namespace Ui {
class sv4guisvFSIBCWidget;
}

class sv4guisvFSIBCWidget : public QDialog
{
    Q_OBJECT

public:
    explicit sv4guisvFSIBCWidget(QWidget *parent = 0);
    ~sv4guisvFSIBCWidget();

    void Setup(sv4guiMitksvFSIJob* mitkJob, int eqIndx, bool addBC, QStringList faceList, QString jobPath);

private slots:

    void LoadBC();

    void SelectGrp(int index);

    void SteadyBoxClicked(bool checked);
    void UnsteadyBoxClicked(bool checked);
    void ResistanceBoxClicked(bool checked);
    void CoupledBoxClicked(bool checked);
    void GeneralBoxClicked(bool checked);
    void ProjectionBoxClicked(bool checked);

    void FlatBoxClicked(bool checked);

    void ParabolicBoxClicked(bool checked);

    void UserBoxClicked(bool checked);

    void ButtonBoxClicked(QAbstractButton *button);

    void SaveBC();

    void SetTemporalFile();

    void SetGeneralFile();

    void SetProfileFile();

    void FaceListSelectionChanged();

    void SearchPatternChanged(const QString &arg1);

    void CopyFile(QString filePath, QString fileName);

private:
    Ui::sv4guisvFSIBCWidget *ui;

    QDoubleValidator*m_RealVal;

    bool m_AddingBC;

    sv4guiMitksvFSIJob* m_MitkJob;
    sv4guisvFSIJob* m_Job;

    int m_EqIndx;

    QStringList m_FaceList;

    QString m_JobPath;

    QString m_TemporalFilePath;
    QString m_GeneralFilePath;
    QString m_ProfileFilePath;

    QStringList m_AvailableFaceList;
};

#endif // sv4guisvFSIBCWIDGET_H

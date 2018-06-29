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

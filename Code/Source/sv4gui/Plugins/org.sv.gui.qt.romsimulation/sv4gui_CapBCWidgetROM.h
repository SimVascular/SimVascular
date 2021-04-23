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

#ifndef SV4GUI_CAPBCWIDGETROM_H
#define SV4GUI_CAPBCWIDGETROM_H

#include "org_sv_gui_qt_romsimulation_Export.h"

#include <QWidget>
#include <set>

namespace Ui {
class SV_QT_ROMSIMULATION sv4guiCapBCWidgetROM;
}

class SV_QT_ROMSIMULATION sv4guiCapBCWidgetROM : public QWidget
{
    Q_OBJECT

public:
    explicit sv4guiCapBCWidgetROM(QWidget *parent = 0);

    ~sv4guiCapBCWidgetROM();

    // This class defines valid boundary condition types.
    class BCType {
      public:
        static const std::string CORONARY;
        static const std::string PRESCRIBED_VELOCITIES;
        static const std::string RCR;
        static const std::string RESISTANCE;
        static const std::set<std::string> types;
        static bool isValid(const std::string& bcType);
    };

    void UpdateGUI(std::string capName, std::map<std::string, std::string> props);

    std::map<std::string, std::string> GetProps();

    bool CreateProps();

    bool IsDouble(QString value);

    bool AreDouble(QString values, int* count = NULL);

public slots:

    void Confirm();
    void Cancel();

    void SelectionChanged(const QString &text);

    void LoadFlowrateFromFile();

    void LoadTimedPressureFromFile();

signals:
    void accepted();

private:
    Ui::sv4guiCapBCWidgetROM *ui;

//    QString m_FileName;
    std::string m_FlowrateContent;

    std::map<std::string, std::string> m_Props;

    std::string m_TimedPressureContent;
};

#endif // SV4GUI_CAPBCWIDGETROM_H

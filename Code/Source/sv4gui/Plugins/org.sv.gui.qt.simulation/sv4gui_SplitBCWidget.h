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

#ifndef SV4GUI_SPLITBCWIDGET_H
#define SV4GUI_SPLITBCWIDGET_H

#include <QWidget>

namespace Ui {
class sv4guiSplitBCWidget;
}

class sv4guiSplitBCWidget : public QWidget
{
    Q_OBJECT

public:
    explicit sv4guiSplitBCWidget(QWidget *parent = 0);

    ~sv4guiSplitBCWidget();

    void UpdateGUI(QString bcType, QString splitTarget);

    std::map<std::string, std::string> GetProps();

    bool SetValues();

    bool IsDouble(QString value);

    bool AreDouble(QString values, int* count = NULL);

    QString GetBCType() {return m_BCType;}

    QString GetSplitTarget() {return m_SplitTarget;}

    double GetTotalValue() {return m_TotalValue;}

    double GetMurrayCoefficient() {return m_MurrayCoefficient;}

    double GetPercentage1() {return m_Percentage1;}
    double GetPercentage2() {return m_Percentage2;}
    double GetPercentage3() {return m_Percentage3;}

public slots:

    void Confirm();
    void Cancel();

signals:
    void accepted();

private:
    Ui::sv4guiSplitBCWidget *ui;

    QString m_BCType;
    QString m_SplitTarget;

    double m_TotalValue;
    double m_MurrayCoefficient;
    double m_Percentage1;
    double m_Percentage2;
    double m_Percentage3;
};

#endif // SV4GUI_SPLITBCWIDGET_H

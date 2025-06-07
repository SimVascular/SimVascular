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

#include <QStringList>
#include <QColor>
#include <QRegularExpression>
#include <mitkAnatomicalStructureColorPresets.h>

namespace mitk
{
  struct OrganNamesHandling
  {
    static QStringList GetDefaultOrganColorString()
    {
      QStringList organColors;

      mitk::AnatomicalStructureColorPresets* anatomicalStructureColorPresets = mitk::AnatomicalStructureColorPresets::New();
      anatomicalStructureColorPresets->LoadPreset();

      for (const auto& preset : anatomicalStructureColorPresets->GetColorPresets())
      {
        auto organName = preset.first.c_str();
        auto color = QColor(preset.second.GetRed(), preset.second.GetGreen(), preset.second.GetBlue());

        AppendToOrganList(organColors, organName, color);
      }
      return organColors;
    }

    static void UpdateOrganList(QStringList& organColors, const QString& organname, mitk::Color color)
    {
      QString listElement(organname + QColor(color.GetRed() * 255, color.GetGreen() * 255, color.GetBlue() * 255).name());

      // remove previous definition if necessary

/* [TODO:DaveP] i don't know how to convert this code.
      int oldIndex = organColors.indexOf(QRegularExpression(QRegularExpression::escape(organname) + "#......", Qt::CaseInsensitive));
      if (oldIndex < 0 || organColors.at(oldIndex) != listElement)
      {
        if (oldIndex >= 0)
        {
          organColors.removeAt(oldIndex);
        }

        // add colored organ name AND sort list
        organColors.append(listElement);
        organColors.sort();
      }
*/
    }

    static void AppendToOrganList(QStringList& organColors, const QString& organname, const QColor& color)
    {
      organColors.append(organname + color.name());
    }
  };
}

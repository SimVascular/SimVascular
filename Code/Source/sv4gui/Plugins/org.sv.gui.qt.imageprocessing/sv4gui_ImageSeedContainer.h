#ifndef sv4guiImageSEEDCONTAINER_H
#define sv4guiImageSEEDCONTAINER_H

#include <iostream>
#include <vector>
#include "mitkBaseData.h"

class sv4guiImageSeedContainer : public mitk::BaseData {

public:

  mitkClassMacro(sv4guiImageSeedContainer, mitk::BaseData);
  itkFactorylessNewMacro(Self)
  itkCloneMacro(Self)

  void addStartSeed(double x, double y, double z);
  void addEndSeed(double x, double y, double z, int seedIndex);
  int getNumStartSeeds() const;
  int getNumEndSeeds(int startSeedIndex) const;
  std::vector<double> getStartSeed(int seedIndex) const;
  std::vector<double> getEndSeed(int startSeedIndex, int endSeedIndex) const;
  std::vector<int> findNearestSeed(double x, double y, double z, double tol);
  void deleteSeed(int startIndex, int endIndex);
  double distance(double x1,double y1,double z1,double x2,double y2,double z2) const;
  //virtual methods, that need to be implemented due to mitk::BaseData inheriting
  //from itk::DataObject
  //however if we dont intend to use this object with an itk filter we can leave them
  //empty
  virtual void UpdateOutputInformation() {};
  virtual void SetRequestedRegionToLargestPossibleRegion() {};
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() { return false;};
  virtual bool VerifyRequestedRegion() { return true;};
  virtual void SetRequestedRegion(const itk::DataObject *data) {};

  std::vector<double> hoverPoint = std::vector<double>();

protected:

  mitkCloneMacro(Self);
  sv4guiImageSeedContainer();
  sv4guiImageSeedContainer(const sv4guiImageSeedContainer& other);
  virtual ~sv4guiImageSeedContainer();

private:

  std::vector< std::vector<double> > m_startSeeds;
  std::vector< std::vector< std::vector<double> > > m_endSeeds;
};

#endif //sv4guiImageSEEDCONTAINER_H

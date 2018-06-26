#include "sv4gui_ImageSeedContainer.h"
#include "math.h"

sv4guiImageSeedContainer::sv4guiImageSeedContainer(){
  m_startSeeds = std::vector<std::vector<double>>();
  m_endSeeds = std::vector<std::vector<std::vector<double>>>();
  hoverPoint.push_back(0.0);
  hoverPoint.push_back(0.0);
  hoverPoint.push_back(0.0);
};

sv4guiImageSeedContainer::sv4guiImageSeedContainer(const sv4guiImageSeedContainer& other)
  :BaseData(other)
{
  int numStartSeeds = other.getNumStartSeeds();
  for(int start = 0; start < numStartSeeds; start++){

    std::vector<double> startSeed  = other.getStartSeed(start);
    addStartSeed(startSeed[0], startSeed[1], startSeed[2]);

    int numEndSeeds = other.getNumEndSeeds(start);
    for (int end = 0; end < numEndSeeds; end++){
      std::vector<double> endSeed = other.getEndSeed(start, end);
      addEndSeed(start, endSeed[0], endSeed[1], endSeed[2]);
    }
  }
};

sv4guiImageSeedContainer::~sv4guiImageSeedContainer(){

};

void sv4guiImageSeedContainer::addStartSeed(double x, double y, double z){
  auto v = std::vector<double>();
  v.push_back(x);
  v.push_back(y);
  v.push_back(z);

  m_startSeeds.push_back(v);

  auto v2 = std::vector<std::vector<double>>();
  m_endSeeds.push_back(v2);
};

void sv4guiImageSeedContainer::addEndSeed(double x, double y, double z, int seedIndex){
  auto v = std::vector<double>();
  v.push_back(x);
  v.push_back(y);
  v.push_back(z);

  m_endSeeds[seedIndex].push_back(v);

};

int sv4guiImageSeedContainer::getNumStartSeeds() const {
  return m_startSeeds.size();
}

int sv4guiImageSeedContainer::getNumEndSeeds(int startSeedIndex) const {
  return m_endSeeds[startSeedIndex].size();
}

std::vector<double> sv4guiImageSeedContainer::getStartSeed(int seedIndex) const {
  return m_startSeeds[seedIndex];
}

std::vector<double> sv4guiImageSeedContainer::getEndSeed(int startSeedIndex, int endSeedIndex) const{
  return m_endSeeds[startSeedIndex][endSeedIndex];
}

std::vector<int> sv4guiImageSeedContainer::findNearestSeed(double x, double y, double z, double tol){

  bool done = false;

  int numStartSeeds = getNumStartSeeds();

  auto v = std::vector<int>();
  v.push_back(-1);
  v.push_back(-1);

  if (numStartSeeds == 0) return v;

  for (int start = 0; start < numStartSeeds; start++){
    auto v_start = m_startSeeds[start];
    auto d       = distance(v_start[0], v_start[1], v_start[2], x, y ,z);

    if (d < tol) {
      v[0] = start;
      v[1] = -1;
      return v;
    }

    int numEndSeeds = getNumEndSeeds(start);
    for (int end = 0; end < numEndSeeds; end++){
      auto v_end = m_endSeeds[start][end];

      auto d     = distance(v_end[0], v_end[1], v_end[2], x, y, z);
      if (d < tol){
        v[0] = start;
        v[1] = end;
        return v;
      }
    }
  }
  return v;
}

void sv4guiImageSeedContainer::deleteSeed(int startIndex, int endIndex){
  std::cout << "Deleting " << startIndex << " " << endIndex << "\n";
  std::cout << "startSeeds size " << m_startSeeds.size() << "\n";

  if (m_startSeeds.size() <= startIndex)
    return;

  if (endIndex == -1){
    m_startSeeds.erase(m_startSeeds.begin()+startIndex);
    if (!(m_endSeeds.size() <= startIndex))
      m_endSeeds.erase(m_endSeeds.begin()+startIndex);

    return;
  }

  std::vector<std::vector<double>>& seedVec = m_endSeeds[startIndex];
  seedVec.erase(seedVec.begin()+endIndex);
  return;
}

double sv4guiImageSeedContainer::distance(double x1, double y1, double z1, double x2,
  double y2, double z2) const{

    auto d1 = (x1-x2)*(x1-x2);
    auto d2 = (y1-y2)*(y1-y2);
    auto d3 = (z1-z2)*(z1-z2);
    return sqrt(d1+d2+d3);
}

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

#include "sv4gui_ImageSeedContainer.h"
#include "math.h"
#include <tuple>

//--------------------------
// sv4guiImageSeedContainer
//--------------------------
//
sv4guiImageSeedContainer::sv4guiImageSeedContainer()
{
  m_NumSeeds = 0;
  m_NumStartSeeds = 0;
  m_startSeeds = std::vector<std::vector<double>>();
  m_endSeeds = std::vector<std::vector<std::vector<double>>>();
  hoverPoint.push_back(0.0);
  hoverPoint.push_back(0.0);
  hoverPoint.push_back(0.0);
  selectStartSeed = false;
  selectStartSeedIndex = -1;
  selectEndSeed = false;
  selectEndSeedIndex = -1;
};

sv4guiImageSeedContainer::sv4guiImageSeedContainer(const sv4guiImageSeedContainer& other) :BaseData(other)
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

//--------------
// addStartSeed
//--------------
//
void sv4guiImageSeedContainer::addStartSeed(double x, double y, double z)
{
  auto v = std::vector<double>();
  v.push_back(x);
  v.push_back(y);
  v.push_back(z);
  m_startSeeds.push_back(v);
  auto v2 = std::vector<std::vector<double>>();
  m_endSeeds.push_back(v2);

  m_Seeds.emplace_back(ImageSeed(m_NumSeeds,x,y,z), std::vector<ImageSeed>()); 
  m_NumSeeds += 1;
  m_NumStartSeeds += 1;
};

//------------
// addEndSeed
//------------
//
void sv4guiImageSeedContainer::addEndSeed(double x, double y, double z, int seedIndex)
{
  std::cout << "========== sv4guiImageSeedContainer::addEndSeed ========== " << std::endl;
  std::cout << "[addEndSeed] seedIndex: " << seedIndex << std::endl;
  auto v = std::vector<double>();
  v.push_back(x);
  v.push_back(y);
  v.push_back(z);

  m_endSeeds[seedIndex].push_back(v);

  std::get<1>(m_Seeds[seedIndex]).emplace_back(ImageSeed(m_NumSeeds,x,y,z)); 
  //std::get<1>(m_Seeds[m_NumStartSeeds-1]).emplace_back(ImageSeed(m_NumSeeds,x,y,z)); 
  m_NumSeeds += 1;
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

//-----------------
// findNearestSeed
//-----------------
//
std::vector<int> 
sv4guiImageSeedContainer::findNearestSeed(double x, double y, double z, double tol)
{
  bool done = false;
  int numStartSeeds = getNumStartSeeds();

  auto seedIDs = std::vector<int>();
  seedIDs.push_back(-1);
  seedIDs.push_back(-1);

  if (numStartSeeds == 0) {
    return seedIDs;
  }

  // Search seed points.
  //
  for (int i = 0; i < numStartSeeds; i++){
    auto point = m_startSeeds[i];
    auto d = distance(point[0], point[1], point[2], x, y ,z);

    if (d < tol) {
      seedIDs[0] = i;
      seedIDs[1] = -1;
      return seedIDs;
    }

    int numEndSeeds = getNumEndSeeds(i);
    for (int j = 0; j < numEndSeeds; j++){
      auto point = m_endSeeds[i][j];
      auto d = distance(point[0], point[1], point[2], x, y, z);
      if (d < tol){
        //seedIDs[0] = i;
        seedIDs[1] = j;
        return seedIDs;
      }
    }
  }

  return seedIDs;
}

//------------
// deleteSeed
//------------
// Remove seeds.
//
void sv4guiImageSeedContainer::deleteSeed(int startIndex, int endIndex)
{
  if (startIndex >= m_startSeeds.size()) {
    return;
  }

  // Remove all end seeds for the given start seed.
  //
  if (endIndex == -1) {
    m_startSeeds[startIndex];
    //m_startSeeds.erase(m_startSeeds.begin()+startIndex);

    if (!(m_endSeeds.size() <= startIndex)) {
      m_endSeeds.erase(m_endSeeds.begin()+startIndex);
    }

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

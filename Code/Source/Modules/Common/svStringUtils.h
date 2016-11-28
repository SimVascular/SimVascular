#ifndef SVSTRINGUTILS_H
#define SVSTRINGUTILS_H

#include "SimVascular.h"

#include <svCommonExports.h>

#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <functional>
#include <cctype>
#include <locale>

// I'm having problems getting the static members to resolve properly on MSVC,
// so use private static functions instead.

static std::vector<std::string> svStringUtils_split(const std::string &s, char delim)
{
    std::stringstream ss(s);
    std::string item;
    std::vector<std::string> elems;
    while (std::getline(ss, item, delim)) {
        if (item.length() > 0) {
            elems.push_back(item);
        }
    }
    return elems;
}

static std::string svStringUtils_ltrim(std::string s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(),
            std::not1(std::ptr_fun<int, int>(std::isspace))));
    return s;
}

static std::string svStringUtils_rtrim(std::string s) {
    s.erase(std::find_if(s.rbegin(), s.rend(),
            std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
    return s;
}

static std::string svStringUtils_trim(std::string s) {
    return svStringUtils_ltrim(svStringUtils_rtrim(s));
}

static std::string svStringUtils_lower(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), ::tolower);
    return s;
}

class SVCOMMON_EXPORT svStringUtils
{

 public:

   static std::vector<std::string> split(const std::string &s, char delim = ' ');

   static std::string ltrim(std::string s);

   static std::string rtrim(std::string s);

   static std::string trim(std::string s);

   static std::string lower(std::string s);
};


#endif // SVSTRINGUTILS_H

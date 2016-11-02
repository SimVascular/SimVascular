#ifndef SVSTRINGUTILS_H
#define SVSTRINGUTILS_H

#include "SimVascular.h"

#include <svCommonExports.h>

#include <string>
#include <vector>

//class SVCOMMON_EXPORT svStringUtils
//{
//public:

////    static std::vector<std::string> Split(const std::string & s, std::string rgx_str = "\\s+");

//    static std::vector<std::string> Split(const std::string &s, char delim = ' ');

//    static std::string &LTrim(std::string &s);

//    static std::string &RTrim(std::string &s);

//    static std::string &Trim(std::string &s);

//    static std::string &ToLower(std::string &s);
//};

namespace sv
{

//    SVCOMMON_EXPORT std::vector<std::string> Split(const std::string & s, std::string rgx_str = "\\s+");

    SVCOMMON_EXPORT std::vector<std::string> split(const std::string &s, char delim = ' ');

    SVCOMMON_EXPORT std::string &ltrim(std::string &s);

    SVCOMMON_EXPORT std::string &rtrim(std::string &s);

    SVCOMMON_EXPORT std::string &trim(std::string &s);

    SVCOMMON_EXPORT std::string &lower(std::string &s);
}


#endif // SVSTRINGUTILS_H

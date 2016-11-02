#ifndef SVSTRINGUTILS_H
#define SVSTRINGUTILS_H

#include "SimVascular.h"

#include <svCommonExports.h>

#include <string>
#include <vector>

class SVCOMMON_EXPORT svStringUtils
{
public:

//    static std::vector<std::string> Split(const std::string & s, std::string rgx_str = "\\s+");

    static std::vector<std::string> Split(const std::string &s, char delim = ' ');

    static std::string &LTrim(std::string &s);

    static std::string &RTrim(std::string &s);

    static std::string &Trim(std::string &s);
};

#endif // SVSTRINGUTILS_H

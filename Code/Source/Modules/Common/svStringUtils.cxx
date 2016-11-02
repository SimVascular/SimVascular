#include "svStringUtils.h"

//#include <string>
//#include <regex>
#include <sstream>
#include <algorithm>
#include <functional>
#include <cctype>
#include <locale>

//std::vector<std::string> svStringUtils::Split(const std::string & s, std::string rgx_str)
//{
//        std::vector<std::string> elems;

//        std::regex rgx (rgx_str);

//        std::sregex_token_iterator iter(s.begin(), s.end(), rgx, -1);
//        std::sregex_token_iterator end;

//        while (iter != end)  {
//            elems.push_back(*iter);
//            ++iter;
//        }

//        return elems;
//}


//std::vector<std::string> svStringUtils::Split(const std::string &s, char delim)
//{
//    std::stringstream ss(s);
//    std::string item;
//    std::vector<std::string> elems;
//    while (std::getline(ss, item, delim)) {
//        if (item.length() > 0) {
//            elems.push_back(item);
//        }
//    }
//    return elems;
//}

//std::string &svStringUtils::LTrim(std::string &s) {
//    s.erase(s.begin(), std::find_if(s.begin(), s.end(),
//            std::not1(std::ptr_fun<int, int>(std::isspace))));
//    return s;
//}

//std::string &svStringUtils::RTrim(std::string &s) {
//    s.erase(std::find_if(s.rbegin(), s.rend(),
//            std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
//    return s;
//}

//std::string &svStringUtils::Trim(std::string &s) {
//    return LTrim(RTrim(s));
//}

//std::string &svStringUtils::ToLower(std::string &s) {
//    std::transform(s.begin(), s.end(), s.begin(), ::tolower);
//    return s;
//}


std::vector<std::string> sv::split(const std::string &s, char delim)
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

std::string &sv::ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(),
            std::not1(std::ptr_fun<int, int>(std::isspace))));
    return s;
}

std::string &sv::rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(),
            std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
    return s;
}

std::string &sv::trim(std::string &s) {
    return ltrim(rtrim(s));
}

std::string &sv::lower(std::string &s) {
    std::transform(s.begin(), s.end(), s.begin(), ::tolower);
    return s;
}

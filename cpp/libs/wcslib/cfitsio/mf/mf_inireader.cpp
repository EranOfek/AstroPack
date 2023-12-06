#include <algorithm>
#include <cctype>
#include <cstdlib>
#include "mf_ini.h"
#include "mf_inireader.h"


IniReader::IniReader()
{
    _error = 0;
}


bool IniReader::open(const std::string& filename)
{
    _error = ini_parse(filename.c_str(), valueHandler, this);
    return true;
}


void IniReader::load(const std::string& text)
{
    _error = ini_parse_string(text.c_str(), valueHandler, this);
}


int IniReader::parseError() const
{
    return _error;
}


std::string IniReader::get(const std::string& section, const std::string& name, const std::string& default_value) const
{
    std::string key = makeKey(section, name);
    // Use _values.find() here instead of _values.at() to support pre C++11 compilers
    return _values.count(key) ? _values.find(key)->second : default_value;
}


std::string IniReader::getString(const std::string& section, const std::string& name, const std::string& default_value) const
{
    const std::string str = get(section, name, "");
    return str.empty() ? default_value : str;
}


long IniReader::getInt(const std::string& section, const std::string& name, long default_value) const
{
    std::string valstr = get(section, name, "");
    const char* value = valstr.c_str();
    char* end;
    // This parses "1234" (decimal) and also "0x4D2" (hex)
    long n = strtol(value, &end, 0);
    return end > value ? n : default_value;
}


double IniReader::getFloat(const std::string& section, const std::string& name, double default_value) const
{
    std::string valstr = get(section, name, "");
    const char* value = valstr.c_str();
    char* end;
    double n = strtod(value, &end);
    return end > value ? n : default_value;
}


bool IniReader::getBool(const std::string& section, const std::string& name, bool default_value) const
{
    std::string valstr = get(section, name, "");
    // Convert to lower case to make string comparisons case-insensitive
    std::transform(valstr.begin(), valstr.end(), valstr.begin(), ::tolower);
    if (valstr == "true" || valstr == "yes" || valstr == "on" || valstr == "1")
        return true;
    else if (valstr == "false" || valstr == "no" || valstr == "off" || valstr == "0")
        return false;
    else
        return default_value;
}


bool IniReader::hasSection(const std::string& section) const
{
    const std::string key = makeKey(section, "");
    std::map<std::string, std::string>::const_iterator pos = _values.lower_bound(key);
    if (pos == _values.end())
        return false;
    // Does the key at the lower_bound pos start with "section"?
    return pos->first.compare(0, key.length(), key) == 0;
}


bool IniReader::hasValue(const std::string& section, const std::string& name) const
{
    std::string key = makeKey(section, name);
    return _values.count(key);
}


std::string IniReader::makeKey(const std::string& section, const std::string& name)
{
    std::string key = section + "=" + name;
    // Convert to lower case to make section/name lookups case-insensitive
    std::transform(key.begin(), key.end(), key.begin(), ::tolower);
    return key;
}


int IniReader::valueHandler(void* user, const char* section, const char* name, const char* value)
{
    IniReader* reader = static_cast<IniReader*>(user);
    std::string key = makeKey(section, name);
    if (reader->_values[key].size() > 0)
        reader->_values[key] += "\n";
    reader->_values[key] += value;
    return 1;
}


std::vector<std::string> IniReader::getSectionLines(const std::string& section) const
{
	std::vector<std::string> lines;
	for (std::map<std::string, std::string>::const_iterator it=_values.begin(); it!=_values.end(); ++it) {
		if (it->first == section) {
			lines.push_back(it->second);
		}
	}		
	
	return lines;
}


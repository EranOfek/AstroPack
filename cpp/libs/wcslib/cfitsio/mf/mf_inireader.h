#ifndef mf_inireaderH
#define mf_inireaderH

#include <map>
#include <string>
#include <vector>
#include "mf_inifile.h"

// Read an INI file into easy-to-access name/value pairs. (Note that I've gone
// for simplicity here rather than speed, but it should be pretty decent.)
class IniReader : public IniFile {
public:
    // Constructor
    IniReader();

    // Open file
    virtual bool open(const std::string& filename);

    // Load from text
    virtual void load(const std::string& text);

    // Return the result of ini_parse(), i.e., 0 on success, line number of
    // first error on parse error, or -1 on file open error.
    virtual int parseError() const;

    // Get a string value from INI file, returning default_value if not found.
    virtual std::string get(const std::string& section, const std::string& name, const std::string& default_value = "") const;

    // Get a string value from INI file, returning default_value if not found,
    virtual std::string getString(const std::string& section, const std::string& name, const std::string& default_value = "") const;

    // Get an integer (long) value from INI file, returning default_value if
    // not found or not a valid integer (decimal "1234", "-1234", or hex "0x4d2").
    virtual long getInt(const std::string& section, const std::string& name, long default_value = 0) const;

    // Get a real (floating point double) value from INI file, returning
    // default_value if not found or not a valid floating point value according to strtod().
    virtual double getFloat(const std::string& section, const std::string& name, double default_value = 0) const;

    // Get a boolean value from INI file, returning default_value if not found or if
    // not a valid true/false value. Valid true values are "true", "yes", "on", "1",
    // and valid false values are "false", "no", "off", "0" (not case sensitive).
    virtual bool getBool(const std::string& section, const std::string& name, bool default_value = false) const;

    // Return true if the given section exists (section must contain at least one name=value pair).
    virtual bool hasSection(const std::string& section) const;

    // Return true if a value exists with the given section and field names.
    virtual bool hasValue(const std::string& section, const std::string& name) const;

	// Return all lines of specified section
	std::vector<std::string> getSectionLines(const std::string& section) const;
	
private:
    int _error;
    std::map<std::string, std::string> _values;
    static std::string makeKey(const std::string& section, const std::string& name);
    static int valueHandler(void* user, const char* section, const char* name, const char* value);
};

#endif


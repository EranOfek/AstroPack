#ifndef mf_inifileH
#define mf_inifileH

#include "mf_def.h"

// Read an INI file into easy-to-access name/value pairs. (Note that I've gone
// for simplicity here rather than speed, but it should be pretty decent.)
class IniFile {
public:
    // Constructor
    IniFile() {}

    // Destructor
    virtual ~IniFile() {}

    // Open file
    virtual bool open(const std::string& filename) { return false; }

    // Load from text
    virtual void load(const std::string& text) {}

    // Return the result of ini_parse(), i.e., 0 on success, line number of
    // first error on parse error, or -1 on file open error.
    virtual int parseError() const { return 0; }

    // Get a string value from INI file, returning default_value if not found.
    virtual std::string get(const std::string& section, const std::string& name, const std::string& defaultValue = "") const { return defaultValue; }

    // Get a string value from INI file, returning default_value if not found,
    virtual std::string getString(const std::string& section, const std::string& name, const std::string& defaultValue = "") const { return defaultValue; }

    // Get an integer (long) value from INI file, returning default_value if
    // not found or not a valid integer (decimal "1234", "-1234", or hex "0x4d2").
    virtual long getInt(const std::string& section, const std::string& name, long defaultValue = 0) const { return defaultValue; }

    // Get a real (floating point double) value from INI file, returning
    // default_value if not found or not a valid floating point value according to strtod().
    virtual double getFloat(const std::string& section, const std::string& name, double defaultValue = 0) const { return defaultValue; }

    // Get a boolean value from INI file, returning default_value if not found or if
    // not a valid true/false value. Valid true values are "true", "yes", "on", "1",
    // and valid false values are "false", "no", "off", "0" (not case sensitive).
    virtual bool getBool(const std::string& section, const std::string& name, bool defaultValue = false) const { return defaultValue; }

    // Return true if the given section exists (section must contain at least one name=value pair).
    virtual bool hasSection(const std::string& section) const { return false; }

    // Return true if a value exists with the given section and field names.
    virtual bool hasValue(const std::string& section, const std::string& name) const { return false; }

private:
};

#endif


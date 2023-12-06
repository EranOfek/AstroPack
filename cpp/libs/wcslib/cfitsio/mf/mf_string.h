#ifndef mf_stringH
#define mf_stringH

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <string>
#include <vector>

//===========================================================================
//							     String Utils
//===========================================================================

// Fast convert unsigned int to string, buf should be big enough
char* fastUIntToStr(uint64_t i, char* buf, size_t bufSize);

// Fast convert unsigned int to string, buf should be big enough
char* fastIntToStr(int64_t i, char* buf, size_t bufSize);

// Convert int to string
std::string intToStr(int64_t i);

// Convert unsigned int to string
std::string uintToStr(uint64_t i);

// Convert double to string
std::string floatToStr(double f);

// as sprintf, result buffer must not exceed 65536 bytes
std::string Avprintf(const char* format, va_list paramList);

// as sprintf
std::string Asprintf(const char* format, ...);

// Convert string to int
int64_t strToInt(const std::string& s, int64_t _default = 0);

// Insert readability commas into an integer without writing a custom locale facet
std::string intToCommaStr(int64_t i);

// Convert string to lower case
std::string toLower(const std::string& s);

// Convert string to upper case
std::string toUpper(const std::string& s);

// Compare string case insensitive
bool equalIC(const std::string& a, const std::string& b);

// Strip string from both ends
std::string strip(const std::string& _s, char c);

// Decode utf-8 - Currently not implemented, returns input as is!
std::string decodeUtf8(const std::string& _s);

// Convert container of strings to comma separated text
template<class StdContainer>
std::string listToCommaText(StdContainer& list)
{
	std::string str;
	int n = 0;
	for (auto it : list) {
		n++;
		if (n > 1)
			str += ",";

		str += it;
	}
	return str;
}


// Convert container of strings to JSON list
template<class StdContainer>
std::string listToJson(StdContainer& list, bool quote = true)
{
	std::string str;
	int n = 0;
	str += "[";
	for (auto it : list) {
		n++;
		if (n > 1)
			str += ", ";

		if (quote)
			str += "\"" + it + "\"";
		else
			str += it;
	}
	str += "]";
	return str;
}


// Convert map container of strings to JSON list
template<class StdContainer>
std::string mapToJson(StdContainer& list)
{
	std::string str;
	int n = 0;
	str += "{";
	for (auto it : list) {
		auto key = it.first;
		auto value = it.second;
		n++;
		if (n > 1)
			str += ", ";

		str += "\"" + key + "\":";
		str += "\"" + value + "\"";
	}
	str += "}";
	return str;
}

//===========================================================================
//							       MultiString
//===========================================================================
// Simple multiple strings, input string is splitted by separator
class CommaString {
public:
	// Constructor
	CommaString(const std::string& str = "", char sep = ',') { split(str, sep); }

	// Split input string to list of strings by given separator
	void split(const std::string& str, char sep);

	// Return list as JSON list 
	std::string toJson() { return listToJson(list); }

	// List of strings
	std::vector<std::string> list;
};

// Split string on delimiter character
std::vector<std::string>& split(const std::string& s, char delim, std::vector<std::string>& elems);

// Split string on delimiter character
std::vector<std::string> split(const std::string& s, char delim);

#endif

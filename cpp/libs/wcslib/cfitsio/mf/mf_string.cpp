#include "mf_def.h"
#include "mf_string.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <locale>

//
// Note that sprintf() and other int to string are slow and not good enough
// for high performance situations, there are better solutions.
// See:
// https://stackoverflow.com/questions/4351371/c-performance-challenge-integer-to-stdstring-conversion
// https://www.zverovich.net/2013/09/07/integer-to-string-conversion-in-cplusplus.html
// https://www.drdobbs.com/flexible-c-1-efficient-integer-to-string/184401596
// https://github.com/miloyip/itoa-benchmark
//


char* fastUIntToStr(uint64_t i, char* buf, size_t bufSize)
{
	char* psz = buf + bufSize - 1;// Set psz to last char
	*psz = 0;                     // Set terminating null
	do {
		unsigned lsd = i % 10;    // Get least significant digit
		i /= 10;                  // Prepare for next most significant digit
		--psz;                    // Move back
		*psz = '0' + lsd;		  // Place the digit
	} while (i != 0);
	return psz;
}


char* fastIntToStr(int64_t i, char* buf, size_t bufSize)
{
	char* psz = buf + bufSize - 1;// Set psz to last char
	*psz = 0;                     // Set terminating null
	bool neg = (i < 0);
	do {
		unsigned lsd = i % 10;    // Get least significant digit
		i /= 10;                  // Prepare for next most significant digit
		--psz;                    // Move back
		*psz = '0' + lsd;		  // Place the digit
	} while (i != 0);

	// Put sign
	if (neg) {
		psz--;
		*psz = '-';
	}
	return psz;
}


std::string intToStr(int64_t i)
{
    char s[128];
    return std::string(fastIntToStr(i, s, sizeof(s)));
}


std::string uintToStr(uint64_t i)
{
	char s[128];
	return std::string(fastUIntToStr(i, s, sizeof(s)));
}


std::string floatToStr(double f)
{
    char s[128];
    sprintf(s, "%0.4f", f);
    return std::string(s);
}


std::string Avprintf(const char* format, va_list paramList)
{
    char s[65536];
    vsprintf(s, format, paramList);
    return std::string(s);
}


std::string Asprintf(const char* format, ...)
{
    va_list paramList;
    va_start(paramList, format);
    std::string s = Avprintf(format, paramList);
    va_end(paramList);
    return s;
}


int64_t strToInt(const std::string& s, int64_t _default)
{
	int64_t value = strtoll(s.c_str(), NULL, 10);
	return value;
}


std::string intToCommaStr(int64_t input)
{
	std::vector<int16_t> tokens;
	std::stringstream ss;
	ss << std::setfill('0');
	int sign = 1;

	if (input < 0) {
		sign = -1;
		input *= -1;
	}

	while (input >= 1000) {
		tokens.push_back(input % 1000);
		input /= 1000;
	}

	ss << (input * sign);

	for (std::vector<int16_t>::const_reverse_iterator it = tokens.rbegin();
		it != tokens.rend(); it++) {
		ss << "," << std::setw(3) << *it;
	}

	return ss.str();
}


std::string toLower(const std::string& s)
{
	std::string data(s);
	std::transform(data.begin(), data.end(), data.begin(),
		[](unsigned char c) { return tolower(c); });
	return data;
}


std::string toUpper(const std::string& s)
{
	std::string data(s);
	std::transform(data.begin(), data.end(), data.begin(),
		[](unsigned char c) { return toupper(c); });
	return data;
}


bool equalIC(const std::string& a, const std::string& b)
{
	return std::equal(a.begin(), a.end(),
		b.begin(), b.end(),
		[](char a, char b) {
		return tolower(a) == tolower(b);
	});
}


std::string strip(const std::string& str, char c)
{
	size_t first = str.find_first_not_of(c);
	if (first == std::string::npos)
		return "";

	size_t last = str.find_last_not_of(c);
	return str.substr(first, (last - first + 1));	
}


// Currently not implemented, returns input as is!
std::string decodeUtf8(const std::string& _s)
{
	return _s;
}

//===========================================================================
void CommaString::split(const std::string& str, char sep)
{
	list.clear();
	if (str.size() == 0)
		return;

	size_t start = 0, stop, len = str.size();
	for (stop = 0; stop < len; stop++) {
		if (str[stop] == sep) {
			std::string s(str.data() + start, stop - start);
			list.push_back(s);
			start = stop + 1;
		}
	}

	if (stop != start) {
		std::string s(str.data() + start, stop - start);
		list.push_back(s);
	}
}


std::vector<std::string>& split(const std::string& s, char delim, std::vector<std::string>& elems)
{
	std::stringstream ss(s);
	std::string item;
	while (std::getline(ss, item, delim)) {
		elems.push_back(item);
	}
	return elems;
}


std::vector<std::string> split(const std::string& s, char delim)
{
	std::vector<std::string> elems;
	return split(s, delim, elems);
}


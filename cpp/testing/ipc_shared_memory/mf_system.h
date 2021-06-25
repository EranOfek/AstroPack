#ifndef mf_systemH
#define mf_systemH

class System {
public:

	// Return true for Window, false for Linux
	static bool isWin32() 
	{
#ifdef _WIN32
		return true;
#else
		return false;
#endif
	}

	// Return true for Window, false for Linux
	static bool isWindows()
	{
		return isWin32();
	}

	// Return true for Linux
	static bool isLinux()
	{
		return !isWin32();
	}
};
#endif

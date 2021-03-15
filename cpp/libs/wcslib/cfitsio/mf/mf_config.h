#ifndef mf_configH
#define mf_configH

#include <string>
#include <vector>
#include "mf_inireader.h"
#include "mf_component.h"
#include "mf_system.h"


// Define to the full name of this package
#define PACKAGE_NAME "MFLOW"

// Define to the full name and version of this package
#define PACKAGE_STRING "MFLOW 1.0.1"

// Define to the one symbol short name of this package
#define PACKAGE_TARNAME "mflow"

// Define to the home page for this package
#define PACKAGE_URL ""

// Define to the version of this package
#define PACKAGE_VERSION "1.0.1"


// Simple parser for argc/argv
class ArgsParser {
public:
    // Constructor
    ArgsParser(int argc = 0, char* argv[] = NULL) 
    {
        init(argc, argv);
    }

    // Initialize
    void init(int _argc, char* _argv[]);

    // Check if specified argument is in command line, return default if not found
    bool haveArg(const char* arg, bool _deafult = false);

    // Get command line value for specified argument, return default if not found
    std::string getArg(const char* arg, const std::string& _default = "");

    // Get command line value for specified argument, return default if not found or invalid
    int64_t getArgInt(const char* arg, int64_t _default = 0);

    int argc;
    char** argv;
};


// System Configuration
class SystemConfig : public Component {
public:

    // Constructor
    SystemConfig();

    // Get application title
    std::string getTitle();

    // Load config from INI file
    bool load(const std::string& fname);

    // Parse command line, overide previous settings loaded from INI file
    bool parse(int _argc, char* _argv[]);

    // Display help and command line options
    void usage();

    // Display help line
    void print(const char* s)
    {
        printf("%s", s);
    }

    // Data
    IniReader       ini;                    //
    ArgsParser      args;                   //

    // Path
	std::string     logPath;                // Log files folder
    std::string     pcapFilename;           //
	std::string     pcapPath;               //
	std::string     tempPath;               // Temporary files folder
    std::string     outputPath;             //
    std::string     debugOptions;           //
    size_t          maxPacketCount;         // Maximum number of packets
    size_t          flowBufSize;            // Flow buffer size
    int64_t         maxFileSize;            //
    int64_t         maxGap;                 //
    int32_t         flowTimeoutSeconds;     //
    size_t          maxSavedFlows;          // Max saved flows 
    bool            debugMode;              // True: debug mode
    bool            saveOnlyMatched;        // True: Save to file only streams that match signature processor
    bool            jsonCompatibility;      // True: Json report compatible with tcpflow
    bool            jsonScannerMode;        // True:
    bool            runUnitTests;           // True: run unit-tests on loading
    bool            flowLog;                // True: write detailed log file per flow  
    bool            noLog;                  // True: Log files will be removed after running
    bool            runScanner;             //
    bool            writeIndexFile;         //
    bool            hash;                   //
    bool            no_ratelimit;           //
    bool            udp;                    //
    bool            capwap;                 //
    bool            quietMode;              // Supress printout
    bool            validateHeaderCheck;    // True to validate ip/tcp/udp headers checksum
};

// Configuration is globally accessible
extern SystemConfig *systemConfig;

// Check if specified debug option is enabled
bool getDebugOption(const char* opt);

#endif

#include "mf_config.h"
#include "mf_file.h"
#include "mf_string.h"
#include "mf_logfile.h"


// Global config
SystemConfig* systemConfig = NULL;


void ArgsParser::init(int _argc, char* _argv[])
{
    argc = _argc;
    argv = _argv;
}


bool ArgsParser::haveArg(const char* arg, bool _deafult)
{
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], arg)) {
            return true;
        }
    }
    return _deafult;
}


std::string ArgsParser::getArg(const char* arg, const std::string& _default)
{
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], arg)) {
            return std::string(argv[i + 1]);
        }
    }
    return _default;
}


int64_t ArgsParser::getArgInt(const char* arg, int64_t _default)
{
    std::string str = getArg(arg, intToStr(_default));
    int64_t value = strToInt(str);
    if ((value == 0) && (_default != 0))
        value = _default;

    return value;
}

//===========================================================================

SystemConfig::SystemConfig()
{
    maxPacketCount          = 0;
    flowBufSize             = 65536;
    maxFileSize             = -1;
    maxGap                  = 1024 * 1024 * 32;
    maxSavedFlows           = 100;
    flowTimeoutSeconds      = 0;
    debugMode               = true;
    saveOnlyMatched         = false;
    jsonCompatibility       = false;
    jsonScannerMode         = true;
    runUnitTests            = false;
    flowLog                 = true;
    runScanner              = false;
    writeIndexFile          = false;
    hash                    = false;
    no_ratelimit            = false;
    udp                     = false;
    capwap                  = false;
    quietMode               = false;
    validateHeaderCheck     = false;
}


std::string SystemConfig::getTitle()
{
    std::string title = "mflow v1.00";
    title += System::isWindows() ? ", running on Windows" : ", running on Linux";
    if (Thread::isDebugBuild())
        title += " - DEBUG build, performance is about x10 slower than release version";
    else
        title += " - RELEASE build";
    return title;
}


bool SystemConfig::load(const std::string& fname)
{
    ini.open(fname);

    // Path
    std::string section = System::isWin32() ? "PathWin" : "Path";
#ifdef _WIN32
    logPath     = ini.getString(section, "LogPath",     "C:\\_mf\\log");
    pcapPath    = ini.getString(section, "PcapPath",    "C:\\_mf\\pcap");
    tempPath    = ini.getString(section, "TempPath",    "C:\\_mf\\temp");
#else
    logPath  = ini.getString(section, "LogPath", ".");
    pcapPath = ini.getString(section, "PcapPath", "");
    tempPath = ini.getString(section, "TempPath", "/tmp");
#endif

    logPath = "";

    // Size and limits
    section = "Options";
    maxPacketCount      = (size_t) ini.getInt(section, "MaxPacketCount", 0);
    flowBufSize         = (size_t)ini.getInt(section, "FlowBufSize", 8192);
    maxFileSize         = (int64_t)ini.getInt(section, "MaxFileSize", -1);
    maxGap              = (int64_t)ini.getInt(section, "MaxGap", 1024 * 1024 * 32);
    maxSavedFlows       = (size_t) ini.getInt(section, "MaxSavedFlows", 100);
    flowTimeoutSeconds  = (int32_t)ini.getInt(section, "FlowTimeoutSeconds", 0);
    
    // Options
    debugMode           = ini.getInt(section, "DebugMode", 1);
    saveOnlyMatched     = ini.getInt(section, "SaveOnlySigned", 0);
    jsonCompatibility   = ini.getInt(section, "JsonCompatibility", 0);
    jsonScannerMode     = ini.getInt(section, "JsonScannerMode", 0);
    runUnitTests        = ini.getInt(section, "RunUnitTests", 0);
    flowLog             = ini.getInt(section, "FlowLog", 1); 
    quietMode           = ini.getInt(section, "QuietMode", 0);
    
    debugOptions        = ini.getString(section, "DebugOptions", "");
    debugOptions = "," + debugOptions + ",";

    return true;
}


bool SystemConfig::parse(int _argc, char* _argv[])
{
    // Parse command line
    args.init(_argc, _argv);

    // Input file name
    if (args.haveArg("-r")) {
        pcapFilename = args.getArg("-r");
        pcapPath = "";
    }

    // Input pcap folder
    if (args.haveArg("-d")) {
        pcapPath = args.getArg("-d");
        pcapFilename = "";
    }

    // Output base folder
    if (args.haveArg("-o")) {
        outputPath = args.getArg("-o");
    }

    // Max file size
    if (args.haveArg("-b")) {
        maxFileSize = args.getArgInt("-b", maxFileSize);
    }

    // Combine options with INI settings
    runUnitTests        = args.haveArg("-utest");
    jsonCompatibility   = args.haveArg("-tcpflow");
    jsonScannerMode     = args.haveArg("-tcpscanner");
    flowLog             = args.haveArg("-flowlog");
    runScanner          = args.haveArg("-P") || jsonScannerMode;
    writeIndexFile      = args.haveArg("-I");
    hash                = args.haveArg("-hash") || args.haveArg("-1");
    no_ratelimit        = args.haveArg("-no_ratelimit");
    udp                 = args.haveArg("-udp");
    capwap              = args.haveArg("-capwap");
    quietMode           = args.haveArg("-q");
    validateHeaderCheck = !args.haveArg("-nochecksum");
    noLog               = args.haveArg("-nolog");

    //
    globalQuietMode = quietMode;

    // Force scanner when processing non-tcp packets
    if (udp) {
        runScanner = true;
        jsonScannerMode = true;
    }

    // Disable log
    if (args.haveArg("-nolog"))
        flowLog = false;
    else
        flowLog = true;

    // Debugger (required only on Linux)
    extern bool linuxIsDebuggerPresent;
    if (args.haveArg("-debugger")) {
        linuxIsDebuggerPresent = true;
    }

    return true;
}


void SystemConfig::usage()
{
    print(getTitle().c_str());
    print("\n");
    print("  -h             : print this usage help and exit\n");
    print("  -r file        : read packets from pcap file\n");
    print("  -d path        : read packets from all pcap files in folder\n");
    print("  -o path        : set output folder\n");
    print("  -b max_bytes   : set maximum output file size per flow\n");
    print("  -utest         : run unit-tests and exit\n");
    print("  -tcpflow       : write report.json in tcpflow compatible format\n");
    print("  -tcpscanner    : keep only flows that match signatures\n");
    print("  -P             : run libscanner plugin\n");
    print("  -no_ratelimit  : supress random filtering in libscanner\n");
    print("  -I             : write for each flow another file *.findx to provide byte-indexed timestamps\n");
    print("  -hash          : calculate CRC32 for each output file and add to report\n");
    print("  -udp           : enable processing of udp packets with libscanner filtering\n");
    print("  -capwap        : enable processing of capwap packets\n");
    print("  -flowlog       : write per-flow log file in log/ subfolder\n");
    print("  -nolog         : supress log files creation\n");
    print("  -checksum      : validate ip/tcp/udp headers checkum, invalid packets are ignored\n");
    print("  -debugger      : enable isDebuggerPresent() for debugging\n");
    print("\n");
}


#ifdef never
// Currently unised, should be replaced by better implementation
bool getDebugOption(const char* opt)
{
    bool result = false;

    if (systemConfig && systemConfig->debugMode) {
        char s[256];
        s[0] = ',';
        s[1] = 0;
        strcat(s, opt);
        strcat(s, ",");
        if (strstr(systemConfig->debugOptions.c_str(), s) != NULL) {
            result = true;
        }
    }

    return result;
}
#endif

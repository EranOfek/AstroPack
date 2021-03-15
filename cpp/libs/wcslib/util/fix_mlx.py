# Automatically fix MATLAB .mlx files for wcslib

import os, glob, time, json, argparse
from datetime import datetime


#--------------------------- Utility Functions ---------------------------

# Log lines are cached in this list, to allow 'undo' of logging if we
# decide later that it should be ommited.
log_lines = None

# Log message to file
logfile = None
def log(msg, dt = False):
    global logfile, log_lines
    if msg == '': dt = False
    if dt: msg = datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
    print(msg)
    if log_lines != None:
        log_lines.append(msg)
    elif logfile:
        logfile.write(msg)
        logfile.write("\n")
        logfile.flush()


def clear_log_lines():
    global log_lines
    log_lines = None


def write_log_lines():
    global log_lines
    lines = log_lines
    log_lines = None
    if lines:
        for line in lines:
            log(line)


# Run command line with timing
def run(cmdline):
    log('run: ' + cmdline)
    try:
        start = time.time()
        os.system(cmdline)
        end = time.time()
        elapsed = end - start
        log(cmdline + ' - ' + ('%.3f' % elapsed) + " seconds")
        return elapsed
    except:
        log('run: FAILED: ' + cmdline)
        return 0


# Creates folders
def mkdirs(path):
    if not os.path.exists(path):
        #log('mkdirs: ' + path)
        os.makedirs(path)


# Replace text between two tag lines, keeping the tags
    '''
    %% C++ function |wcsSkyToPix| with MATLAB name |clib.wcslibPkg.wcsSkyToPix|
    % C++ Signature: void wcsSkyToPix(double * x,double * y,size_t len)
    %wcsSkyToPixDefinition = addFunction(libDef, ...
    %    "void wcsSkyToPix(double * x,double * y,size_t len)", ...
    %    "MATLABName", "clib.wcslibPkg.wcsSkyToPix", ...
    %    "Description", "clib.wcslibPkg.wcsSkyToPix    Representation of C++ function wcsSkyToPix."); % Modify help description values as needed.
    %defineArgument(wcsSkyToPixDefinition, "x", "clib.array.wcslibPkg.Double", "input", <SHAPE>); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
    %defineArgument(wcsSkyToPixDefinition, "y", "clib.array.wcslibPkg.Double", "input", <SHAPE>); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
    %defineArgument(wcsSkyToPixDefinition, "len", "uint64");
    %validate(wcsSkyToPixDefinition);   
    '''

def fix_func(lines, line_num):

    func_def = ''

    for i in range(line_num, line_num+100):
        line = lines[i]
        if line.startswith('%'):

            # Remove '%'
            line = line[1:]

            add_function = line.find('addFunction(') > -1
            define_argument = line.startswith('defineArgument(')
            define_output = line.startswith('defineOutput(')
            validate = line.startswith('validate(')

            line_handled = False

            if add_function:
                func_def = line
                line_handled = True

            if validate:
                line_handled = True

            if define_argument or define_output:

                if line.find('%') > -1:
                    code, comment = line.split('%')
                else:
                    code, comment = line, ''

                # String
                # %defineArgument(wcsReadFitsDefinition, "fname", <MLTYPE>, "input", <SHAPE>); % '<MLTYPE>' can be clib.array.wcslibPkg.Char,int8,string, or char
                if code.find('<MLTYPE>') > -1 and code.find('<SHAPE>') > -1 and comment.find('string') > -1:
                    code = code.replace('<MLTYPE>', '"string"')
                    code = code.replace('<SHAPE>', '"nullTerminated"')
                    line = code + ' % @Fixed - ' + comment

                #
                elif func_def.find('GetProp_') > -1:
                    if define_argument:
                        if code.find('"input"'):
                            code = code.replace("input", "output")

                # Matrix
                # %defineArgument(wcsSkyToPixDefinition, "x", "clib.array.wcslibPkg.Double", "input", <SHAPE>); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
                if code.find('<SHAPE>') > -1 and comment.find('array') > -1:

                    # Int
                    dtype = 'clib.array.wcslibPkg.Int'
                    atype = 'int'
                    code = code.replace(dtype, atype)
                    code = code.replace('<SHAPE>', '"len"')

                    # double
                    dtype = 'clib.array.wcslibPkg.Double'
                    atype = 'double'
                    code = code.replace(dtype, atype)
                    code = code.replace('<SHAPE>', '"len"')

                    line = code + ' % @Fixed - ' + comment

            # Replace the line
            lines[i] = line

            # Reached last line of current function
            if line.startswith('validate('):
                break

            if not line_handled:
                log('Line not handled: ' + line)

    return lines



# Process process_files_*.log file generated by process_files.py
def process_file(filename):

    path, fname = os.path.split(filename)
    log('\n## ' + path)
    log(filename + '\n')

    # Load log file
    lines = []
    with open(filename) as f:
        lines = f.read().splitlines()

    # Look for function defintion blocks
    '''
    %% C++ function |wcsSkyToPix| with MATLAB name |clib.wcslibPkg.wcsSkyToPix|
    % C++ Signature: void wcsSkyToPix(double * x,double * y,size_t len)
    %wcsSkyToPixDefinition = addFunction(libDef, ...
    %    "void wcsSkyToPix(double * x,double * y,size_t len)", ...
    %    "MATLABName", "clib.wcslibPkg.wcsSkyToPix", ...
    %    "Description", "clib.wcslibPkg.wcsSkyToPix    Representation of C++ function wcsSkyToPix."); % Modify help description values as needed.
    %defineArgument(wcsSkyToPixDefinition, "x", "clib.array.wcslibPkg.Double", "input", <SHAPE>); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
    %defineArgument(wcsSkyToPixDefinition, "y", "clib.array.wcslibPkg.Double", "input", <SHAPE>); % '<MLTYPE>' can be clib.array.wcslibPkg.Double, or double
    %defineArgument(wcsSkyToPixDefinition, "len", "uint64");
    %validate(wcsSkyToPixDefinition);   
    '''

    # Prepare list of functions to fix
    fix_list = []
    for i in range(0, len(lines)-2):
        if lines[i].startswith('%% C++ function') and lines[i+1].startswith('% C++ Signature:') \
            and lines[i+2].startswith('%') and lines[i+2].find(' = addFunction(') > -1:
            fix_list.append(i+2)
            log(lines[i+1])

    # Fix all functions in the list
    for line_num in fix_list:
        lines = fix_func(lines, line_num)

    write_file = True

    # Replace file contents
    if write_file:
        matlab_file = open(filename, 'wt')

        for line in lines:
            matlab_file.write('%s\n' % line)

        matlab_file.flush()

#------------------------------- Main -------------------------------

# parser.add_option("-f", "--file", action="store", type="string", dest="filename")
def main():
    global logfile, args

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    #parser.add_argument('-d',           dest='dir',         default=None,                                   help='pcap folder')
    #parser.add_argument('-s',           dest='subdirs',     action='store_true',    default=True,   help='Process pcap files in subfolders')
    #args = parser.parse_args()

    #


    # Create log file based on current date/time
    dir = './' #args.dir
    dtstr = datetime.now().strftime('%Y_%m_%d__%H_%M_%S')
    outdir = '.' #os.path.join(dir, '_check_' + dtstr)
    #mkdirs(outdir)
    logfile = open(os.path.join(outdir, 'util.log'), 'a')

    log('started: ' + dtstr + '  ')
    log('')

    process_file('../wcs_matlab/definewcslibPkg.m')

    log('\n\nDone\n')


if __name__ == '__main__':
    main()


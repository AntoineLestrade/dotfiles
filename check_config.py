import sys, getopt
import os
import subprocess
import re
import xml.etree.ElementTree as ET

class bcolors:
    HEADER  = '\033[95m'
    OKBLUE  = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[1;33m'
    FAIL    = '\033[1;41m'
    ENDC    = '\033[0;m'

    def disable(self):
        self.HEADER = ''
        self.OKBLUE  = ''
        self.OKGREEN = ''
        self.WARNING = ''
        self.FAIL    = ''
        self.ENDC    = ''

def get_errors (config_files):
    result = 0;
    for filename in config_files:
        with open(filename, encoding='utf-8') as f:
            s = f.read()
            if re.search('<connectionStrings>', s):
                try:
                    root = ET.fromstring(s)
                    for cs in root.find("connectionStrings").iter("add"):
                        match = re.match(r'.*data source=(.*?);.*initial catalog=(.*?);.*', cs.get('connectionString'))
                        if not match:
                            print((bcolors.WARNING + "ERROR {0}" + bcolors.ENDC).format(cs.get('connectionString')))
                            continue
                        err = False
                        if not(match.group(1) == '.\sqlexpress' or match.group(1) == 'dabel69\sqlexpress' or match.group(1) == 'dabel69.corp.altegroup.dir\sqlexpress'):
                            err = True
                        if re.match(".*^_...$", match.group(2)):
                            err = True
                        c = bcolors.OKGREEN
                        if err:
                            c = bcolors.FAIL
                            result += 1
                        print(c + "File: {3}; Name: {2}; Server: {0} DB: {1}".format(match.group(1), match.group(2), cs.get('name'), filename) + bcolors.ENDC)
                except ET.ParseError:
                    print(bcolors.WARNING + ("File %(filename)s is not a valid xml file" %{ 'filename': filename }) + bcolors.ENDC)
    return result

def main_git(argv):
    root_folder = argv[0]
    config_files = []

    with subprocess.Popen("git ls-files --full-name", cwd = root_folder, shell = True, stdout = subprocess.PIPE, stderr = subprocess.PIPE, bufsize = 1) as p:
        for line in p.stdout:
            line_str = line.decode("utf-8")
            if re.match(r'.*\.config$', line_str):
                config_files.append(root_folder + '\\' + line_str.replace('\n', ''))
    #(out, error) = pr.communicate()
    #for line in pr.stdout:
    #    line_string = line.rstrip()
    #    print (line_string)
    #    if re.match(r'.*\.config$', line_string):
    #        config_files.append(line_string);
    #print(str(error))

    return get_errors(config_files)

def main(argv):
    root_folder = argv[0]
    config_files = []

    for root, directories, filenames in os.walk(root_folder):
        for filename in filenames:
            if re.match(r'.*\.config$', filename):
                config_files.append(os.path.join(root, filename))

    return get_errors(config_files)

if __name__ == '__main__':
    if sys.argv[1] == 'git':
        print(main_git(sys.argv[2:]))
    else:
        print(main(sys.argv[1:]))

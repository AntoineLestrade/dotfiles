import sys
import os
import subprocess

root = 'W:'
def back_module_path(proj_name):
    return root + '\\' + proj_name + '\\back\main'
def project_path(proj_name):
    return root + "\\" + proj_name

mappings = {
    'carpooling': back_module_path('m3-carpooling'),
    'libkami': project_path('m3-libkami'),
    'release': os.path.join(root, 'm3-release', 'back'),
}

idea_path = '"C:/Program Files/JetBrains/IntelliJ IDEA Community Edition 2019.1.3/bin/idea64.exe"'

def main(argv):
    path = ''
    if argv in mappings:
        path = mappings[argv]
    elif os.path.isdir(back_module_path('m3-' + argv)):
        path = back_module_path('m3-' + argv)
    elif os.path.isdir(project_path('m3-' + argv)):
        path = project_path('m3-' + argv)
    else:
        print ("Unkown project " + argv)
        return
    #os.system(idea_path + " " + path)
    subprocess.Popen(idea_path + " " + path)
    return

if __name__ == '__main__':
    main(sys.argv[1])

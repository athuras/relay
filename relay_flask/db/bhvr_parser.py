import numpy as np

def parse(bhvr_mtx):
    '''
    Given a matrix of the current behaviour, hack together a name for it
    '''

    allow_paths = (np.array(bhvr_mtx) > 0)*1

    name = ""
    if allow_paths[0,2] == 1:
        name += "N"
    if allow_paths[2,0] == 1:
        name += "S"
    if allow_paths[1,3] == 1:
        name += "E"
    if allow_paths[3,1] == 1:
        name += "W"

    if name=="":
        name = "AG"
    else:
        name+="T"

    return name
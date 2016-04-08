from __future__ import print_function
import os
import argparse
import numpy as np
import flopy


__date__ = 'April 6, 2015'
__version__ = "1.0"
__maintainer__ = "Christian D. Langevin"
__email__ = "langevin@usgs.gov"
__status__ = "Production"
__description__ = '''
This program reads a MODFLOW name file and then creates the files needed
to run the gridrep program.
'''


def parser():
    """
    Construct the parser and return argument values
    """
    description = __description__
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('namfile', help='Name of MODFLOW name file')
    parser.add_argument('--zscale',
                        help='''Scale elevation factor.''',
                        default=1.0, type=float)
    args = parser.parse_args()
    return args


def mkver(m):
    nvert = (m.dis.nrow + 1) * (m.dis.ncol + 1)
    ver = np.empty((nvert, 2), dtype=np.float)
    ver [:, 0] = m.dis.sr.xgrid.flatten()
    ver [:, 1] = m.dis.sr.ygrid.flatten()
    f = open('example.ver', 'w')
    f.write('{}\n'.format(nvert))
    for i, (x, y) in enumerate(ver):
        f.write(' {} {} {}\n'.format(i + 1, x, y))
    f.close()
    return ver


def mknod(m):
    nodes = m.dis.nrow * m.dis.ncol
    nod = np.empty((nodes, 3), dtype=np.float)
    nod[:, 0] = m.dis.sr.xcentergrid.flatten()
    nod[:, 1] = m.dis.sr.ycentergrid.flatten()
    nod[:, 2] = m.dis.zcentroids.flatten()
    f = open('example.nod', 'w')
    f.write('{}\n'.format(nodes))
    for i, (x, y, z) in enumerate(nod):
        f.write(' {} {} {} {}\n'.format(i + 1, x, y, z))
    f.close()
    return nod


def mkelv(m, zscale=1.0):
    elv = np.hstack((np.unique(m.dis.top.array),
                      np.unique(m.dis.botm.array)))
    f = open('example.elv', 'w')
    nrec = elv.shape[0]
    f.write('{}\n'.format(nrec))
    for i in range(nrec):
        f.write(' {} {}\n'.format(i + 1, elv[i] * zscale))
    f.close()
    return elv


def mkcel(m, elv):
    f = open('example.cel', 'w')
    nodes = m.nlay * m.nrow * m.ncol
    f.write('{} {}\n'.format(nodes, 4))
    n = 0
    top = m.dis.top.array
    bot = m.dis.botm.array
    for k in range(m.dis.nlay):
        for i in range(m.dis.nrow):
            for j in range(m.dis.ncol):
                # for each cell: index, node, top_elev, bot_elev, vertices (ccw)
                if k == 0:
                    tp = top[i, j]
                else:
                    tp = bot[k - 1, i, j]
                bt = bot[k, i, j]
                itp = np.where(elv == tp)[0][0]
                ibt = np.where(elv == bt)[0][0]
                s = ' {} {} {} {} '.format(n + 1, n + 1, itp + 1, ibt + 1)
                # ur
                icell2d = i * (m.dis.ncol + 1) + j
                s += '{} '.format(icell2d + 2)
                # ul
                s += '{} '.format(icell2d + 1)
                # ll
                icell2d = (i + 1) * (m.dis.ncol + 1) + j
                s += '{} '.format(icell2d + 1)
                # lr
                s += '{}\n'.format(icell2d + 2)
                f.write(s)
                n += 1
    f.close()


def mksee(m):
    f = open('example.see', 'w')
    header = """AUTO                       # viewpoint ("AUTO" or XVIEW YVIEW ZVIEW OVECX OVECY OVECZ OANGL)
0.03                       # node radius
T   0.  0.  0.   0.85      # T/F R G B Tr for drawing default (unhighlighted) cells
T   0.  0.  0.   0.0       # T/F R G B Tr drawing highlighted cells
T   0.  0.  0.   0.85      # T/F R G B Tr for drawing default (unhighlighted) nodes
T   0.  0.  0.   0.0       # T/F R G B Tr drawing highlighted nodes
F   0.  0.9 0.   0.6       # T/F R G B Tr for drawing horizontal-connection interfaces (highlighted cells only)
F   0.  0.7 0.   0.0       # T/F R G B Tr for drawing horizontal-connection lengths (highlighted cells only)
F   0.  0.  1.   0.6       # T/F R G B Tr for drawing vertical-connection interfaces (highlighted cells only)
F   0.  0.  1.   0.0       # T/F R G B Tr for drawing vertical-connection lengths (highlighted cells only)
T                          # T/F for whether BOTH cells need to be highlighted to draw connection
SOME
"""
    f.write(header)
    nodes = m.nlay * m.nrow * m.ncol
    bas = m.get_package('BAS6')
    ibound = None
    if bas is not None:
        ibound = bas.ibound.array.flatten()
    for n in range(nodes):
        idraw = 'T'
        ibd = 1
        # do not draw the cell if ibound is available and is 0
        if ibound is not None:
            ibd = ibound[n]
        if ibd == 0:
            idraw = 'F'
        f.write('{} {}\n'.format(n + 1, idraw))
    f.close()
    return


def main(namfile, zscale):

    # check namefile exists
    print(os.getcwd())
    if not os.path.isfile(namfile):
        raise Exception('Namefile file not found: {}'.format(namfile))

    # load model
    m = flopy.modflow.Modflow.load(namfile, check=False)
    ver = mkver(m)
    nod = mknod(m)
    elv = mkelv(m, zscale)
    mkcel(m, elv)
    mksee(m)
    return


if __name__ == "__main__":
    args = parser()
    main(args.namfile, args.zscale)

import numpy as np


def h2_accounting_before_hmm(pyfiler):

    current_integer_year_python = pyfiler.ncntrl.curiyr - 1


    tmp_qelhm = np.copy(pyfiler.qblk.qelhm[:, current_integer_year_python])
    tmp_qnghm = np.copy(pyfiler.qblk.qnghm[:, current_integer_year_python])

    th2con = {'qelhm' : tmp_qelhm,
              'qnghm' : tmp_qnghm}
    
    return th2con

def h2_accounting_after_hmm(pyfiler, th2con):

    IND_DIV = [[0, 2 ,4 ,7, 10],    # Census Division start for each Region and US
               [1, 3, 6, 8, 10]]    # Census Division end   for each Region and US
    
    current_integer_year_python = pyfiler.ncntrl.curiyr - 1

    pyfiler.qblk.qelin[:, current_integer_year_python] = pyfiler.qblk.qelin[:, current_integer_year_python] - th2con['qelhm']       # electricity
    pyfiler.qblk.qngin[:, current_integer_year_python] = pyfiler.qblk.qngin[:, current_integer_year_python] - th2con['qnghm']       # nat gas h&p

    pyfiler.qblk.qelin[:, current_integer_year_python] = pyfiler.qblk.qelin[:, current_integer_year_python] + pyfiler.qblk.qelhm[:, current_integer_year_python]    # electricity
    pyfiler.qblk.qngin[:, current_integer_year_python] = pyfiler.qblk.qngin[:, current_integer_year_python] + pyfiler.qblk.qnghm[:, current_integer_year_python]    # nat gas h&p

    pyfiler.bifurc.qe2ngin[10, current_integer_year_python, 0] = pyfiler.bifurc.qe2ngin[10, current_integer_year_python, 0] - th2con['qnghm'][10]   # nat gas h&p

    pyfiler.bifurc.qe2ngin[10, current_integer_year_python, 0] = pyfiler.bifurc.qe2ngin[10, current_integer_year_python, 0] + pyfiler.qblk.qnghm[10, current_integer_year_python]   # natural gas (h&p and feedstock)

    for ir in range(5):
        pyfiler.indrep.h2con[0, ir, current_integer_year_python] = sum(pyfiler.qblk.qelhm[IND_DIV[0][ir]:IND_DIV[1][ir] + 1, current_integer_year_python])
        pyfiler.indrep.h2con[1, ir, current_integer_year_python] = sum(pyfiler.qblk.qnghm[IND_DIV[0][ir]:IND_DIV[1][ir] + 1, current_integer_year_python])
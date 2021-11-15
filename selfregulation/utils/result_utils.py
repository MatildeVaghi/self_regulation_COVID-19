from glob import glob
import os
from os import path
from dimensional_structure.results_mv import Results
from selfregulation.utils.utils import get_info
import rpy2.robjects
import pandas as pd


def load_results(datafile, name=None, results_dir=None, verbose = True):
    if results_dir is None:
        results_dir = get_info('results_directory')
    results = {}
    result_files = glob(os.path.join(results_dir, 'dimensional_structure/%s/Output/*results.pkl' % (datafile)))
    if name is not None:
        result_files = [i for i in result_files if name in i]
    for filey in result_files:
        name = os.path.basename(filey).split('_')[0]
        results[name] = Results(saved_obj_file=filey)
    if verbose:
        print('Getting result file: %s...:\n'  % (result_files))
    return results


def get_attr(fa, attr):
        try:
            index = list(fa.names).index(attr)
            val = list(fa.items())[index][1]
            if len(val) == 1:
                val = val[0]
            if type(val)==rpy2.robjects.vectors.Matrix:
                val = np.matrix(val)
            return val
        except ValueError:
            print('Did not pass a valid attribute')


def get_fit_indexes (EFA, c=None):
    if c is None:
        c = EFA.get_c()

    print('Number of factors:', c)
    print('EFA on N =', get_attr(EFA.results['factor_tree_Rout_%s' % 'oblimin'][c],'n.obs'), 'subjects')
    print('Fit indexes:')
    print('RMSEA index ', get_attr(EFA.results['factor_tree_Rout_%s' % 'oblimin'][c],
                        'RMSEA'))

    print('BIC', get_attr(EFA.results['factor_tree_Rout_%s' % 'oblimin'][c],
                        'BIC'))

    print('TLI',get_attr(EFA.results['factor_tree_Rout_%s' % 'oblimin'][c],
                        'TLI'))

    print('rms, root mean square of the residuals (RMSA)',get_attr(EFA.results['factor_tree_Rout_%s' % 'oblimin'][c],
                        'rms'))

    print('crms, df corrected root mean square of the residuals',get_attr(EFA.results['factor_tree_Rout_%s' % 'oblimin'][c],
                        'crms'))
    print('*'*79)


def get_EFA (results):
    EFA =results.EFA
    return EFA

def get_HCA (results):
    HCA=results.HCA
    return HCA

def get_fa_oblimin(folder_fa_results, name):
    results = load_results(datafile = folder_fa_results, name = name, verbose = False)

    if name == 'demo':
        results = load_results(datafile = 'Covid_train', verbose = False)
        EFA = results['task'].DA
    else:
        EFA= results[name].EFA
    #EFA =get_EFA(results)
    c = EFA.get_c()
    fa = EFA.results['factor_tree_Rout_%s' % 'oblimin'][c]
    print('Getting factor solution:')
    print('EFA on', get_attr(EFA.results['factor_tree_Rout_%s' % 'oblimin'][c],'n.obs'), 'subjects, ', '# of factors:', c )
    print('')
    return fa


def get_loadings(datafile, subset = None, rotate = 'oblimin', verbose = False):
    results = load_results(datafile= datafile, name = subset, verbose = verbose)
    EFA  = get_EFA(results[subset])
    c    = EFA.get_c()
    loadings = EFA.get_loading(c, rotate = rotate)
    return loadings

def get_predicted_score_data(folder, file):
    datadir=path.join(get_info('base_directory'),'Results/dimensional_structure/', folder)
    datafile=os.path.join(datadir,file)
    data=pd.read_csv(datafile,index_col=0)
    return data.sort_index()


def get_short_names(df):
    df = df.rename(columns={'Speeded IP': 'SpeededIP',
                            'Strategic IP':'StrategicIP',
                            'Caution':'Caution',
                            'Perc /Resp':'PercResp',
                            'Goal-directed/Mindfulness': 'GDM',
                            'Sensation Seeking': 'SS',
                            'Emotional Control':'EMC',
                            'Reward Sensitivity':'RS',
                            'Risk Perception':'RP',
                            'Ethical Risk Taking':'ERT',
                            'Social Risk Taking':'SRT',
                            'Agreeableness':'AGR',
                            'Anxious-Depressed': 'AD',
                            'Compulsive Intrusive Thoughts':'CIT',
                            'Social Withdrawal':'SW'})
    return(df)

def get_var_of_interest(df, var):
    df_var_of_interest = df[[var,'time']]
    return df_var_of_interest

def get_paired_diff(df, var):
    paired_diff = df[df['time'] =='covid'][var] - df[df['time'] =='master'][var]
    paired_diff = pd.DataFrame(paired_diff)
    return paired_diff

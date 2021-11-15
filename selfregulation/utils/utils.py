"""
some util functions
"""
from glob import glob
import os
import pandas,numpy
import re
from sklearn.metrics import confusion_matrix
import pkg_resources
from collections import OrderedDict
from os import path
import json

# Regex filtering helper functions
def not_regex(txt):
        return '^((?!%s).)*$' % txt

def filter_behav_data(data, filter_regex):
    """ filters dataframe using regex
    Args:
        filter_regex: regex expression to filter data columns on. Can also supply
            "survey(s)" or "task(s)" to return measures associated with those
    """
    # main variables used to reduce task data down to its "main" variables
    main_vars = ['adaptive_n_back.mean_load',
             'angling_risk_task_always_sunny\..*_adjusted_clicks',
             'angling_risk_task_always_sunny\.release_adjusted_clicks',
             'attention_network_task\.alerting',
             'attention_network_task\.orienting',
             'attention_network_task\.conflict',
             'bickel_titrator\.hyp_discount_rate_medium',
             'choice_reaction_time',
             'cognitive_reflection_survey\.correct_proportion',
             'columbia_card_task_cold',
             'columbia_card_task_hot'
             'dietary_decision',
             'digit_span',
             'directed_forgetting\.proactive_interference_hddm_drift',
             'discount_titrate\.percent_patient',
             'dot_pattern_expectancy\.AY-BY',
             'dot_pattern_expectancy\.BX-BY',
             'go_nogo',
             'hierarchical_rule\.score',
             'holt_laury_survey\.risk_aversion',
             'information_sampling_task\..*P_correct',
             'keep_track\.score',
             'kirby\.hyp_discount_rate_medium',
             'local_global_letter\.conflict',
             'local_global_letter\.global',
             'local_global_letter\.switch',
             'motor_selective_stop_signal\.SSRT',
             'probabilistic_selection',
             'psychological_refractory_period_two_choices',
             'ravens\.score',
             'recent_probes\.proactive_interference',
             'shape_matching\.stimulus_interference',
             'shift_task\.model',
             'simon\.simon',
             'simple_reaction_time',
             'spatial_span',
             'stim_selective_stop_signal\.SSRT',
             '^stop_signal\.SSRT_high',
             'stroop\.stroop',
             'threebytwo\.cue_switch',
             'threebytwp\.task_switch',
             'tower_of_london\.planning_time',
             'two_stage_decision\.model'
             ]

    demo_vars = ['Age', 'Sex']
    raven_vars = ['ravens.score*']

    stress_vars_pre = ['lon_pre*', 'pss_pre*', 'soc_supp_pre*']
    stress_vars_post = ['lon_post*', 'pss_post*', 'soc_supp_post*']
    mindset_vars  = ['mindset*']


    # filter columns if filter_regex is set
    if filter_regex.rstrip('s').lower() == 'survey':
        regex = not_regex(not_regex('survey')+'|cognitive_reflection|holt')
    elif filter_regex.rstrip('s').lower() == 'task':
        regex = not_regex('survey')+'|cognitive_reflection|holt'
    elif filter_regex.lower() == 'main':
        regex = '|'.join(main_vars)

    elif filter_regex.lower() == 'demo':
        regex = '|'.join(demo_vars)

    elif filter_regex.lower() == 'stress_pre':
            regex = '|'.join(stress_vars_pre)

    elif filter_regex.lower() == 'stress_post':
        regex = '|'.join(stress_vars_post)

    elif filter_regex.lower() == 'mindset':
            regex = '|'.join(mindset_vars)

    elif filter_regex.lower() == 'raven':
        regex = '|'.join(raven_vars)

    else:
        regex = filter_regex
    return data.filter(regex=regex)

def get_var_category(var):
    ''' Return "task" or "survey" classification for variable

    var: variable name passed as a string
    '''
    m = re.match(not_regex('survey')+'|cognitive_reflection|holt', var)
    if m is None:
        return 'survey'
    else:
        return 'task'

# Data get methods
def sorting(L):
    date = L.split('_')[-1]
    month,day,year = date.split('-')
    return year, month, day

def get_recent_dataset(data_subset=None):
    basedir=get_info('base_directory')
    if data_subset == 'Data_master':
        files = glob(os.path.join(basedir,'Data_master/Complete*'))
        files.sort(key=sorting)
        dataset = files[-1].split(os.sep)[-1]
    else:
        files = glob(os.path.join(basedir,'Data/Complete*'))
        files.sort(key=sorting)
        dataset = files[-1].split(os.sep)[-1]

    return dataset

def get_behav_data(dataset=None, file=None, filter_regex=None,
                flip_valence=False, verbose=False, full_dataset=None, data_subset=None):
    '''Retrieves a file from a data release.

    By default extracts meaningful_variables from the most recent Complete dataset.

    Args:
        dataset: optional, string indicating discovery, validation, or complete dataset of interest
        file: optional, string indicating the file of interest
        filter_regex: regex expression to filter data columns on. Can also supply
            "survey(s)" or "task(s)" to return measures associated with those
        flip_valence: bool, default false. If true use DV_valence.csv to flip variables based on their subjective valence
    '''
    if full_dataset is not None:
        print("Full dataset is deprecrated and no longer functional")

    basedir=get_info('base_directory')

    if dataset == None:
        dataset = get_recent_dataset()

    if data_subset == 'Data_master':
        datadir = os.path.join(basedir, data_subset,'Complete_02-16-2019')

    elif data_subset == 'Data_psych':
        datadir = os.path.join(basedir, data_subset,'Rouault_2018')

    elif data_subset =='Data_crisis':
        datadir = os.path.join(basedir, data_subset,'PA_Data_Sharing/April/Data')

    else:
        datadir = os.path.join(basedir,'Data',dataset)

    if file == None:
        file = 'meaningful_variables.csv'
    if verbose:
        print('Getting dataset: %s...:\n' 'file: %s \n ' % (datadir, file))
    datafile=os.path.join(datadir,file)
    if os.path.exists(datafile):
        data=pandas.read_csv(datafile,index_col=0)
    else:
        data = pandas.DataFrame()
        print('Error: %s not found in %s' % (file, datadir))
        return None

    def valence_flip(data, flip_list):
        for c in data.columns:
            try:
                data.loc[:,c] = data.loc[:,c] * flip_list.loc[c]
            except TypeError:
                continue
    if flip_valence==True:
        print('Flipping variables based on valence')
        flip_df = os.path.join(datadir, 'DV_valence.csv')
        valence_flip(data, flip_df)
    if filter_regex is not None:
        data = filter_behav_data(data, filter_regex=filter_regex)
    return data.sort_index()


def get_behav_data_master(dataset=None, file=None, filter_regex=None,
                flip_valence=False, verbose=False, full_dataset=None):
    '''Retrieves a file from a data release.

    By default extracts meaningful_variables from the most recent Complete dataset.

    Args:
        dataset: optional, string indicating discovery, validation, or complete dataset of interest
        file: optional, string indicating the file of interest
        filter_regex: regex expression to filter data columns on. Can also supply
            "survey(s)" or "task(s)" to return measures associated with those
        flip_valence: bool, default false. If true use DV_valence.csv to flip variables based on their subjective valence
    '''
    if full_dataset is not None:
        print("Full dataset is deprecrated and no longer functional")

    basedir=get_info('base_directory')
    if dataset == None:
        dataset = get_recent_dataset()
    datadir = os.path.join(basedir,'Data_master',dataset)
    if file == None:
        file = 'meaningful_variables.csv'
    if verbose:
        print('Getting dataset: %s...:\n' 'file: %s \n ' % (datadir, file))
    datafile=os.path.join(datadir,file)
    if os.path.exists(datafile):
        data=pandas.read_csv(datafile,index_col=0)
    else:
        data = pandas.DataFrame()
        print('Error: %s not found in %s' % (file, datadir))
        return None

    def valence_flip(data, flip_list):
        for c in data.columns:
            try:
                data.loc[:,c] = data.loc[:,c] * flip_list.loc[c]
            except TypeError:
                continue
    if flip_valence==True:
        print('Flipping variables based on valence')
        flip_df = os.path.join(datadir, 'DV_valence.csv')
        valence_flip(data, flip_df)
    if filter_regex is not None:
        data = filter_behav_data(data, filter_regex=filter_regex)
    return data.sort_index()




def get_retest_data(dataset):
    retest_data = get_behav_data(dataset, file='bootstrap_merged.csv.gz')
    if retest_data is None:
        return
    retest_data = retest_data[~retest_data.index.isnull()]
    retest_data = retest_data.loc[:, ['dv','icc3.k', 'spearman', 'pearson']]
    for column in retest_data.columns[1:]:
        retest_data[column] = pandas.to_numeric(retest_data[column])
    retest_data = retest_data.groupby('dv').mean()
    retest_data.rename(index={'dot_pattern_expectancy.BX.BY_hddm_drift': 'dot_pattern_expectancy.BX-BY_hddm_drift',
                        'dot_pattern_expectancy.AY.BY_hddm_drift': 'dot_pattern_expectancy.AY-BY_hddm_drift'},
                        inplace=True)
    return retest_data

def get_info(item,infile=None):
    """
    get info from settings file
    """
    config=pkg_resources.resource_string('selfregulation',
                        'data/Self_Regulation_Settings.txt')
    config=str(config,'utf-8').strip()
    infodict={}
    for l in config.split('\n'):
        if l.find('#')==0:
            continue
        l_s=l.rstrip('\n').split(':')
        if len(l_s)>1:
                infodict[l_s[0]]=l_s[1]
    if (item == 'dataset') and (not 'dataset' in infodict):
        files = glob(os.path.join(infodict['base_directory'],'Data/Complete*'))
        files.sort(key=sorting)
        datadir = files[-1]
        return os.path.basename(datadir)
    try:
        assert item in infodict
    except:
        raise Exception('infodict does not include requested item: %s' % item)
    return infodict[item]

def get_item_metadata(survey, dataset=None,verbose=False):
    data = get_behav_data(dataset=dataset, file=os.path.join('Individual_Measures',
                                                             '%s.csv.gz' % survey))

    metadata = []
    for i in data.question_num.unique():
        item = data[data['question_num'] == i].iloc[0].to_dict()
        # drop unnecessary variables
        for drop in ['battery_name', 'finishtime', 'required', 'response',
                     'response_text', 'worker_id','experiment_exp_id']:
            try:
                item.pop(drop)
            except KeyError:
                continue
        if type(item['options']) != list:
            if verbose:
                print(item['options'])
            item['options'] = eval(item['options'])
        # turn options into an ordered dict, indexed by option number
        item['responseOptions']=OrderedDict()
        for o in item['options']:
            option_num=int(o['id'].split('_')[-1])
            o.pop('id')
            o['valueOrig']=option_num
            try:
                v=int(o['value'])
            except ValueError:
                v=o['value']
            if v in item['responseOptions']:
                item['responseOptions'][v]['valueOrig']=[item['responseOptions'][v]['valueOrig'],o['valueOrig']]
                item['responseOptions'][v]['text']=[item['responseOptions'][v]['text'],o['text']]
            else:
                item['responseOptions'][v]=o.copy()
                item['responseOptions'][v].pop('value')
        # scoring
        values = [int(i['value']) for i in item['options']]
        sorted_values = list(range(1,len(values)+1))
        cc=numpy.corrcoef(values,sorted_values)[0,1]
        if cc>0.5:
            item['scoring'] = 'Forward'
        elif cc<0.5:
            item['scoring'] = 'Reverse'
        else:
            item['scoring'] = 'other'
        # convert from numpy.int64 since it's not json serializable
        item['question_num']=int(item['question_num'])
        item_s=item['id'].replace('_options','').split('_')
        item['expFactoryName']='_'.join(item_s[:-1])+'.'+item_s[-1]
        item.pop('id')
        item.pop('options')
        metadata.append(item)
    return metadata

def get_demographics(dataset=None, cleanup=True, num_response_thresh=10,
                     drop_categorical=True, drop_crisis = False, verbose=False, data_subset=None):
    """ Preprocess and return demographic data

    Args:
        dataset: optional, which data release to draw from. The most recent one
            will be used if not specified
        cleanup: bool, indicated whether to remove data that is impossible
            (e.g. really low weights or heights)
        num_response_thresh: int, number of NaN responses allowed before removing
            variable
        drop_categorical: bool, whether to drop categorical variables

    """
    categorical_vars = ['HispanicLatino','Race',
                        'DiseaseDiagnoses', 'DiseaseDiagnosesOther',
                        'MotivationForParticipation', 'MotivationOther',
                        'NeurologicalDiagnoses',
                        'NeurologicalDiagnosesDescribe',
                        'OtherDebtSources',
                        'OtherDrugs', 'OtherRace', 'OtherTobaccoProducts',
                        'PsychDiagnoses',
                        'PsychDiagnosesOther']

    crisis_vars = ['country','state','other_state',
                    'crisis_currentlyworking', 'crisis_occupation', 'crisis_military',
                    'crisis_urbanicity', 'crisis_householdpeople', 'crisis_householdrelation',
                    'crisis_esssentialworkers', 'crisis_workershome', 'crisis_workersforcovid',
                    'crisis_householdrooms', 'crisis_healthinsurance', 'crisis_moneysupport',
                    'crisis_physicalhealth', 'crisis_mentalhealth']

    demogdata=get_behav_data(file = 'demographic_health.csv', data_subset = data_subset, verbose=True)
    if cleanup:
        q=demogdata.query('WeightPounds<50')
        for i in q.index:
            demogdata.loc[i,'WeightPounds']=numpy.nan
        if verbose and len(q)>0:
            print('replacing bad WeightPounds value for', list(q.index))
        q=demogdata.query('HeightInches<36')
        for i in q.index:
            demogdata.loc[i,'HeightInches']=numpy.nan
        if verbose and len(q)>0:
            print('replacing bad HeightInches value for', list(q.index))
        q=demogdata.query('CaffienatedSodaCansPerDay<0')
        for i in q.index:
            demogdata.loc[i,'CaffienatedSodaCansPerDay']=numpy.nan
        q=demogdata.query('CaffieneOtherSourcesDayMG>2000')
        for i in q.index:
            demogdata.loc[i,'CaffieneOtherSourcesDayMG']=numpy.nan
        if verbose and len(q)>0:
            print('replacing bad CaffienatedSodaCansPerDay value for', list(q.index))
    #MV out for now as different weight and high system in covid need to fix
    #demogdata=demogdata.assign(BMI=demogdata['WeightPounds']*0.45 / (demogdata['HeightInches']*0.025)**2)
    if drop_categorical:
       demogdata.drop(categorical_vars, axis=1, inplace=True)
       if verbose:
           print('dropping categorical variables')


    #MV out for now as different weight and high system in covid need to fix
    #demogdata=demogdata.assign(Obese=(demogdata['BMI']>30).astype('int'))

    if drop_crisis:
       demogdata.drop(crisis_vars, axis=1, inplace=True)
       if verbose:
           print('dropping crisis variables')

    # only keep variables with fewer NaNs then num_response_thresh
    if num_response_thresh is not None:
        good_vars = demogdata.isnull().sum() <= num_response_thresh
        demogdata = demogdata.loc[:,good_vars]
    return demogdata

def get_single_dataset(dataset,survey):
    basedir=get_info('base_directory')
    infile=os.path.join(basedir,'data/Derived_Data/%s/surveydata/%s.tsv'%(dataset,survey))
    print(infile)
    assert os.path.exists(infile)
    if survey.find('ordinal')>-1:
        survey=survey.replace('_ordinal','')
    mdfile=os.path.join(basedir,'data/Derived_Data/%s/metadata/%s.json'%(dataset,survey))
    print(mdfile)
    assert os.path.exists(mdfile)
    data=pandas.read_csv(infile,index_col=0,sep='\t')
    metadata=load_metadata(survey,os.path.join(basedir,
        'data/Derived_Data/%s/metadata'%dataset))
    return data,metadata


def get_survey_data(dataset):
    basedir=get_info('base_directory')
    infile=os.path.join(basedir,'Data/Derived_Data/%s/surveydata.csv'%dataset)
    surveydata=pandas.read_csv(infile,index_col=0)
    keyfile=os.path.join(basedir,'Data/Derived_Data/%s/surveyitem_key.txt'%dataset)
    with open(keyfile) as f:
        keylines=[i.strip().split('\t') for i in f.readlines()]
    surveykey={}
    for k in keylines:
        surveykey[k[0]]=k[2]
    return surveydata,surveykey

def get_admin_data(data_dir, filename):
    admin_dir = (path.join(data_dir))
    with open((path.join(admin_dir, filename)), 'r') as f:
        return(json.load(f))

def get_turkers_finishing_battery(dictionary, string):
    return {k:v for k,v in dictionary.items() if k.startswith(string)}


def get_overlap_mturk_id(dict_a, dict_b):
    keys_a = set(dict_a.values())
    keys_b = set(dict_b.values())
    overlap_values = keys_a & keys_b
    return overlap_values

def print_confusion_matrix(y_true,y_pred,labels=[0,1]):
    cm=confusion_matrix(y_true,y_pred)
    print('Confusion matrix')
    print('\t\tPredicted')
    print('\t\t0\t1')
    print('Actual\t0\t%d\t%d'%(cm[0,0],cm[0,1]))
    print('\t1\t%d\t%d'%(cm[1,0],cm[1,1]))

def get_save_directory_train_test(data_folder):
    output_dir=path.join(get_info('base_directory'),data_folder)
    data_dir = get_recent_dataset(data_subset=data_folder)
    directory = path.join(output_dir, data_dir)
    return directory

def drop_constant_column(data, label_dataset):
    """
    Drops constant value columns of pandas dataframe and get name of the columns
    """
    data_drop = data.loc[:,data.nunique()!=1]
    colname_drop = set(data.loc[:,data.nunique()==1].columns)
    # if colname_drop ==set():
    #     print('Dataset', label_dataset , ', no need to remove variables')
    # else:
    #     print('Dataset', label_dataset , ',need to remove variables:')
    #     #print('These variables need to be removed because no variability')
    #     #print(' '.join(colname_drop))
    return(data_drop, colname_drop)

def remove_variables_from_df(data, drop_columns):
    data_drop= data.drop(list(drop_columns), axis = 1)
    return(data_drop)

def drop_not_common_vars(df, common_vars = None):
    drop_unique_vars = set(df.index)-set(common_vars)
    df.drop(drop_unique_vars, axis=0, inplace = True)
    return df.sort_index(axis = 0)

def remove_suffix_row(df, suffix):
    df.index = ['.'.join(i.split('.')[:-1]) if suffix in i else i for i in df.index]


def get_predicted_score_data(datadir, file, verbose = False):
    datafile=os.path.join(datadir,file)
    if os.path.exists(datafile):
        data=pandas.read_csv(datafile,index_col=0)
        if verbose:
            print('Getting dataset: %s' % (datafile))
    else:
        data = pandas.DataFrame()
        print('Error: %s not found' % (datafile))
        return None

    return data.sort_index()

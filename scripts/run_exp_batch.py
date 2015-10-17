import math

def template2str(filename, mapping):
    """
    read in a template file and substitute variables using mapping, return a str
    @param filename str
           absolute path and filename
    @param mapping dict
           for example. {'username','junyi'}
           key must be str; value can be str or type that can be converted to str
    @return subsitituted_file str
    """
    from string import Template
    filein  = open(filename)
    filestr = filein.read()

    for ikey in mapping:
        mapping[ikey] = str(mapping[ikey])

    file_temp = Template(filestr)
    subsitituted_file = file_temp.substitute(mapping)
    return subsitituted_file

def write2file(filename, str2write):
    """
    write str in `str2write` to file `filename`
    """
    file_handle = file(filename,'w')
    file_handle.write(str2write)
    file_handle.close()
    return

template_dir = '../run_env/'
f1_dir      = '/lustre/f1/Junyi.Chai/'
exe_file    = '../exec/shallow_water.x'

#--------------- experiment parameters -------------------
for exp_i in range(3, 21):
    exp_name    = 'Oct17_amp' + str(exp_i)
    num_procs   = 32
    walltime    = '00:10:00'
    zeta0       = exp_i*1.1458e-05 #wave amplitude
    
    
    exp_dir = f1_dir + exp_name + '/'
    
    runscript_template = template_dir + 'runscript.template'
    transfer_template  = template_dir + 'transferScript.template'
    input_template     = template_dir + 'input.nml.template'
    
    runscript_params = {'num_procs':num_procs, 'exp_name':exp_name, 'walltime':walltime}
    transfer_params  = {'exp_name':exp_name}
    input_params     = {'zeta0': zeta0}
    
    runscript_str = template2str(runscript_template, runscript_params)
    transfer_str  = template2str(transfer_template,  transfer_params)
    input_str     = template2str(input_template,     input_params)
    
    import os
    import stat
    os.mkdir(exp_dir)
    READ_WRITE_EXE = stat.S_IREAD|stat.S_IEXEC|stat.S_IWRITE
    write2file(exp_dir + 'runscript', runscript_str)
    os.chmod(  exp_dir + 'runscript', READ_WRITE_EXE)
    write2file(exp_dir + 'transferScript', transfer_str)
    os.chmod(  exp_dir + 'transferScript', READ_WRITE_EXE)
    write2file(exp_dir + 'input.nml', input_str)
    os.chmod(  exp_dir + 'input.nml', READ_WRITE_EXE)
    
    import shutil
    shutil.copyfile(template_dir + 'restartNum', exp_dir + 'restartNum')
    shutil.copyfile(exe_file, exp_dir + 'shallow_water.x')
    shutil.copyfile(template_dir + 'combine', exp_dir + 'combine')
    shutil.copyfile(template_dir + 'diag_table', exp_dir + 'diag_table')
    os.chmod(template_dir + 'restartNum', READ_WRITE_EXE)
    os.chmod(exp_dir + 'shallow_water.x', READ_WRITE_EXE)
    os.chmod(exp_dir + 'combine', READ_WRITE_EXE)
    os.mkdir(exp_dir + 'INPUT/')
    os.mkdir(exp_dir + 'RESTART/')
    
    print 'Run environment preparation success at dir: %s' %exp_dir

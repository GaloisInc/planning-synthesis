from tulip import spec, synth, transys
from tulip import dumpsmach
import cPickle as pickle

dim = 8
dime = 3
env_vars = {'env2':(0,8)}
#env_init = {'env3 = 0', 'env2 = 0'}
env_init = set()
env_init |= {'env2 = 0 || env2 = 1|| env2 = 2|| env2 = 3|| env2 = 5 || env2 = 6 || env2 = 7 || env2 = 8'}
env_safe = set()
# The environment robots have to move to adjacent cells at each step
for iter1 in range(dime*dime):
	if (iter1 == 0):
		env_safe |= {('(env2='+str(iter1)+') -> X((env2='+str(iter1)+') || (env2='+str(iter1+1)+')|| (env2='+str(iter1+2)+')  || (env2='+str(iter1+dime)+')|| (env2='+str(iter1+(2*dime))+'))')}
	elif (iter1 == 1):
		env_safe |= {('(env2='+str(iter1)+') -> X ((env2='+str(iter1)+') || (env2 ='+str(iter1+1)+')  || (env2='+str(iter1-1)+')|| (env2 ='+str((iter1+1)+dime)+')  || (env2='+str((iter1-1)+dime)+'))')}
	elif (iter1 == 2):
		env_safe |= {('(env2='+str(iter1)+') -> X ((env2='+str(iter1)+') || (env2 ='+str(iter1-1)+') || (env2 ='+str(iter1-2)+') || (env2='+str(iter1+dime)+')|| (env2='+str(iter1+(2*dime))+'))')}
	elif (iter1 == 3):
		env_safe |= {('(env2='+str(iter1)+') -> X ((env2='+str(iter1)+') || (env2='+str(iter1-dime)+') || (env2='+str(iter1+dime)+')|| (env2='+str(iter1-dime+1)+') || (env2='+str(iter1+dime+1)+'))')}
	elif (iter1 == 5):
		env_safe |= {('(env2='+str(iter1)+') -> X ((env2='+str(iter1)+') || (env2='+str(iter1-dime)+') || (env2='+str(iter1+dime)+')|| (env2='+str(iter1-dime-1)+') || (env2='+str(iter1+dime-1)+'))')}
	elif iter1 == 6:
		env_safe |= {('(env2='+str(iter1)+') -> X((env2='+str(iter1)+') || (env2='+str(iter1+1)+')|| (env2='+str(iter1+2)+')  || (env2='+str(iter1-dime)+')|| (env2='+str(iter1-(2*dime))+'))')}
	elif iter1 == 7:
		env_safe |= {('(env2='+str(iter1)+') -> X ((env2='+str(iter1)+') || (env2 ='+str(iter1+1)+')  || (env2='+str(iter1-1)+')|| (env2 ='+str(iter1+1-dime)+')  || (env2='+str(iter1-1-dime)+'))')}
	elif iter1 == 8:
		env_safe |= {('(env2='+str(iter1)+') -> X ((env2='+str(iter1)+') || (env2 ='+str(iter1-1)+')  || (env2='+str(iter1-dime)+')|| (env2 ='+str(iter1-2)+')  || (env2='+str(iter1-(2*dime))+'))')}

#env_init |= {'env3 = 0'}
#env_init |= {'env2 = 7'}

env_prog = set()
#env_prog |= {'env3 = 7'}
env_prog |= {'env2 = 0'}
#env_prog |= {'env3 = 0'}
env_prog |= {'env2 = 8'}
sys_vars = {}
sys_vars['loc'] = (0, 63)
sys_vars['stage'] = (0, 1)
sys_init = {'loc = 45','stage = 0'}
#sys_init = {'stage = 0'}

sys_safe = set()
# The transition of 'stage'
sys_safe |= {('((stage = 0) && (! (loc = 60))) -> X (stage = 0)')}
sys_safe |= {('((stage = 0) && (loc = 60)) -> X (stage = 1)')}
sys_safe |= {('((stage = 1) && (! (loc = 45))) -> X (stage = 1)')}
sys_safe |= {('((stage = 1) && (loc = 45)) -> X (stage = 0)')}


for iter1 in range(dim*dim):
	if (iter1 % dim != 0) & (iter1 % dim != dim-1) & (iter1 / dim != 0) & (iter1 / dim != dim-1):
		sys_safe |= {('(loc='+str(iter1)+') -> X((loc='+str(iter1)+') || (loc='+str(iter1+1)+') ||( loc='+str(iter1-1)+') || (loc='+str(iter1-dim)+') || (loc='+str(iter1+dim)+'))')}
	elif (iter1 % dim == 0) & (iter1 / dim != 0) & (iter1 / dim != dim-1):
		sys_safe |= {('(loc='+str(iter1)+') -> X ((loc='+str(iter1)+') || (loc ='+str(iter1+1)+') || (loc='+str(iter1-dim)+') || (loc='+str(iter1+dim)+'))')}
	elif (iter1 % dim == dim-1) & (iter1 / dim != 0) & (iter1 / dim != dim-1):
		sys_safe |= {('(loc='+str(iter1)+') -> X ((loc='+str(iter1)+') || (loc ='+str(iter1-1)+') || (loc='+str(iter1-dim)+') || (loc='+str(iter1+dim)+'))')}
	elif (iter1 % dim != 0) & (iter1 % dim != dim-1) & (iter1 / dim == 0):
		sys_safe |= {('(loc='+str(iter1)+') -> X ((loc='+str(iter1)+') || (loc ='+str(iter1-1)+') || (loc='+str(iter1+1)+') || (loc='+str(iter1+dim)+'))')}
	elif (iter1 % dim != 0) & (iter1 % dim != dim-1) & (iter1 / dim == dim-1):
		sys_safe |= {('(loc='+str(iter1)+') -> X ((loc='+str(iter1)+') || (loc ='+str(iter1-1)+') || (loc='+str(iter1-dim)+') || (loc='+str(iter1+1)+'))')}
	elif iter1 == 0:
		sys_safe |= {('(loc='+str(iter1)+') -> X ((loc='+str(iter1)+') || (loc ='+str(iter1+1)+') || (loc='+str(iter1+dim)+'))')}
	elif iter1 == dim-1:
		sys_safe |= {('(loc='+str(iter1)+') -> X ((loc='+str(iter1)+') || (loc ='+str(iter1-1)+') || (loc='+str(iter1+dim)+'))')}
	elif iter1 == dim*(dim-1):
		sys_safe |= {('(loc='+str(iter1)+') -> X ((loc='+str(iter1)+') || (loc ='+str(iter1+1)+') || (loc='+str(iter1-dim)+'))')}
	elif iter1 == dim*dim-1:
		sys_safe |= {('(loc='+str(iter1)+') -> X ((loc='+str(iter1)+') || (loc ='+str(iter1-1)+') || (loc='+str(iter1-dim)+'))')}

#The system robot should never collide with either environment robots


sys_safe |= {('(loc = 51) -> (!(env2 = 0)) && X(!(env2 = 0)) && X(X(!(env2 = 0)))')}
sys_safe |= {('(loc = 52) -> (!(env2 = 1)) && X(!(env2 = 1)) && X(X(!(env2 = 1)))')}
sys_safe |= {('(loc = 53) -> (!(env2 = 2)) && X(!(env2 = 2)) && X(X(!(env2 = 2)))')}
sys_safe |= {('(loc = 59) -> (!(env2 = 3)) && X(!(env2 = 3)) && X(X(!(env2 = 3)))')}
sys_safe |= {('(loc = 61) -> (!(env2 = 5)) && X(!(env2 = 5)) && X(X(!(env2 = 5)))')}
sys_safe |= {('(loc = 67) -> (!(env2 = 6)) && X(!(env2 = 6)) && X(X(!(env2 = 5)))')}
sys_safe |= {('(loc = 68) -> (!(env2 = 7)) && X(!(env2 = 7)) && X(X(!(env2 = 6)))')}
sys_safe |= {('(loc = 69) -> (!(env2 = 8)) && X(!(env2 = 8)) && X(X(!(env2 = 7)))')}
for iter1 in range(dim):
        sys_safe |= {('!(loc = '+str(iter1+(4*dim))+')')}



sys_prog = set()
sys_prog |= {'stage = 0'}
sys_prog |= {'stage = 1'}
specs = spec.GRSpec(env_vars, sys_vars, env_init, sys_init, env_safe, sys_safe, env_prog, sys_prog)
ctrl = synth.synthesize('gr1c', specs)
dumpsmach.write_python_case("democontroller3.py", ctrl, classname="ExampleCtrl3")
#pickle.dump(ctrl, open("pro3_1_ctrl.pkl", "wb"), pickle.HIGHEST_PROTOCOL)
print specs.pretty()

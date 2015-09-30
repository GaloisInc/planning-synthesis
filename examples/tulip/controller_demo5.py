from tulip import spec, synth, transys
from tulip import dumpsmach
import cPickle as pickle

dim = 8
env_vars = {'env2':(0,dim-1)}
#env_init = {'env3 = 0', 'env2 = 0'}
env_init = set()
env_safe = set()
# The environment robots have to move to adjacent cells at each step
for iter1 in range(dim-2):
	if dim - iter1 >= 3 and iter1 >=1 :
		env_safe |= {('(env2 = '+str(1 + iter1)+')  -> X (((env2 = '+str(iter1)+') ) || ((env2 = '+str(iter1+2)+') )|| ((env2 = '+str(1+iter1+2)+') )|| ((env2 = '+str(iter1-1)+') )|| ((env2 = '+str(1+iter1)+') ) )')}
	elif iter1 >= 1:
		env_safe |= {('(env2 = '+str(1 + iter1)+')  -> X (((env2 = '+str(iter1)+') ) || ((env2 = '+str(iter1+2)+') )|| ((env2 = '+str(iter1-1)+') )|| ((env2 = '+str(1+iter1)+') )|| ((env2 = '+str(1+iter1+dim)+') ) )')}
	elif dim - iter1 >= 3:
		env_safe |= {('(env2 = '+str(1 + iter1)+')  -> X (((env2 = '+str(iter1)+') ) || ((env2 = '+str(iter1+2)+') )|| ((env2 = '+str(1+iter1+2)+') )|| ((env2 = '+str(1+iter1)+') )|| ((env2 = '+str(1+iter1+dim)+') ) )')}		
	else:
		env_safe |= {('(env2 = '+str(1 + iter1)+')  -> X (((env2 = '+str(iter1)+') ) || ((env2 = '+str(iter1+2)+') )|| ((env2 = '+str(1+iter1)+') ) )')}

env_safe |= {('((env2 = 0) )-> X ((env2 = 1) || (env2 = 0) || (env2=2))')}


env_safe |= {('((env2 = '+str(dim-1)+') )-> X ((env2 = '+str(dim-2)+') || (env2 = '+str(dim-1)+') || (env2 = '+str(dim-3)+') )')}

#env_init |= {'env2 = 7'}

env_prog = set()
env_prog |= {'env2 = 0'}
env_prog |= {'env2 = 7'}
sys_vars = {}
sys_vars['loc'] = (0, 63)
sys_vars['stage'] = (0, 1)
sys_init = {'loc = 21','stage = 0'}
#sys_init = {'stage = 0'}

sys_safe = set()
# The transition of 'stage'
sys_safe |= {('((stage = 0) && (! (loc = 0))) -> X (stage = 0)')}
sys_safe |= {('((stage = 0) && (loc = 0)) -> X (stage = 1)')}
sys_safe |= {('((stage = 1) && (! (loc = 16))) -> X (stage = 1)')}


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
for iter1 in range(dim):
	#sys_safe |= {('!(env2 = '+str(iter1)+') || !(loc = '+str(iter1+dim)+')')}
        sys_safe |= {('(loc = '+str(iter1+(dim))+') -> X(!(env2 = '+str(iter1)+'))')}
        sys_safe |= {('!(loc = '+str(iter1+(3*dim))+')')}

sys_prog = set()

sys_prog |= {'stage = 1'}
specs = spec.GRSpec(env_vars, sys_vars, env_init, sys_init, env_safe, sys_safe, env_prog, sys_prog)
ctrl = synth.synthesize('gr1c', specs)
dumpsmach.write_python_case("democontroller5.py", ctrl, classname="ExampleCtrl5")
#pickle.dump(ctrl, open("pro3_1_ctrl.pkl", "wb"), pickle.HIGHEST_PROTOCOL)
print specs.pretty()

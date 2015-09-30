from tulip import spec, synth, transys
#from tulip.transys import executions
#from tulip.transys.executions import MachineInputSequence
import cPickle as pickle
import time
from tulip import dumpsmach
start = time.clock()
x = 8
y = 8

s_states = x * y
sys = transys.FTS()
env1 = transys.FTS()

env1.owner = 'env'

e1_states = x
e2_states = x
s_stateslist = []
e_stateslist = []

s_atomiclist = []
e_atomiclist = []

speclist = []

for i in range(0,s_states):
    s_stateslist.append('s'+str(i))

for i in range(0,s_states):
    s_atomiclist.append('sa'+str(i))

for r in range(0,e1_states):
	for i in range(0,e2_states):
		e_stateslist.append('e'+str(r+x)+str(i+(2*x)))

for r in range(0,e1_states):
	for i in range(0,e2_states):
		e_atomiclist.append('ea1'+str(r+x))
		e_atomiclist.append('ea2'+str(i+(2*x)))

sys.states.add_from(s_stateslist)

s_atomicset = set(s_atomiclist)

sys.atomic_propositions.add_from(s_atomicset)

for i in range(0,s_states):
	sys.states.add('s'+str(i), ap={'sa'+str(i)})
sys.states.initial.add('s0')

env1.states.add_from(e_stateslist)

e_atomicset = set(e_atomiclist)
env1.atomic_propositions.add_from(e_atomicset)

#env1.states.initial.add('e1'+str(x))
#env1.states.initial.add('e2'+str(1(x-1)))

for r in range(0,e1_states):
	for i in range(0,e2_states):
		env1.states.add('e'+str(r+x)+str(i+(2*x)), ap={'ea1'+str(r+x),'ea2'+str(i+(2*x))})
		env1.states.initial.add('e'+str(r+x)+str(i+(2*x)))

	#env1.states.initial.add('e1'+str(i+x))
	#env1.states.initial.add('e2'+str(i+(2*x)))

for i in range(0,s_states):
	print i
	if i == 0:
		sys.transitions.add_comb({'s'+str(i)}, {'s'+str(i),'s'+str(i+1),'s'+str(i+x)})
	elif i == x - 1:
		sys.transitions.add_comb({'s'+str(i)}, {'s'+str(i),'s'+str(i-1),'s'+str(i+x)})
	elif i == (x * y) - 1:
		sys.transitions.add_comb({'s'+str(i)}, {'s'+str(i),'s'+str(i-1),'s'+str(i-x)})
	elif i == x * (y - 1):
		sys.transitions.add_comb({'s'+str(i)}, {'s'+str(i),'s'+str(i+1),'s'+str(i-x)})
	elif i < x:
		sys.transitions.add_comb({'s'+str(i)}, {'s'+str(i),'s'+str(i+1),'s'+str(i+x),'s'+str(i-1)})
	elif i >= x * (y - 1):
		sys.transitions.add_comb({'s'+str(i)}, {'s'+str(i),'s'+str(i+1),'s'+str(i-x),'s'+str(i-1)})
	elif i % x == 0:
		sys.transitions.add_comb({'s'+str(i)}, {'s'+str(i),'s'+str(i+1),'s'+str(i+x),'s'+str(i-x)})
	elif i % x == x - 1:
		sys.transitions.add_comb({'s'+str(i)}, {'s'+str(i),'s'+str(i-1),'s'+str(i+x),'s'+str(i-x)})
	else:
		sys.transitions.add_comb({'s'+str(i)}, {'s'+str(i),'s'+str(i-1),'s'+str(i+x),'s'+str(i-x),'s'+str(i+1)})

for i in range(0,s_states):
	sys.states.add('s'+str(i), ap={'sa'+str(i)})

for r in range(0,e1_states):
	for i in range(0,e2_states):
		if r == 0:
			if i == 0:
				env1.transitions.add_comb({'e'+str(r+x)+str(i+(2*x))}, {'e'+str(r+(x+1))+str(i+((2*x)+1))})
			elif i == e2_states -1:
				env1.transitions.add_comb({'e'+str(r+x)+str(i+(2*x))}, {'e'+str(r+(x+1))+str(i+((2*x)-1))})
			else:
				env1.transitions.add_comb({'e'+str(r+x)+str(i+(2*x))}, {'e'+str(r+(x+1))+str(i+((2*x)-1)),'e'+str(r+(x+1))+str(i+((2*x)+1))})

		elif r == e1_states - 1:
			if i == 0:
				env1.transitions.add_comb({'e'+str(r+x)+str(i+(2*x))}, {'e'+str(r+(x-1))+str(i+((2*x)+1))})
			elif i == e2_states -1:
				env1.transitions.add_comb({'e'+str(r+x)+str(i+(2*x))}, {'e'+str(r+(x-1))+str(i+((2*x)-1))})
			else:
				env1.transitions.add_comb({'e'+str(r+x)+str(i+(2*x))}, {'e'+str(r+(x-1))+str(i+((2*x)-1)),'e'+str(r+(x-1))+str(i+((2*x)+1))})
		else:
			if i == 0:
				env1.transitions.add_comb({'e'+str(r+x)+str(i+(2*x))}, {'e'+str(r+(x+1))+str(i+((2*x)+1)),'e'+str(r+(x-1))+str(i+((2*x)+1))})
			elif i == e2_states -1:
				env1.transitions.add_comb({'e'+str(r+x)+str(i+(2*x))}, {'e'+str(r+(x+1))+str(i+((2*x)-1)),'e'+str(r+(x-1))+str(i+((2*x)-1))})
			else:
				env1.transitions.add_comb({'e'+str(r+x)+str(i+(2*x))}, {'e'+str(r+(x+1))+str(i+((2*x)-1)),'e'+str(r+(x+1))+str(i+((2*x)+1)),'e'+str(r+(x-1))+str(i+((2*x)-1)),'e'+str(r+(x-1))+str(i+((2*x)+1))})

for r in range(0,e1_states):
	for i in range(0,e2_states):
			speclist.append('!(ea1'+str(r+x)+' & sa'+str(r+x)+')')
			speclist.append('!(ea2'+str(i+(2*x))+' & sa'+str(i+(2*x))+')')

specset = set(speclist)

env_vars = set()
env_init = set()
env_safe = set()
env_prog = set()
env_prog |= {'ea1'+str(2*x - 1)}
env_prog |= {'ea2'+str(2*x)}

sys_vars = dict()
sys_prog = set()
sys_safe = set()
sys_init = {'stage = 0'}
sys_vars['stage'] = (0, 3)
sys_prog |= {'stage = 0'}
sys_prog |= {'stage = 3'}
sys_safe |= {('((stage = 0) && (sa'+str((2*x)-1)+')) -> X (stage = 1)')}
sys_safe |= {('((stage = 0) && (!sa'+str((2*x)-1)+')) -> X (stage = 0)')}
sys_safe |= {('((stage = 1) && (sa'+str(2*x)+')) -> X (stage = 2)')}
sys_safe |= {('((stage = 1) && (!sa'+str(2*x)+')) -> X (stage = 1)')}
sys_safe |= {('((stage = 2) && (sa'+str((x*y) - 1)+')) -> X (stage = 3)')}
sys_safe |= {('((stage = 2) && (!sa'+str((x*y) - 1)+')) -> X (stage = 2)')}
sys_safe |= {('((stage = 3) && (sa'+str(0)+')) -> X (stage = 0)')}
sys_safe |= {('((stage = 3) && (!sa'+str(0)+')) -> X (stage = 3)')}

sys_safe |= specset


for iter1 in range(0,x-1):
	sys_safe |= {'((ea1'+str(iter1+x)+') && (sa'+str(iter1+x+1)+')) -> X !(sa'+str(iter1+x)+')'}
	sys_safe |= {'((ea1'+str(iter1+1+x)+') && (sa'+str(iter1 + x)+')) -> X !(sa'+str(iter1+1 + x)+')'} 
	sys_safe |= {'((ea2'+str(iter1+(2*x))+') && (sa'+str(iter1+1+ (2*x))+')) -> X !(sa'+str(iter1+ (2*x))+')'}
	sys_safe |= {'((ea2'+str(iter1+1+ (2*x))+') && (sa'+str(iter1 + (2*x))+')) -> X !(sa'+str(iter1+ (2*x)+1)+')'}
specs = spec.GRSpec(env_vars, sys_vars, env_init, sys_init,
                    env_safe, sys_safe, env_prog, sys_prog)
ctrl = synth.synthesize('gr1c',specs, sys=sys, env=env1)
finish = time.clock()
print finish - start
dumpsmach.write_python_case("gr1controller"+str(x)+".py", ctrl, classname="ExampleCtrl")
finish = time.clock()
print finish - start


#finish = time.clock()
#print finish - start
#print specs.pretty()

#if not ctrl.save('discerete2.png'):
#	print(ctrl)
#finish = time.clock()
#print finish - start

from sitl_client import *
from democontroller1 import ExampleCtrl
from democontroller2new import ExampleCtrl2
from democontroller3 import ExampleCtrl3
from democontroller4new import ExampleCtrl4
from democontroller5 import ExampleCtrl5
import time

M1 = ExampleCtrl()
M2 = ExampleCtrl2()
M3 = ExampleCtrl3()
M4 = ExampleCtrl4()
M5 = ExampleCtrl5()
mission2done = 0
mission1done = 0
mission3done = 0
mission = 1
xsize = 8
ysize = 9
blocklength = 80000
mission = 1
mission2done = 0
mission1done = 0
mission3done = 0
latbase = -3536536100
longtbase = 14916072800


latbasesmall = -3536056100
longtbasesmall = 14916312800

r = 0
def coord2state(lat, longt, xsizesmall = xsize ):
	lat = lat * 100000000
	longt = longt * 100000000
	yaxis = abs(int(lat - latbase))/blocklength
	xaxis = abs(int(longt - longtbase))/blocklength
	state = xaxis + (yaxis*xsizesmall)
	return state;
def coord2statesmall(lat, longt,xsizesmall = 3):
	lat = lat * 100000000
	longt = longt * 100000000
	yaxis = abs(int(lat - latbasesmall))/blocklength
	xaxis = abs(int(longt - longtbasesmall))/blocklength
	state = xaxis + (yaxis*xsizesmall)
	return state;

def accuratecoord2state(lat, longt,prevs,ac=0.1, xsizesmall = xsize):
        state = coord2state(lat,longt,xsizesmall)
	coord = state2coord(state)
        upperlon = coord[1] + (((blocklength/200000000.))*ac)
        lowerlon = coord[1] - (((blocklength/200000000.))*ac)
        upperlat = coord[0] + (((blocklength/200000000.))*ac)
        lowerlat = coord[0] - (((blocklength/200000000.))*ac)

        if (lat >= lowerlat and lat <= upperlat and longt >= lowerlon and longt <= upperlon):
	    return state
        else:
            return prevs

def state2coord(state):
	xaxis = state % xsize
	yaxis = state / xsize
	lat = latbase + (blocklength/2.) + (yaxis * blocklength ) 
	lat = float(lat) / 100000000.
	longt = longtbase + (blocklength/2.) + (xaxis * blocklength ) 
	longt = float(longt) / 100000000.
	return (lat,longt,50);

client = SitlClient()

print 'connecting'
client.connect()

print 'connected'
ps = client.get_positions()
print client.get_positions()
print 'start_obstacles'
client.start_obstacles()

for x in range(1,6000):

    if r == 0:
            clatSystem = client.get_positions()[0][1].lat
            clonSystem = client.get_positions()[0][1].lon
            clatEnv0 = client.get_positions()[1][1].lat
            clonEnv0 = client.get_positions()[1][1].lon
            clatEnv1 = client.get_positions()[2][1].lat
            clonEnv1 = client.get_positions()[2][1].lon
            clatEnv2 = client.get_positions()[3][1].lat
            clonEnv2 = client.get_positions()[3][1].lon
    	    env0past = coord2state(clatEnv0,clonEnv0)
            env1past = coord2state(clatEnv1,clonEnv1)
            env2past = coord2statesmall(clatEnv2,clonEnv2)

            r = 1
    else:
            env0past = env0new
            env1past = env1new
            env2past = env2new


    location = coord2state(clatSystem,clonSystem)
    if location == 8 and mission == 1:
        mission1done = 1
    if location == 21 and mission == 1 and mission1done ==1:
        mission = 2
    if location == 39 and mission1done == 1 and mission == 2:
        mission2done = 1
    if location == 45 and mission == 2 and mission2done == 1:
        mission = 3
    if location == 60 and mission == 3:
        mission3done = 1
    if location == 45 and mission3done == 1 and mission == 3:
        mission = 4
    if location == 21 and mission == 4:
        mission = 5
    if location == 0 and mission == 5:
        M1 = ExampleCtrl()
        M2 = ExampleCtrl2()
        M3 = ExampleCtrl3()
        M4 = ExampleCtrl4()
        M5 = ExampleCtrl5()
        mission2done = 0
        mission1done = 0
        mission3done = 0
        mission = 1

    if mission == 1:
        nextstate = M1.move(env0past-8)
    if mission == 2:
        nextstate = M2.move(env1past-24)
    if mission == 3:
        nextstate = M3.move(env2past)
    if mission == 4:
        nextstate = M4.move(env1past-24)
    if mission == 5:
        nextstate = M5.move(env0past-8)

    client.move_to(Position(state2coord(nextstate["loc"])[0],state2coord(nextstate["loc"])[1],state2coord(nextstate["loc"])[2]))
    print "Go to {0} Env0 {1} Env1 {2} Mission {3}".format(nextstate["loc"],env0past-8,env1past,mission)
    print "Commandlat: {0}".format(clatSystem)
    print "Commandlon: {0}".format(clonSystem)
    location = coord2state(clatSystem,clonSystem)
    env0new = env0past
    env1new = env1past
    env2new = env2past
 
    if location == nextstate["loc"]:

	    while (env0past == env0new and env1past == env1new and env2past == env2new ):
                        
			print "First case"
			clatSystem = client.get_positions()[0][1].lat
			clonSystem = client.get_positions()[0][1].lon
			clatEnv0 = client.get_positions()[1][1].lat
			clonEnv0 = client.get_positions()[1][1].lon
			clatEnv1 = client.get_positions()[2][1].lat
			clonEnv1 = client.get_positions()[2][1].lon
			clatEnv2 = client.get_positions()[3][1].lat
			clonEnv2 = client.get_positions()[3][1].lon

			time.sleep(2)
			location = accuratecoord2state(clatSystem,clonSystem,location)
			env0new = accuratecoord2state(clatEnv0,clonEnv0,env0new,1)
			env1new = accuratecoord2state(clatEnv1,clonEnv1,env1new,1)
			env2new = coord2statesmall(clatEnv2,clonEnv2)

			print "clatSystem: {0}".format(clatSystem)
			print "clatEnv0: {0}".format(clatEnv0)
			print "clatEnv1: {0}".format(clatEnv1)
			print "clatEnv2: {0}".format(clatEnv2)
			print "clonSystem: {0}".format(clonSystem)
			print "clonEnv0: {0}".format(clonEnv0)
			print "clonEnv1: {0}".format(clonEnv1)
			print "clonEnv2: {0}".format(clonEnv2)
			print "System: {0}".format(location)
			print "Env0: {0}".format(env0new)
			print "Env1: {0}".format(env1new)
			print "Env2: {0}".format(env2new)

    else:
		while location != nextstate["loc"]:
			print "Second case"
			clatSystem = client.get_positions()[0][1].lat
			clonSystem = client.get_positions()[0][1].lon
			clatEnv0 = client.get_positions()[1][1].lat
			clonEnv0 = client.get_positions()[1][1].lon
			clatEnv1 = client.get_positions()[2][1].lat
			clonEnv1 = client.get_positions()[2][1].lon
			clatEnv2 = client.get_positions()[3][1].lat
			clonEnv2 = client.get_positions()[3][1].lon
			time.sleep(2)
			location = accuratecoord2state(clatSystem,clonSystem,location)
			env0new = accuratecoord2state(clatEnv0,clonEnv0,env0new,1)
			env1new = accuratecoord2state(clatEnv1,clonEnv1,env1new,1)
			env2new = coord2statesmall(clatEnv2,clonEnv2)
			print "clatSystem: {0}".format(clatSystem)
			print "clatEnv0: {0}".format(clatEnv0)
			print "clatEnv1: {0}".format(clatEnv1)
			print "clatEnv2: {0}".format(clatEnv2)
			print "clonSystem: {0}".format(clonSystem)
			print "clonEnv0: {0}".format(clonEnv0)
			print "clonEnv1: {0}".format(clonEnv1)
			print "clonEnv2: {0}".format(clonEnv2)
			print "System: {0}".format(location)
			print "Env0: {0}".format(env0new)
			print "Env1: {0}".format(env1new)
			print "Env2: {0}".format(env2new)
print 'disconnect'
client.disconnect()

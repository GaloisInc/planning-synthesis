
from pymavlink import mavutil
from MAVProxy.modules.lib import mp_module
from MAVProxy.modules.lib.mp_settings import MPSetting

import socket
import string
import os


class ServerConnection:

    def __init__(self):
        self.conn = None

    def connect(self, inst_id=0):
        self.conn = socket.create_connection( ('localhost', 9001 + inst_id) )
        self.conn.setblocking(0)

    def recv(self):
        try:
            msg = self.conn.recv(1024)

        except socket.error:
            msg = ''

        return msg.rstrip()

    def send_position(self, log, lat, lon, alt, hdg):
        self.conn.sendall('UpdatePosition %0.8f %0.8f %0.8f %0.8f\n' % (lat * 1e-7, lon * 1e-7,
            alt, hdg * 1e-2))

    def send_armed(self):
        self.conn.sendall('Armed\n')

    def send_disarmed(self):
        self.conn.sendall('Disarmed\n')

    def send_ack(self):
        self.conn.sendall('Ack\n')

    def send_pid(self,pid):
        if pid is None:
            self.conn.sendall('Pid Nothing\n' % pid)
        else:
            self.conn.sendall('Pid (Just %d)\n' % pid)


class ProxyModule(mp_module.MPModule):
    '''Connects back to the SITL server'''

    def __init__(self, mpstate):
        super(ProxyModule, self).__init__(mpstate, 'proxy', 'sitl server proxy')

        self.armed = False

        self.conn = None

        self.arming = False
        self.armed  = False

        self.is_controlled = None

        self.add_command('sitl_connect', self.connect, 'Connect to the SITL server')

        self.inst_id = None

    def connect(self,args):
        if len(args) > 0:
            self.inst_id = int(args[0])

        else:
            self.inst_id = int(os.environ['SITL_INST_ID'])

        self.parent_pid = int(os.environ['SITL_PID'])

        self.say('\nConnecting to SITL server as instance %d\n' % self.inst_id)
        self.conn = ServerConnection()
        self.conn.connect(self.inst_id)
        self.conn.send_pid(self.parent_pid)
        self.say('\nConnected to SITL server\n')

    def idle_task(self):

        # ignore messages until we're connected
        if not self.conn: return;

        line = self.conn.recv()
        cmd  = line.split(' ')

        if cmd[0] == 'Arm':
            self.master.wait_gps_fix()
            self.say('\nGot a GPS fix\n')
            self.mpstate.functions.process_stdin('mode guided')
            self.mpstate.functions.process_stdin('rc 3 1000')
            self.mpstate.functions.process_stdin('arm throttle')
            self.arming = True
            self.is_controlled = cmd[1] is 'True'

        elif cmd[0] == 'Disarm':
            self.mpstate.functions.process_stdin('mode stabilize')
            self.mpstate.functions.process_stdin('rc 3 -1')
            self.mpstate.functions.process_stdin('disarm throttle')

        elif cmd[0] == 'Move':
            parsed = cmd[1].split(',')
            lat    = float(parsed[0])
            lon    = float(parsed[1])
            alt    = float(parsed[2])

            self.master.mav.mission_item_send(self.settings.target_system,
                    self.settings.target_component,
                    0,
                    mavutil.mavlink.MAV_FRAME_GLOBAL_RELATIVE_ALT,
                    mavutil.mavlink.MAV_CMD_NAV_WAYPOINT,
                    2, 0, 0, 0, 0, 0,
                    lat, lon, alt)

            self.conn.send_ack()

        elif cmd[0] == 'Waypoints':
            path = string.join(cmd[1:])
            self.mpstate.functions.process_stdin('wp load ' + path)
            self.conn.send_ack()

        elif cmd[0] == 'Auto':
            self.mpstate.functions.process_stdin('mode auto')
            self.conn.send_ack()

        elif line != '':
            self.say('Unhandled command: "%s"' % line)


    def unload(self):
        self.conn.close()

    def unknown_command(self,string):
        return False

    def mavlink_packet(self, packet):
        ty = packet.get_type()

        # send position information back to the SITL server
        if ty == 'GLOBAL_POSITION_INT':
            self.conn.send_position(self, packet.lat, packet.lon,
                    self.status.altitude,
                    packet.hdg)

        elif ty == 'HEARTBEAT':
            armed = self.master.motors_armed()
            if armed and self.arming:
                self.arming = False
                self.armed  = True
                self.mpstate.functions.process_stdin('rc 3 1500')
                self.conn.send_armed()

            elif self.armed and not armed:
                self.armed = False
                self.conn.send_disarmed()

            else:
                # not sure what this state is
                pass

def init(mpstate):
    '''Initialize module'''
    return ProxyModule(mpstate)

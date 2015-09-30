
import json
import socket
import time


class Position:
    '''
    A position, represented as latitude, longitude, and altitude (relative to
    the starting position of the quadcopter, in meters)
    '''
    def __init__(self, lat, lon, alt):
        self.lat = lat
        self.lon = lon
        self.alt = alt

    def __repr__(self):
        return '<%0.8f,%0.8f,%0.8f>' % (self.lat, self.lon, self.alt)

    def __str__(self):
        return self.__repr__()

    @staticmethod
    def from_json(obj):
        return Position(obj['lat'], obj['lon'], obj['alt'])

class Instance:
    def __init__(self, name):
        self.controlled = name is 'controlled'
        self.inst_name  = name

    def __repr__(self):
        return self.inst_name

    @staticmethod
    def from_json(obj):
        return Instance(obj['name'])

class RpcException(Exception):
    def __init__(self,msg):
        super(RpcException, self).__init__(msg)
        self.msg = msg

class Rpc:
    '''
    Performs RPC over a TCP connection
    '''
    def __init__(self, host, port):
        self.conn = socket.create_connection( (host,port) )
        self.io   = self.conn.makefile()

    def request(self, method, params=None):
        req = json.dumps( { 'method' : method, 'params' : params } )
        self.io.write('%s\n' % req)
        self.io.flush()

        resp = ''
        while True:
            msg   = self.io.readline(4096)
            resp += msg

            if len(msg) < 1024: break

        obj = json.loads(resp)
        if 'error' not in obj:
            return obj['result']

        else:
            raise RcpException(obj.error), 'method call to %s failed' % method

    def disconnect(self):
        self.io.close()
        self.conn.close()

class SITLException(Exception):
    def __init__(self,msg):
        super(SITLException,msg)
        self.msg = msg

class SitlClient:

    def __init__(self):
        self.conn = None

    def connect(self, host='localhost', port=31320):
        """
        Makes a connection to the SITL server, and waits for the associated
        quadcopter to be armed.
        """
        self.conn = Rpc(host, port)

    def disconnect(self):
        """
        Disconnect from the server, resetting the connected drone.
        """
        self.conn.disconnect()

    def move_to(self, position):
        """
        Moves the controlled drone to the Position instance given
        """
        self.conn.request('move-to', position.__dict__)

    def get_positions(self):
        """
        Returns an array of pairs of obstacle id's, and Position instances
        """
        resp = self.conn.request('positions')
        return SitlClient.parse_positions(resp['positions'])

    def start_obstacles(self):
        """
        Starts any dynamic obstacles in the system
        """
        self.conn.request('start-obstacles')

    @staticmethod
    def parse_positions(positions):
        parsed = []

        for result in positions:
            parsed.append((Instance.from_json(result['instance']),
                    Position.from_json(result['position'])))

        return parsed


if __name__ == '__main__':
    client = SitlClient()

    print 'connecting'
    client.connect()

    print 'connected'
    ps = client.get_positions()
    for x in range(1,2):
        print client.get_positions()
        time.sleep(1)

    print 'start_obstacles'
    client.start_obstacles()

    print 'move_to'
    client.move_to(Position(-35.36304470,149.16559820,50.0))
    for x in range(1,60):
        print client.get_positions()
        time.sleep(1)

    print 'disconnect'
    client.disconnect()

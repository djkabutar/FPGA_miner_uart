from ctypes import cdll, c_ubyte, c_uint32, POINTER
from codecs import decode
from bitcoinrpc.authproxy import AuthServiceProxy, JSONRPCException
from pprint import pprint

user = "djkabutar"
password = "Devang110402"
# host = "api2.bitcoin.cz"
# host = "stratum.braiins.com"
host = "192.168.29.43"
http_port = "8332"

serial_port = "/dev/ttyUSB0"

askrate = 5
flag = 1

###############################################################################

from time import ctime, sleep, time
from json import loads, dumps
from serial import Serial
from threading import Thread, Event
from queue import Queue
from base64 import b64encode
from urllib.parse import urlparse
import binascii, struct

def stats(count, starttime):
    # 2**32 hashes per share (difficulty 1)
    mhshare = 4294.967296

    s = sum(count)
    tdelta = time() - starttime
    rate = s * mhshare / tdelta

    # This is only a rough estimate of the true hash rate,
    # particularly when the number of events is low. However, since
    # the events follow a Poisson distribution, we can estimate the
    # standard deviation (sqrt(n) for n events). Thus we get some idea
    # on how rough an estimate this is.

    # s should always be positive when this function is called, but
    # checking for robustness anyway
    if s > 0:
        stddev = rate / s**0.5
    else:
        stddev = 0

    return "[%i accepted, %i failed, %.2f +/- %.2f Mhash/s]" % (count[0], count[1], rate, stddev)

class GetWork():
    def __init__(self):
        self.rpc_connection = AuthServiceProxy("http://%s:%s@%s:%s"%(user, password, host, http_port))
    
    def getwork(self, *args):
        if not args:
            return self.rpc_connection.batch_([["getwork"]])[0]
        else:
            return self.rpc_connection.batch_([["getwork", args, user]])[0]

class Reader(Thread):
    def __init__(self):
        Thread.__init__(self)

        self.daemon = True

        # flush the input buffer
        ser.read(1000)

    def run(self):
        global flag
        while True:
            # pass
            nonce = ser.read(4)

            if len(nonce) == 4:
                flag = 0
                # Keep this order, because writer.block will be
                # updated due to the golden event.
                submitter = Submitter(writer.block, nonce)
                submitter.start()
                golden.set()


class Writer(Thread):
    def __init__(self):
        Thread.__init__(self)

        # Keep something sensible available while waiting for the
        # first getwork
        self.block = "0" * 256
        self.midstate = "0" * 64

        self.daemon = True

    def run(self):
        global flag
        while True:
            try:
                work = bitcoin.getwork()
                self.block = work['data']
                self.midstate = work['midstate']
                flag = 1
            except Exception as e:
                print("RPC getwork error: " + str(e))
                # In this case, keep crunching with the old data. It will get 
                # stale at some point, but it's better than doing nothing.

            # Just a reminder of how Python slices work in reverse
            #rdata = self.block.decode('hex')[::-1]
            #rdata2 = rdata[32:64]
            rdata2 = decode(self.block[95:63:-1], 'hex')

            rmid = decode(self.midstate[::-1], 'hex')
            
            payload = rmid + rdata2

            print(binascii.hexlify(payload[::-1]))
            
            ser.write(payload[::-1])
            
            result = golden.wait(askrate)

            if result:
                golden.clear()

            # while flag:
                # pass

class Submitter(Thread):
    def __init__(self, block, nonce):
        Thread.__init__(self)

        self.block = block
        self.nonce = nonce

    def run(self):
        # This thread will be created upon every submit, as they may
        # come in sooner than the submits finish.

        print("Block found on " + ctime())

        hrnonce = int.from_bytes(self.nonce, 'big')

        print(hex(hrnonce)[2:])

        data = self.block[:152] + str(hex(hrnonce)[2:]) + self.block[160:]

        try:
            result = bitcoin.getwork(data)
            print("Upstream result: " + str(result))
        except Exception as e:
            print("RPC send error: ", e)
            # a sensible boolean for stats
            result = False

        results_queue.put(result)

class Display_stats(Thread):
    def __init__(self):
        Thread.__init__(self)

        self.count = [0, 0]
        self.starttime = time()
        self.daemon = True

        print("Miner started on " + ctime())

    def run(self):
        while True:
            result = results_queue.get()
            
            if result:
                self.count[0] += 1
            else:
                self.count[1] += 1
                
            print(stats(self.count, self.starttime))
                
            results_queue.task_done()

golden = Event()

bitcoin = GetWork()

# headers = {"User-Agent": "poclbm/12.0", "Authorization": "Basic " + b64encode(b"djkabutar:Devang110402").decode('ascii'), "X-Mining-Extensions": 'hostlist midstate rollntime'}

# connection = httplib2.HTTPConnectionWithTimeout(host, 8332, timeout=5)
# postdata = {"method": "getwork", "params": [], "id":0}
# postdata['params'] = [None] if None else []
# connection.request('GET', '/', dumps(postdata), headers=headers)

# response = connection.getresponse()
# if response.status == httplib.UNAUTHORIZED:
#     print("Authentication failed")

# r = 3
# while response.status == httplib.TEMPORARY_REDIRECT:
#     response.read()
#     url = response.getheader('Location', '')
#     if r == 0 or url == '': raise HTTPException('Too much or bad redirects')
#     connection.request('GET', url, headers=headers)
#     response = connection.getresponse()
#     print(loads(response.read()))
#     r = r - 1

# connection.close()

results_queue = Queue()

ser = Serial(serial_port, 2000000, timeout=askrate)

reader = Reader()
writer = Writer()
disp = Display_stats()

reader.start()
writer.start()
disp.start()

try:
    while True:
        # Threads are generally hard to interrupt. So they are left
        # running as daemons, and we do something simple here that can
        # be easily terminated to bring down the entire script.
        sleep(10000)
except KeyboardInterrupt:
    print("Terminated")

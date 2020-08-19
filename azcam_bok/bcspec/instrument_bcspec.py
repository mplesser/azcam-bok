# Contains BCSpecInstrument class for the Bok B&C spectrograph.

import socket
import json
import time

import azcam
from azcam.utils import *
from azcam.instruments.instrument import Instrument


class BCSpecInstrument(Instrument):
    """
    The interface to the BCSpec spectrograph instrument at Bok.
    The InstrumentServer is J. Fookson's Ruby server for the Opto22.
    """

    # Valid lamp names
    Lamps = ["NEON", "CONT", "UV", "HE/AR", "FE/NE", "UNDEF", "MIRROR", "SPARE"]

    def __init__(self):

        super().__init__()

        self.use_bokpop = 1

        self.Name = "BCSpec"
        self.Host = "10.30.1.2"  # IP address for instrument server on bokap3
        self.Port = 9875
        self.ActiveComps = [""]
        self.enabled = 1

        # opto22 server interface
        self.Iserver = InstrumentServerInterface(self.Host, self.Port, self.Name)

        # bokpop
        self.bokpop = BokData("10.30.1.3", 5554, 1.0)  # 1 sec timeout

        # add keywords
        reply = self.define_keywords()

    def command(self, Command):
        """
        Command interface for BCSpec instrument.
        """

        if not self.initialized:
            self.initialize()

        reply = self.Iserver.open()
        if reply[0] == "OK":
            reply = self.Iserver.recv()[1]  # read string and ignore for now
            self.Iserver.send(Command, "")  # no terminator
            reply = self.Iserver.recv()[1]
            self.Iserver.send("CLIENTDONE", "")
            reply1 = self.Iserver.recv()[1]  # read string and ignore for now
            self.Iserver.close()
        else:
            return reply

        # check for error, valid replies starts with 'OK: ' and errors with '?: '
        if reply.startswith("OK"):
            # reply=reply[4:]
            return reply
        else:
            raise azcam.AzcamError(reply)

    def initialize(self):
        """
        Initialize OPTO22.
        """

        cmd = "INITOPTO"
        self.initialized = 1
        reply = self.command(cmd)

        return reply

    def test(self):
        """
        Test the B&C lamps.
        """

        lamps = self.get_all_comps()[1:]  # ignore OK
        for lamp in lamps:
            print("%s ON" % lamp)
            self.set_active_comps(lamp)
            reply = self.comps_on()
            time.sleep(1)
            reply = self.comps_off()
            print("%s OFF" % lamp)
            time.sleep(1)

        return

    # *** comparisons ***

    def comps_delay(self, DelayTime=0):
        """
        Delay for lamp warmup.
        If DelayTime==0, use internal fixed delays based on ActiveComps.
        """

        if DelayTime == 0:
            if "FE/NE" in self.ActiveComps:
                delay = 10
            else:
                delay = 2
        else:
            delay = DelayTime

        time.sleep(delay)

        return

    def get_all_comps(self, CompID=0):
        comps = list(self.Lamps)
        comps.append("HE/AR/NE")
        return comps

    def get_active_comps(self, CompID=0):
        comps = list(self.ActiveComps)
        return comps

    def set_active_comps(self, CompNames=[], CompTypeID=0):

        comps = []
        if type(CompNames) == list:
            for lamp in CompNames:
                if lamp == "HE/AR/NE":
                    comps.append("HE/AR")
                    comps.append("NEON")
                else:
                    comps.append(lamp.strip("'\""))
        elif CompNames == "HE/AR/NE":
            comps = ["HE/AR", "NEON"]
        else:
            comps.append(CompNames.strip("'\""))

        self.ActiveComps = comps

        return

    def comps_on(self, CompID=0):
        """
        Turn Comps on.
        """

        if not self.enabled:
            return

        # so we want this?
        # reply=self.lamps_off_all()

        for lamp in self.ActiveComps:
            reply = self.lamp_on(lamp)

        return

    def comps_off(self, CompID=0):
        """
        Turn Comps off.
        """

        if not self.enabled:
            return

        # reply=self.lamps_off_all()

        for lamp in self.ActiveComps:
            reply = self.lamp_off(lamp)

        return

    def lamp_on(self, LampName):
        """
        Turn a lamp on.
        """

        if not self.enabled:
            return

        if not LampName.upper() in self.Lamps and LampName.upper() != "HE/AR/NE":
            raise azcam.AzcamError(f"Invalid lamp name: {LampName}")

        if LampName.upper() == "FE/NE":
            for i in range(2):
                cmd = "ONLAMP " + LampName.upper()
                reply = self.command(cmd)
                cmd = "OFFLAMP " + LampName.upper()
                reply = self.command(cmd)
            cmd = "ONLAMP " + LampName.upper()
            reply = self.command(cmd)
        else:
            if LampName.upper() == "HE/AR/NE":
                cmd = "ONLAMP HE/AR"
                reply = self.command(cmd)
                cmd = "ONLAMP NEON"
                reply = self.command(cmd)
            else:
                cmd = "ONLAMP " + LampName.upper()
                reply = self.command(cmd)

        return

    def lamp_off(self, LampName):
        """
        Turn a lamp off.
        """

        if not self.enabled:
            return

        if not LampName.upper() in self.Lamps and LampName.upper() != "HE/AR/NE":
            raise azcam.AzcamError(f"Invalid lamp name: {LampName}")

        if LampName.upper() == "HE/AR/NE":
            cmd = "OFFLAMP HE/AR"
            reply = self.command(cmd)
            cmd = "OFFLAMP NEON"
            reply = self.command(cmd)
        else:
            cmd = "OFFLAMP " + LampName.upper()
            reply = self.command(cmd)

        return

    def lamps_off_all(self):
        """
        Turn all lamps off.
        """

        # cmd='OFFALL'
        # reply=self.command(cmd)

        for lamp in self.Lamps:
            reply = self.lamp_off(lamp)

        return

    def read_keyword(self, Keyword):
        """
        Read an instrument keyword value.
        This command will read hardware to obtain the keyword value.
        """

        try:
            reply = self.header.values[Keyword]
        except:
            raise azcam.AzcamError(f"Keyword {Keyword} not defined")

        # store value in Header
        self.header.set_keyword(Keyword, reply)

        # convert type
        if self.header.typestrings[Keyword] == "int":
            reply = int(reply)
        elif self.header.typestrings[Keyword] == "float":
            reply = float(reply)

        t = self.header.get_type_string(self.header.typestrings[Keyword])

        return [reply, self.header.comments[Keyword], t]

    def read_header(self):
        """
        Reads, records, and returns the current header.
        This method looks up all keywords and queries hardware for the current value of each keyword.
        Returns [Header[]]: Each element Header[i] contains the sublist (keyword, value, comment, and type).
        Example: Header[2][1] is the value of keyword 2 and Header[2][3] is its type.
        Type is one of 'str', 'int', 'float', or 'complex'.
        """

        header = []
        reply = self.header.get_all_keywords()

        for key in reply:
            reply = self.read_keyword(key)
            list1 = [key, reply[0], reply[1], reply[2]]
            header.append(list1)

        # get infrastructure header info
        reply = self.get_info()

        return header

    # *** INFRASTRUCTURE ***

    def get_info(self):
        """
        Get infrastructure info from servers running on bokpct.
        These are temperatures, humidity, dewpoints, and weather.
        """

        if self.use_bokpop:
            reply = self.get_bokpop_info()

        return reply

    def get_bokpop_info(self):
        """
        Get info from bokpop server.
        """

        bokpopdata = self.bokpop.makeHeader()

        for item in bokpopdata:
            keyword = item[0]
            value = item[1]
            comment = item[2]
            self.header.set_keyword(keyword, value, comment, str)

        return bokpopdata


# *** instrument server interface ***


class InstrumentServerInterface(object):
    """
    Defines the InstrumentServerInterface class.
    Communicates with an instrument server using an ethernet socket.
    """

    Host = ""  # instrument server host
    Port = 0  # instrument server port
    Timeout = 5.0  # socket timeout in seconds
    OK = "OK"
    ERROR = "ERROR"

    def __init__(self, Host, Port, Name=""):

        self.Host = Host
        self.Port = Port
        self.Name = Name

    def open(self, Host="", Port=-1):
        """
        Open a socket connection to an instrument.
        Creates the socket and makes a connection.
        @return AzCamStatus
        """

        if Host != "":
            self.Host = Host
        if Port != -1:
            self.Port = Port

        self.Socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.Socket.settimeout(float(self.Timeout))
        try:
            reply = self.Socket.connect((self.Host, self.Port))
            return ["OK"]
        except:
            self.close()
            return ["ERROR", "%s not opened" % self.Name]

    def close(self):
        """
        Close an open socket connection to an instrument.
        """

        try:
            self.Socket.close()
        except:
            pass

        return ["OK"]

    def command(self, Command, Terminator="\r\n"):
        """
        Communicte with the remote instrument server.
        Opens and closes the socket each time.
        Returns the exact reply from the server.
        """

        reply = self.open()
        if reply[0] == "OK":
            reply = self.send(Command, Terminator)
            if reply[0] == "OK":
                reply = self.recv(-1, "\n")

        return reply

    def send(self, Command, Terminator="\r\n"):
        """
        send a command string to a socket instrument.
        Appends the terminator.
        """

        try:
            self.Socket.send(str.encode(Command + Terminator))  # send command with terminator
            return ["OK"]
        except:
            self.close()
            return ["ERROR", "could not send command to %s" % self.Name]

    def recv(self, Length=-1, Terminator="\n"):
        """
        Receives a reply from a socket instrument.
        Terminates the socket read when Length bytes are received or when the Terminator is received.
        @param Length is the number of bytes to receive.  -1 means receive through Terminator character.
        @param Terminator is the terminator character.
        """

        if Length == -2:
            try:
                self.Socket.settimeout(3)
                msg = self.Socket.recv(1024).decode()
                self.Socket.settimeout(self.Timeout)
                return ["OK", msg]
            except:
                return ["OK", ""]

        # receive Length bytes
        if Length != -1:
            msg = self.Socket.recv(Length).decode()
            return ["OK", msg]

        # receive with terminator
        msg = chunk = ""
        loop = 0
        while chunk != Terminator:  # CR LF is usually '\n' when translated
            try:
                chunk = self.Socket.recv(1).decode()
            except Exception as errorcode:
                self.close()
                return ["ERROR", "%s communication problem" % self.Name]
            if chunk != "":
                msg = msg + chunk
                loop = 0
            else:
                loop += 1
                if loop > 10:
                    return ["ERROR", "%s server communication loop timeout" % self.Name]

        Reply = msg[:-2]  # remove CR/LF
        if Reply == None:
            Reply = ""
        return ["OK", Reply]


class BokData(socket.socket):
    # Class to retreive data from the bok server on bokpop

    # lookup table for fits keywords and descriptions with bokserv keywords
    keyword_header_map = (
        # Weather
        ("udome_dewpoint", "UDOMEDP", "Upper Dome Dewpoint [F]"),
        ("udome_temp", "UDOMET", "Upper Dome Temp [F]"),
        ("udome_humid", "UDOMEH", "Upper Dome Humidity [%]"),
        ("indewpoint", "INDP", "Inside Dewpoint [F]"),
        ("inhumid", "INH", "Inside Humidity [%]"),
        ("intemp", "INT", "Inside Temperature [F]"),
        ("outdewpoint", "OUTDP", "[F]"),
        ("outhumid", "OUTH", "[%]"),
        ("outtemp", "OUTT", "[F]"),
        ("mcell_dewpoint", "MCELLDP", "Mirror Cell Dewpoint"),
        ("mcell_humid", "MCELLH", "Mirror Cell Humidity [%]"),
        ("mcell_temp", "MCELLT", "Mirror Cell Temperature [F]"),
        ("wind_speed", "WSPEED", "Wind Speed [MPH]"),
        ("wind_direction", "WDIR", "Wind Direction [Degrees]"),
        # Telemetry
        ("airmass", "AIRMASS", "Airmass"),
        ("azimuth", "AZIMUTH", "Tel Azimuth [Degrees]"),
        ("declination", "DEC", "Telescope Declination"),
        ("dome", "DOME", "Dome pos [degrees]"),
        ("elevation", "ELEVAT", "Tel Elevation [degrees]"),
        ("epoch", "EPOCH", "Coordinate Epoch"),
        ("hour_angle", "HA", "Tel Hour Angle"),
        ("iis", "ROT", "Rotator Position"),
        ("julian_date", "JD", "Julian Date"),
        ("motion", "MOT", "Motion bits"),
        ("right_ascension", "RA", "Tel Right Ascension"),
        ("sidereal_time", "LST-OBS", "Local Sidereal Time"),
        ("universal_time", "UTC", "Universal Time"),
        ("wobble", "WOB", "Wobble"),
    )

    def __init__(self, HOST="10.30.1.3", PORT=5554, timeout=1.0):

        # self.timeout=timeout
        self.host = HOST
        self.port = PORT

        self.kwmap = self.keyword_header_map

        socket.socket.__init__(self, socket.AF_INET, socket.SOCK_STREAM)

    def listen(self):
        # listen for incomming socket data
        test = True
        resp = ""
        while test:
            try:
                newStuff = self.recv(100).decode()
            except socket.timeout:
                return resp

            if newStuff:
                resp += newStuff
            else:
                return resp

    def converse(self, message):
        # send socket data and then listen for a response
        self.send(str.encode(message))
        return self.listen()

    def getAll(self):
        # retrieve all information from the bokpop server
        self.data = self.converse("all\n")
        # convert to python dict type and return.
        return json.loads(self.data)

    def putHeader(self, fitsfd):
        # use the kwmap to put in fits
        # header keywords, values and
        # descriptions in to the fits
        # header.
        all_data = self.getAll()
        for Map in self.kwmap:
            kw, fitskw, descr = Map
            val = self.extract(all_data, kw)
            print(kw, val, descr)
            try:
                # lazily look for numbers
                val = float(val)
            except ValueError:
                pass

            fitsfd[0].header[fitskw] = (val, descr)

    def makeHeader(self):
        # use the kwmap to put in fits
        # header keywords, values and
        # descriptions in to the fits
        # header.
        # all_data = self.getAll()

        all_data = self.get_header()
        header = []
        for Map in self.kwmap:
            kw, fitskw, descr = Map
            val = self.extract(all_data, kw)
            try:
                # lazily look for numbers
                val = float(val)
            except ValueError:
                pass
            except:
                pass

            keyword1 = fitskw
            value1 = val
            comment1 = '"' + descr + '"'

            header.append([keyword1, value1, comment1])

            if keyword1 == "LST-OBS":
                header.append(["ST", value1, '"local siderial time"'])

        return header

    def extract(self, pyDict, keyword):
        # Extract fits header data from bokserver
        # using the kwmap
        for key, val in pyDict.items():
            if type(val) == dict:
                resp = self.extract(val, keyword)
                if resp != None:
                    return resp
            else:
                if key == keyword:
                    return val

    def get_header(self):
        """
        Added for AzCam
        """

        # open socket
        socket.socket.__init__(self, socket.AF_INET, socket.SOCK_STREAM)
        if self.timeout:
            self.settimeout(self.timeout)
        HOST = socket.gethostbyname(self.host)
        self.connect((HOST, int(self.port)))

        # get data
        reply = self.getAll()

        # output
        return reply

    def update_header(self):
        """
        Update instrument header.
        """

        data = self.makeHeader()


"""
example of data

{u'upper_dome': {u'udome_humid': 58.51, u'timestamp': u'2016-08-31 16:32:13.432139+00:00', u'udome_dewpoint': 53.15, u'udome_temp': 68.27},
u'dome_state': {u'timestamp': u'2016-08-31 16:32:20.489121+00:00', u'dome_open': 0.0, u'dome_closed': 1.0}, 
u'tube_temps': {u'ninety_prime': -994.0, u'north_strut': 66.3, u'primary_mirror': 66.7, u'timestamp': u'2016-08-31 16:32:20.276327+00:00'}, 
u'iis': {u'timestamp': u'2016-08-31 16:32:20.542472+00:00', u'iispos': -994}, 
u'mirror_cell': {u'timestamp': u'2016-08-31 16:32:14.713395+00:00', u'mcell_dewpoint': 53.78, u'mcell_humid': 61.23, u'mcell_temp': 67.55}, 
u'wind': {u'wind_speed': 7.15, u'wind_gust_direction': 133.65, u'wind_direction': 106.98, u'wind_gust': 13.02, u'timestamp': u'2016-08-31 16:32:20.483598+00:00'}, 
u'telem': {u'airmass': u'1.00', u'elevation': u'90.0', u'iis': u'0.0', u'sidereal_time': u'07:46:55', u'wobble': u'1', u'right_ascension': u'07:45:54.08', u'dome': u'-45.7', u'motion': u'0', u'epoch': u'HD2000.000', u'declination': u'+31:50:00.0', u'hour_angle': u'-00:00:00', u'timestamp': u'2016-08-31 16:32:19.830680+00:00', u'azimuth': u'+40.2', u'universal_time': u'16:32:13.2', u'julian_date': u'2457632.1'}, 
u'inside_outside': {u'outdewpoint': 57.74, u'outhumid': 58.47, u'timestamp': u'2016-08-31 16:32:20.195288+00:00', u'outtemp': 73.22, u'indewpoint': 53.96, u'inhumid': 47.36, u'intemp': 75.38}
}
"""

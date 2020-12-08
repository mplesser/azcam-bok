# Contains the BokTCS class which defines the Bok telescope interface.

import os
import socket
import sys
import time

import azcam
from azcam.header import Header
from azcam.telescope import Telescope


class BokTCS(Telescope):
    """
    The interface to the Steward Observatory Bok TCS telescope server.
    """

    def __init__(self, obj_id="telescope", obj_name="Bok telescope"):

        super().__init__(obj_id, obj_name)

        # telescope header object
        # self.header = Header("Telescope")
        self.use_bokpop = 0
        self.DEBUG = 0

    def initialize(self):
        """
        Initializes the telescope interface.
        """

        if self.initialized:
            return

        if not self.enabled:
            azcam.AzcamWarning(f"{self.name} is not enabled")
            return

        # do not write telescope header with bokpop as it is in 'instrument'
        if self.use_bokpop:
            azcam.db.headerorder.remove("telescope")

        # telescope server interface
        self.Tserver = TelcomServerInterface()

        # add keywords
        self.define_keywords()

        self.initialized = 1

        return

    # **************************************************************************************************
    # exposure
    # **************************************************************************************************

    def exposure_start(self):
        """
        Setup before exposure starts.
        """

        return

    def exposure_finish(self):
        """
        Setup before exposure starts.
        """

        return

    # **************************************************************************************************
    # Keywords
    # **************************************************************************************************

    def define_keywords(self):
        """
        Defines telescope keywords to telescope, if they are not already defined.
        """

        if len(self.Tserver.keywords) == 0:
            return

        # add keywords to header
        for key in self.Tserver.keywords:
            self.header.keywords[key] = self.Tserver.keywords[key]
            self.header.comments[key] = self.Tserver.comments[key]
            self.header.typestrings[key] = self.Tserver.typestrings[key]

        return

    def get_keyword(self, keyword):
        """
        Reads an telescope keyword value.
        Keyword is the name of the keyword to be read.
        This command will read hardware to obtain the keyword value.
        """

        if not self.enabled:
            azcam.AzcamWarning("telescope not enabled")
            return

        try:
            command = self.Tserver.make_packet("REQUEST " + self.Tserver.keywords[keyword])
        except KeyError:
            raise azcam.AzcamError(f"Keyword {keyword} not defined")

        ReplyLength = self.Tserver.ReplyLengths[keyword]
        reply = self.Tserver.command(command, ReplyLength + self.Tserver.Offset)
        if reply[0] != "OK":
            self.header.set_keyword(keyword, "")
            return reply
        reply = self.Tserver.parse_reply(reply[1], ReplyLength)
        # reply=reply.lstrip()
        # reply=reply.rstrip()

        # parse RA and DEC specially
        if keyword == "RA":
            reply = "%s:%s:%s" % (reply[0:2], reply[2:4], reply[4:])
        elif keyword == "DEC":
            reply = "%s:%s:%s" % (reply[0:3], reply[3:5], reply[5:])
        else:
            pass

        # store value in Header
        self.header.set_keyword(keyword, reply)

        reply, t = self.header.convert_type(reply, self.header.typestrings[keyword])

        return [reply, self.Tserver.comments[keyword], t]

    def read_header(self):
        """
        Returns telescope header info.
        returns [Header[]]: Each element Header[i] contains the sublist (keyword, value, comment, and type).
        Example: Header[2][1] is the value of keyword 2 and Header[2][3] is its type.
        Type is one of str, int, or float.
        """

        if not self.enabled:
            azcam.AzcamWarning("telescope not enabled")
            return

        header = []

        cmd = self.Tserver.make_packet("REQUEST ALL")
        l = (
            len(self.Tserver.TELID) + len(self.Tserver.SYSID) + len(self.Tserver.PID)
        )  # get one telemetry string
        h = self.Tserver.command(cmd, 151 + l)
        if h[0] != "OK":
            return h
        h = h[1][l + 1 :]  # strip header stuff

        for key in self.header.get_all_keywords():
            t = self.header.get_type_string(self.Tserver.typestrings[key])
            list1 = [
                key,
                self.Tserver.parse_keyword(key, h)[1],
                self.Tserver.comments[key],
                t,
            ]
            header.append(list1)
            # store value in Header
            self.header.set_keyword(list1[0], list1[1], list1[2], list1[3])

        return header

    def update_header(self):
        """
        Update headers, reading current data.
        """

        # delete all keywords if not enabled
        if not self.enabled:
            self.header.delete_all_keywords()
            return

        if self.use_bokpop:
            return

        self.define_keywords()
        reply = self.read_header()

        return

    # **************************************************************************************************
    # Move
    # **************************************************************************************************

    def offset(self, RA, Dec):
        """
        Offsets telescope in arcsecs.
        """

        if not self.enabled:
            azcam.AzcamWarning("telescope not enabled")
            return

        command = self.Tserver.make_packet("RADECGUIDE %s %s" % (RA, Dec))

        replylen = 1024
        reply = self.Tserver.command(command, replylen)

        # wait for motion to stop
        reply = self.wait_for_move()

        return reply

    def move(self, RA, Dec, Epoch=2000.0):
        """
        Moves telescope to an absolute RA,DEC position.

        Do not use colons in coordinates.
        """

        if not self.enabled:
            azcam.AzcamWarning("telescope not enabled")
            return

        if self.DEBUG == 1:
            return

        replylen = 1024

        command = "EPOCH %s" % Epoch
        command = self.Tserver.make_packet(command)
        reply = self.Tserver.command(command, replylen)
        command = "NEXTRA %s" % RA
        command = self.Tserver.make_packet(command)
        reply = self.Tserver.command(command, replylen)
        command = "NEXTDEC %s" % Dec
        command = self.Tserver.make_packet(command)
        reply = self.Tserver.command(command, replylen)

        command = "MOVNEXT"
        command = self.Tserver.make_packet(command)
        reply = self.Tserver.command(command, replylen)

        # wait for motion to START
        time.sleep(0.5)

        # wait for motion to stop
        self.wait_for_move()

        return

    def move_start(self, RA, Dec, Epoch=2000.0):
        """
        Moves telescope to an absolute RA,DEC position without waiting for motion to stop.

        Do not use colons in coordinates.
        """

        azcam.log("move_start command received:%s %s" % (RA, Dec))

        if not self.enabled:
            azcam.AzcamWarning("telescope not enabled")
            return

        if self.DEBUG == 1:
            return

        replylen = 1024

        command = "EPOCH %s" % Epoch
        command = self.Tserver.make_packet(command)
        reply = self.Tserver.command(command, replylen)
        command = "NEXTRA %s" % RA
        command = self.Tserver.make_packet(command)
        reply = self.Tserver.command(command, replylen)
        command = "NEXTDEC %s" % Dec
        command = self.Tserver.make_packet(command)
        reply = self.Tserver.command(command, replylen)

        command = "MOVNEXT"
        command = self.Tserver.make_packet(command)
        reply = self.Tserver.command(command, replylen)

        return

    def wait_for_move(self):
        """
        Wait for telescope to stop moving.
        """

        if not self.enabled:
            azcam.AzcamWarning("telescope not enabled")
            return

        if self.DEBUG == 1:
            return

        # loop without timeout
        azcam.log("Checking for telescope motion...")
        cycle = 0
        while True:
            reply = self.get_keyword("MOTION")
            try:
                motion = int(reply[0])
            except:
                raise azcam.AzcamError("bad MOTION status keyword: %s" % reply)

            if not motion:
                azcam.log("Telescope reports it is STOPPED")
                azcam.log("Coords:", self.get_keyword("RA"), self.get_keyword("DEC"))
                return
            else:
                azcam.log("Coords:", self.get_keyword("RA"), self.get_keyword("DEC"))

            time.sleep(0.1)
            cycle += 1  # not used for now

        # stop the telescope
        azcam.log("Telescope motion TIMEOUT - sending CANCEL")
        command = "CANCEL"
        command = self.Tserver.make_packet(command)
        reply = self.Tserver.command(command, 1024)

        raise azcam.AzcamError("STOPPED motion flag not detected")


class TelcomServerInterface(object):

    Host = ""
    Port = 0
    Socket = 0

    TELID = ""  # Telescope ID
    SYSID = "TCS"  # Subsystem ID
    PID = "001"  # Packet ID

    # the value of the keyword is the string used by TCS
    keywords = {
        "RA": "RA",
        "DEC": "DEC",
        "AIRMASS": "SECZ",
        "HA": "HA",
        "LST-OBS": "ST",
        "EQUINOX": "EQ",
        "JULIAN": "JD",
        "ELEVAT": "EL",
        "AZIMUTH": "AZ",
        "ROTANGLE": "ROT",
        "ST": "ST",
        "EPOCH": "EQ",
        "MOTION": "MOTION",
    }
    comments = {
        "RA": "right ascension",
        "DEC": "declination",
        "AIRMASS": "airmass",
        "HA": "hour angle",
        "LST-OBS": "local siderial time",
        "EQUINOX": "equinox of RA and DEC",
        "JULIAN": "julian date",
        "ELEVAT": "elevation",
        "AZIMUTH": "azimuth",
        "MOTION": "telescope motion flag",
        "ROTANGLE": "IIS rotation angle",
        "ST": "local siderial time",
        "EPOCH": "equinox of RA and DEC",
        "MOTION": "motion flag",
    }
    typestrings = {
        "RA": str,
        "DEC": str,
        "AIRMASS": float,
        "HA": str,
        "LST-OBS": str,
        "EQUINOX": float,
        "JULIAN": float,
        "ELEVAT": float,
        "AZIMUTH": float,
        "MOTION": int,
        "BEAM": int,
        "ROTANGLE": float,
        "ST": str,
        "EPOCH": float,
    }
    # ReplyLengths={'RA':9,'DEC':9,'AIRMASS':5,'HA':9,'LST-OBS':8,'EQUINOX':7,
    #      'JULIAN':10,'ELEVAT':5,'AZIMUTH':6,'MOTION':1,'ROTANGLE':5,'ST':8,'EPOCH':7}
    ReplyLengths = {
        "RA": 9,
        "DEC": 9,
        "AIRMASS": 5,
        "HA": 9,
        "LST-OBS": 8,
        "EQUINOX": 7,
        "JULIAN": 9,
        "ELEVAT": 5,
        "AZIMUTH": 6,
        "MOTION": 1,
        "ROTANGLE": 5,
        "ST": 8,
        "EPOCH": 7,
    }
    Offsets = {
        "RA": 4,
        "DEC": 14,
        "AIRMASS": 57,
        "HA": 25,
        "LST-OBS": 35,
        "EQUINOX": 76,
        "JULIAN": 85,
        "ELEVAT": 44,
        "AZIMUTH": 50,
        "MOTION": 1,
        "ROTANGLE": 129,
        "ST": 35,
        "EPOCH": 76,
    }

    def __init__(self):
        """
        Initialize communication interface to telescope server.
        """

        name = "bok"

        telname = name.lower()
        if telname == "bok":
            self.Host = "10.30.3.42"
            self.Port = 5750
            self.TELID = "BOK"
            self.Offset = 10
        else:
            azcam.AzcamError(f"ERROR bad telescope name: {name}")

        return

    def open(self, Host="", Port=-1):
        """
        Opens a connection (socket) to the telescope server.
        Creates the socket and connects.
        """
        if Host != "":
            self.Host = Host
        if Port != -1:
            self.Port = Port

        self.Socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.Socket.settimeout(5.0)
        try:
            self.Socket.connect((self.Host, self.Port))
            return
        except Exception as inst:
            raise azcam.AzcamError("could not open telescope server socket")

    def close(self):
        """
        Closes an open connection (socket) to a telescope server.
        """
        try:
            self.Socket.close()
        except:
            pass

    def command(self, command, ReplyLength):
        """
        Sends a command to the telescope server and receives the reply.
        Opens and closes the socket each time.
        """

        self.open()
        self.send(command)
        reply = self.recv(ReplyLength)
        self.close()

        return reply

    def send(self, command):
        """
        Sends a command to a socket telescope.
        Appends CRLF to command.
        """

        reply = self.Socket.send(str.encode(command + "\r\n"))  # send command with terminator

    def recv(self, Length):
        """
        Receives a reply from a socket telescope.
        """

        try:
            msg = self.Socket.recv(Length)
            if msg[-2] == 255:  # funny \xff\n at end of REQUEST ALL data
                msg = msg[:-2]
            msg = msg.decode()
            return ["OK", msg]
        except Exception as inst:
            msg = chunk = ""
            return ["ERROR", "telescope server read error: %s" % inst]

    def make_packet(self, command):
        """
        Internal Use Only.<br>
        Makes a telemetry packet for transmission to the telescope server.
        """

        # packetlist = [self.TELID,self.SYSID,self.PID,'REQUEST',command]
        packetlist = [self.TELID, self.SYSID, self.PID, command]
        packet = " ".join(packetlist)
        return packet

    def parse_keyword(self, keyword, telemetry):
        """
        Returns a telescope telemetry keyword value from the telemetry string.
        Data returned may be of type string, integer, or float.
        """

        ReplyLength = self.ReplyLengths[keyword]
        reply = telemetry[self.Offsets[keyword] - 1 : self.Offsets[keyword] + ReplyLength]

        # parse RA and DEC specially
        if keyword == "RA":
            reply = "%s:%s:%s" % (reply[0:2], reply[2:4], reply[4:])
        elif keyword == "DEC":
            reply = "%s:%s:%s" % (reply[0:3], reply[3:5], reply[5:])

        # convert type
        try:
            if self.typestrings[keyword] == int:
                reply = int(reply)
            elif self.typestrings[keyword] == float:
                reply = float(reply)
        except Exception as message:
            azcam.log("ERROR reading telescope data (%s):" % keyword, message)
            return ["ERROR", message]

        return ["OK", reply]

    def parse_reply(self, reply, ReplyLength):
        """
        Internal Use Only.
        """
        try:
            reply = reply.rstrip()
            replist = reply.split(" ")
            reply = self.parse_remove_null(replist)
            return reply[3]
        except:
            raise azcam.AzcamError("telescope parse error")

    def parse_remove_null(self, List):
        """
        Internal Use Only.<br>
        """

        while 1:
            try:
                List.remove("")
            except:
                break

        return List

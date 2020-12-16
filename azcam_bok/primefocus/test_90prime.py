"""
test entire 90prime system
"""

import sys

import azcam


def test_90prime():

    # **************************************************************
    # reset controller
    # **************************************************************
    print("")
    print("*** Testing controller ***")
    print("")
    reply = azcam.api.server.rcommand("controller.Test(2)")
    print(reply)

    print("")
    print("*** Testing instrument ***")
    print("")
    reply = azcam.api.server.rcommand("instrument.Test()")
    print(reply)
    print("")

    print("")
    print("*** Testing telescope ***")
    print("")
    reply = azcam.api.server.rcommand("telescope.Test()")
    print(reply)

    print("")
    print("*** Testing weather ***")
    print("")
    reply = azcam.api.server.rcommand("instrument.GetWeatherInfo()")
    print(reply)

    print("")
    print("*** Testing temperatures ***")
    print("")
    reply = azcam.api.server.rcommand("instrument.GetTemperatureInfo()")
    print("")

    return


if __name__ == "__main__":
    args = sys.argv[1:]
    test_90prime(*args)

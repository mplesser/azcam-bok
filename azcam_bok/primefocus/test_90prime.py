"""
test entire 90prime system
"""

import sys

from azcam.console import azcam


def test_90prime():

    # **************************************************************
    # reset controller
    # **************************************************************
    print("")
    print("*** Testing controller ***")
    print("")
    reply = azcam.console.api.rcommand("controller.Test(2)")
    print(reply)

    print("")
    print("*** Testing instrument ***")
    print("")
    reply = azcam.console.api.rcommand("instrument.Test()")
    print(reply)
    print("")

    print("")
    print("*** Testing telescope ***")
    print("")
    reply = azcam.console.api.rcommand("telescope.Test()")
    print(reply)

    print("")
    print("*** Testing weather ***")
    print("")
    reply = azcam.console.api.rcommand("instrument.GetWeatherInfo()")
    print(reply)

    print("")
    print("*** Testing temperatures ***")
    print("")
    reply = azcam.console.api.rcommand("instrument.GetTemperatureInfo()")
    print("")

    return


if __name__ == "__main__":
    args = sys.argv[1:]
    test_90prime(*args)

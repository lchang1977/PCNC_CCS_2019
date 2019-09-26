import os

wire          = 'example/extend_stage2.json'          # path to wire format
install       = 'tmp.kat'                      # location to extract the program
policy        = "frenetic/ScenarioReconfig.kat"      # location of policy to check containment against
NAL_PATH      = "./_build/default/src/nal.exe" # path to NAL executable
FRENETIC_PATH = "frenetic"                     # path to frenetic executable

def main():
    if not os.popen("{} checker {}".format(NAL_PATH, wire)).read() == "true\n":
        print("Error: Nal fails to typecheck!")
        return False
    if not os.popen("{} verify {}".format(NAL_PATH, wire)).read() == "true\n":
        print("Error: Signatures do not match!")
        return False
    if not os.popen("{} extract {} {}".format(NAL_PATH, wire, install)).read() == "true\n":
       print("Error: Program does not match or no install command!")
       return False
    if not os.popen("{} dump bisim --containment {} {}".format(FRENETIC_PATH, policy, install)).read() == "true\n":
        print("Error: Program not contained by policy!")
        return False

    print("All checks passed! The program {} is safe to install!".format(install))


if __name__ == "__main__":
    main()


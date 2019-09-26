from mininet.topo import Topo, LinearTopo
from mininet.cli import CLI
from mininet.net import Mininet
from mininet.node import RemoteController
from mininet.node import OVSSwitch

import sys

class MyTopo( Topo ):
    "Simple topology example."

    def build( self ):
        "Create custom topo."

        # Add hosts and switches
        h1 = self.addHost('h1')
        h2 = self.addHost('h2')
        h3 = self.addHost('h3')
        h4 = self.addHost('h4')
        h5 = self.addHost('h5')

        sh1 = self.addSwitch('s1')
        sh2 = self.addSwitch('s2')
        er  = self.addSwitch('s3')
        gs  = self.addSwitch('s4')

        # Add links
        self.addLink(h1, sh1, 1, 1)
        self.addLink(h2, sh2, 1, 1)
        self.addLink(sh1, er, 2, 3)
        self.addLink(er, gs, 1, 1)
        self.addLink(sh2, er, 2, 2)
        self.addLink(gs, h3, 3, 1)
        self.addLink(gs, h4, 4, 1)
        self.addLink(gs, h5, 5, 1)

if len(sys.argv) != 3:
    print("Usage: python PCNC_Scenario.py <ip> <port>")
    exit()

mininet = Mininet(topo=MyTopo(), build=False)
mininet.addController(RemoteController('c', sys.argv[1], int(sys.argv[2])))
mininet.build()
mininet.start()

for host in mininet.hosts:
    print(str(host) + " : " + host.IP())

for lnk in mininet.links:
    print(lnk)

print("")

CLI(mininet)
mininet.stop()

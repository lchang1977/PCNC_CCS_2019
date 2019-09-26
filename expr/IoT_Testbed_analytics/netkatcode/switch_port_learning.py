#program 1
import sys, logging
import frenetic
from frenetic.syntax import *
from frenetic.packet import *

class RepeaterApp3(frenetic.App):
	client_id = "repeater"
	def packet_in(self, dpid, port_id, payload):
		pkt = Packet.from_payload(dpid, port_id, payload)
		logging.info("Packet_in event - Switch: "+str(dpid)+":"+str(port_id)+"||"+"Packet: "+str(pkt))

	def connected(self):
		def handle_current_switches(switches):
			logging.info("Connected to Frenetic - Switches: "+str(switches))
		self.current_switches(callback=handle_current_switches)

logging.basicConfig(stream = sys.stderr, \
format='%(asctime)s [%(levelname)s] %(message)s', level=logging.INFO \
)

app = RepeaterApp3()
app.update(SendToController("repeater_app"))
app.start_event_loop()

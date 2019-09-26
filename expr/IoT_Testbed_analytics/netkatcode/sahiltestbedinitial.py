import frenetic
from frenetic.syntax import*

BPM_server1 = IP4DstEq("89.30.121.150") & TCPDstPortEq(443)
BPM_server2 = IP4DstEq("151.101.146.217") & TCPDstPortEq(443)

policy = IfThenElse(BPM_server1|BPM_server2,drop,SetPort(1,2,3))
class Rep(frenetic.App):
	pass

app = Rep()
app.update(policy)
app.start_event_loop()

import frenetic
from frenetic.syntax import *

firewall = ~SwitchEq(3) | (IP4DstEq("10.0.0.3") & TCPDstPortEq(10000))

gsForward = IfThenElse(IP4DstEq("10.0.0.3"), SetPort(3), SetPort(4))

#sahil: Union operator to make copy of packet and apply gsForward on one copy and gsmirroringrule on other copy.
gsmirroringrule = IfThenElse(IP4DstEq("10.0.0.3"), SetIP4Dst("10.0.0.5")>>SetTCPDstPort(80)>>SetPort(5), SetPort(4))
finalgspolicy = Union([gsForward, gsmirroringrule])
#sahil: code ends

erForward = SetPort(1)
shForward = SetPort(2)

#forward = IfThenElse(SwitchEq(3), erForward, IfThenElse(SwitchEq(4), gsForward, shForward))
forward = IfThenElse(SwitchEq(3), erForward, IfThenElse(SwitchEq(4), finalgspolicy, shForward))
# This is here so nc works
backPred = IP4DstEq("10.0.0.1") | IP4DstEq("10.0.0.2")
backProp = IfThenElse(SwitchEq(3), IfThenElse(IP4DstEq("10.0.0.1"), SetPort(3), SetPort(2)), SetPort(1))

policy = (Filter(firewall) >> forward) | (Filter(backPred) >> backProp)

class InitialConfig(frenetic.App):
    pass

app = InitialConfig()
app.update(policy)
app.start_event_loop()


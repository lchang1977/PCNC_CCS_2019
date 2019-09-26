import frenetic
from frenetic.syntax import *

firewall = ~SwitchEq(3) | (IP4DstEq("10.0.0.3") & TCPDstPortEq(10000) & IPProtoEq(17))

#gsForward = IfThenElse(IP4DstEq("10.0.0.3"), SetPort(3), SetPort(4))
#gsForward = IfThenElse(TCPDstPortEq(10000), SetPort(3), SetPort(4))
#erForward = Filter(IP4DstEq("10.0.0.3") | IP4DstEq("10.0.0.4")) >> SetPort(1)
#erForward = SetPort(1)
#shForward = SetPort(2)

#forward = IfThenElse(SwitchEq(4), gsForward, IfThenElse(SwitchEq(3), erForward, shForward))

#forward = Filter(IP4DstEq("10.0.0.3") | IP4DstEq("10.0.0.4")) >> forward

forward = IfThenElse(SwitchEq(3), SetPort(1),SetPort(2))

# This is here so nc works
#backPred = IP4DstEq("10.0.0.1") | IP4DstEq("10.0.0.2")
#backProp = SetPort(1) | IfThenElse(SwitchEq(3), SetPort(3), Filter(~SwitchEq(4)) >> SetPort(2))
backprop_cond = IfThenElse(IP4DstEq("10.0.0.1"), SetPort(3),SetPort(2))
backProp = IfThenElse(SwitchEq(3),backprop_cond ,SetPort(1))

#policy = (Filter(firewall) >> forward) | (Filter(backPred) >> backProp)
policy = (Filter(firewall) >> forward) | (backProp)

class InitialConfig(frenetic.App):
    pass

app = InitialConfig()
app.update(policy)
app.start_event_loop()


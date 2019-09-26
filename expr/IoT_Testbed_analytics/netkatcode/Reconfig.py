import frenetic
from frenetic.syntax import *

#sahil code starts here
switch = SwitchEq(346653522121)

pub_data_cond1 = IP4DstEq("104.16.65.50") & VlanEq(1000) & TCPDstPortEq(443)
pub_data_cond2 = IP4DstEq("104.16.66.50") & VlanEq(1000) & TCPDstPortEq(443)
emergency_data_cond1 = IP4DstEq("89.30.121.150") & VlanEq(1001) & TCPDstPortEq(443)
emergency_data_cond2= IP4DstEq("151.101.146.217") & VlanEq(1001) & TCPDstPortEq(443)

DNSQ = IPProtoEq(17)

#sahil: firewall policy
firewall = ~switch|pub_data_cond1|pub_data_cond2|emergency_data_cond1|emergency_data_cond2|DNSQ

#sahil: forward policy
ovs_f = SetPort(1)
forword_policy = IfThenElse(switch,ovs_f,SetPort(55555))

#sahil: backpropagation policy
ovs_b =  Filter(IP4SrcEq("104.16.65.50") | IP4SrcEq("104.16.66.50") |IP4SrcEq("89.30.121.150")|IP4SrcEq("151.101.146.217")| DNSQ) >>SetPort(2)
backpropagation_policy = IfThenElse(switch,ovs_b,SetPort(55555))

final_policy = Filter(firewall) >> forword_policy | backpropagation_policy

class InitialConfig(frenetic.App):
    pass

app = InitialConfig()
app.update(final_policy)
app.start_event_loop()

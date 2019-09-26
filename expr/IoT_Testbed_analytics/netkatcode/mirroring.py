import frenetic
from frenetic.syntax import *

#sahil code starts here
switch = SwitchEq(346653522121)

#sahil:mirroring policy
pub_data_cond1 = IP4DstEq("104.16.65.50") & TCPDstPortEq(443) 
pub_data_cond2 = IP4DstEq("104.16.66.50") & TCPDstPortEq(443)
DNSQ = IPProtoEq(17) 
#firewall_mirror = ~switch|pub_data_cond1|pub_data_cond2|DNSQ

firewall_mirror = ~switch|pub_data_cond1|pub_data_cond2
#dns_forword_policy = IfThenElse(DNSQ,SetPort(1,2),Drop())

ovs_f_mirror = SetPort(1)
forword_policy_mirror_for_h3 = IfThenElse(switch,SetIP4Dst("104.16.66.50")>>ovs_f_mirror|SetIP4Dst("129.21.61.113")>>SetPort(1),SetPort(55555))
ovs_b_mirror =  Filter(PortEq(1)) >>SetPort(2)
dns_forword_policy = Filter(DNSQ)>>SetPort(1,2)
#backpropagation_policy_mirror = IfThenElse(PortEq(1),SetPort(2),SetPort(55555))
#backpropagation_policy_mirror = IfThenElse(switch,ovs_b_mirror,SetPort(55555))
#final_policy = Filter(firewall_mirror) >> forword_policy_mirror_for_h3 | backpropagation_policy_mirror|dns_forword_policy
#final_policy = Filter(firewall_mirror) >> forword_policy_mirror_for_h3 | backpropagation_policy_mirror
#final_policy = Filter(firewall_mirror) >> forword_policy_mirror_for_h3 | ovs_b_mirror
final_policy = Filter(firewall_mirror) >> forword_policy_mirror_for_h3 | ovs_b_mirror |dns_forword_policy
#final_policy = Filter(firewall_mirror) >> forword_policy_mirror_for_h3 | backpropagation_policy_mirror

class InitialConfig(frenetic.App):
    pass

app = InitialConfig()
app.update(final_policy)
app.start_event_loop()


import frenetic
from frenetic.syntax import *
 
# policy for sw1
firewall_for_sw1 = SwitchEq(1)
forwarding_policy_for_s1 = IfThenElse(PortEq(1),SetPort(2),SetPort(1))
final_policy_for_sw1 = Filter(firewall_for_sw1)>>forwarding_policy_for_s1

# policy for sw2
firewall_for_sw2 = SwitchEq(2)
forwarding_policy_for_s2 = IfThenElse(PortEq(1),SetPort(2),SetPort(1))
final_policy_for_sw2 = Filter(firewall_for_sw2)>>forwarding_policy_for_s2

# policy for sw3
firewall_for_sw3 = SwitchEq(3)
#forwarding_policy_for_s3 = IfThenElse(PortEq(1),SetPort(2,3),IfThenElse(IP4DstEq("10.0.0.3")| IP4DstEq("10.0.0.4"),IfThenElse(TCPDstPortEq(10000)|TCPDstPortEq(11000),SetPort(1),SetPort(9999)),SetPort(9999)))
forwarding_policy_for_s3 = IfThenElse(PortEq(1),SetPort(2,3),IfThenElse(IP4DstEq("10.0.0.3") & TCPDstPortEq(10000)| IP4DstEq("10.0.0.4") & TCPDstPortEq(11000),SetPort(1),SetPort(9999)))

final_policy_for_sw3 = Filter(firewall_for_sw3)>>forwarding_policy_for_s3

# policy for sw4
firewall_for_sw4 = SwitchEq(4)
forwarding_policy_for_s4 = IfThenElse(PortEq(1),SetPort(3,4,5),SetPort(1))
final_policy_for_sw4 = Filter(firewall_for_sw4)>>forwarding_policy_for_s4

final_policy_for_sw1_sw2_sw3_sw4 = final_policy_for_sw1 | final_policy_for_sw2 | final_policy_for_sw3 | final_policy_for_sw4


class InitialConfig(frenetic.App):
    pass

app = InitialConfig()
app.update(final_policy_for_sw1_sw2_sw3_sw4)
app.start_event_loop()


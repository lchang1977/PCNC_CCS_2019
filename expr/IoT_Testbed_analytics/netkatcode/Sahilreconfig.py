import frenetic
from frenetic.syntax import*

policy = SetPort(1,2,3)
class Rep(frenetic.App):
	pass

app = Rep()
app.update(policy)
app.start_event_loop()

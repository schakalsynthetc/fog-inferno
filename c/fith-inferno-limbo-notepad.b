
with generics [T] I think we can do
	mod=initmod(Mod, Mod->PATH);
		returning either mod or Oops
			and then init (running Oops->init() if we have oopsed)


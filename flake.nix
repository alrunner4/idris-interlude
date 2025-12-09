{
	description = "things missing from the Idris Prelude";

	inputs = {
        idrx.url = "github:alrunner4/idrx";
    };

	outputs = { self, idrx }: {
		packages.x86_64-linux.default = import ./default.nix { inherit idrx; };
	};
}

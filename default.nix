{ idrx ? builtins.getFlake "github:alrunner4/idrx" }:
let
stt = idrx.importFromGitHub {
	owner = "alrunner4";
	repo  = "idris-stt";
	rev   = "eb0890a07f0de0dfc3be3b352885b0997ae5ddfd";
	hash  = "sha256-ihcjKBpdNQ0j9nQtXImxSoHnOaWRzOuVMXmH9OFU/7M=";
};
in
idrx.importFromSrc {
	ipkgName = "interlude";
	version = "2025.12.09";
	src = ./.;
	idrisLibraries = [stt];
}

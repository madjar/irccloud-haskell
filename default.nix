{ mkDerivation, base, bytestring, configurator, HsOpenSSL
, io-streams, lens, lens-aeson, network, openssl-streams, stdenv
, text, websockets, wreq
}:
mkDerivation {
  pname = "irccloud-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base bytestring configurator HsOpenSSL io-streams lens lens-aeson
    network openssl-streams text websockets wreq
  ];
  description = "Haskell client for IrcCloud";
  license = stdenv.lib.licenses.bsd3;
}

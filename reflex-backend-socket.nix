{ mkDerivation, base, bytestring, containers, lens, lib, mtl
, network, reflex, semialign, semigroupoids, stm, these, witherable
}:
mkDerivation {
  pname = "reflex-backend-socket";
  version = "0.2.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring lens mtl network reflex semialign semigroupoids stm
    these
  ];
  executableHaskellDepends = [
    base bytestring containers lens network reflex witherable
  ];
  homepage = "https://github.com/qfpl/reflex-backend-socket/";
  description = "Reflex bindings for TCP sockets";
  license = lib.licenses.bsd3;
}

{
    pkgs ? import <nixpkgs> {}
}:
pkgs.mkShell {
    name="dev";
    buildInputs = [
       pkgs.rustup
       pkgs.libiconv
       pkgs.llvm
    ];
    shellHook = ''
        echo "Go!"
    '';
}

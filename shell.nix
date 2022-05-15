{
    pkgs ? import <nixpkgs> {}
}:
pkgs.mkShell {
    name="dev";
    buildInputs = [
       pkgs.rustup
       pkgs.libiconv
    ];
    shellHook = ''
        echo "Go!"
    '';
}

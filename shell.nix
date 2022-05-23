{
    pkgs ? import <nixpkgs> {}
}:
pkgs.mkShell {
    name="dev";
    buildInputs = [
       pkgs.rustup
       pkgs.libiconv
       pkgs.llvm_12
       pkgs.lit
    ];
    shellHook = ''
        export LLVM_SYS_120_PREFIX=`llvm-config --prefix`
        echo "Go!"
    '';
}

# PCNC

 Proof Carrying Network Code: Theory, Design, and Implementation
 
## Building
### PCNC
```bash
git clone https://github.com/uvm-plaid/pcnc.git
cd pcnc/src
dune build nal.exe --profile release
cd ..
cargo build --release
```
### Frenetic
```bash
git clone https://github.com/frenetic-lang/frenetic.git
cd frenetic 
git checkout pcnc
cd ..
opam pin add frenetic frenetic --inplace-build --reuse-build-dir 
```
## USE
The `server-checks.py` file serves as an example of running all checks required of the PCNC Server using the NAL and Frenetic executables.

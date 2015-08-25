find Lamdu -type dir -exec mkdir -p lamdu.hsproj/{} ";"
find Lamdu -name "*.hs" -exec ln -f {} lamdu.hsproj/{} ";"

cd bottlelib
find * -type dir -exec mkdir -p ../lamdu.hsproj/{} ";"
find * -name "*.hs" -exec ln -f {} ../lamdu.hsproj/{} ";"
cd ..

cd submodules/AlgoW
find Data Lamdu Text -type dir -exec mkdir -p ../../lamdu.hsproj/{} ";"
find Data Lamdu Text -name "*.hs" -exec ln -f {} ../../lamdu.hsproj/{} ";"
cd ../..

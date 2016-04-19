find Lamdu -type dir -exec mkdir -p lamdu.hsproj/{} ";"
find Lamdu -name "*.hs" -exec ln -f {} lamdu.hsproj/{} ";"

cd bottlelib
find * -type dir -exec mkdir -p ../lamdu.hsproj/{} ";"
find * -name "*.hs" -exec ln -f {} ../lamdu.hsproj/{} ";"
cd ..

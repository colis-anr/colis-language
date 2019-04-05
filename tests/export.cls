begin
echo [ '1' ] ;
if pipe env into grep [ 'COLIS_X' ] epip then true fi ;
COLIS_X := 'x' ;
echo [ '2' ] ;
if pipe env into grep [ 'COLIS_X' ] epip then true fi ;
export COLIS_X;
echo [ '3' ] ;
if pipe env into grep [ 'COLIS_X' ] epip then true fi ;
export COLIS_Y;
echo [ '4' ] ;
if pipe env into grep [ 'COLIS_Y' ] epip then true fi ;
COLIS_Y := 'y' ;
echo [ '5' ] ;
if pipe env into grep [ 'COLIS_Y' ] epip then true fi
end
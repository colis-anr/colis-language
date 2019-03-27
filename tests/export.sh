echo 1
env|grep COLIS_X || true
COLIS_X=x
echo 2
env|grep COLIS_X || true
export COLIS_X
echo 3
env|grep COLIS_X || true

export COLIS_Y
echo 4
env|grep COLIS_Y || true
COLIS_Y=y
echo 5
env|grep COLIS_Y || true

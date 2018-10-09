x=A
x=${x}B | { echo "X1: ${x}"; x=${x}C; }
echo "X2: ${x}"

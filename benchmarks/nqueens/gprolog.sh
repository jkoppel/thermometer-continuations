# Compile native.pl with GNU Prolog's compiler. The generated binary is
# called "native". It's a Prolog toplevel with the definitions from
# native.pl compiled in.
gplc native.pl  || exit 1
echo "count($1, Count)." | ./native

for f in *.sml
do
	echo $f
	mlton $f
done

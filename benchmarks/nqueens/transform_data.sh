mkdir -p latex

cat result.data | awk -f array.awk > latex/array_data_all.tex
cat result.data | awk -f array.awk -v ONLYSML=1 > latex/array_data_onlysml.tex
cat result.data | awk -f array.awk -v NOSML=1 > latex/array_data_nosml.tex

cat result.data | awk -f plot.awk > latex/plot_data_all.tex
cat result.data | awk -f plot.awk -v ONLYSML=1 > latex/plot_data_onlysml.tex
cat result.data | awk -f plot.awk -v NOSML=1 > latex/plot_data_nosml.tex

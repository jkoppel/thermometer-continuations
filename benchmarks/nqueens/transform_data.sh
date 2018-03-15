mkdir -p latex

cat result.data | awk -f array.awk > latex/array_data_all.tex
cat result.data | awk -f array.awk -v SML=1 | sed "s:(SML/NJ)::g" | sed "s:, SML/NJ):):g" > latex/array_data_sml.tex
cat result.data | awk -f array.awk -v OCAML=1 | sed "s:(OCaml)::g" | sed "s:, OCaml):):g" > latex/array_data_ocaml.tex
cat result.data | awk -f array.awk -v OTHERS=1 | sed "s:(OCaml)::g" | sed "s:, OCaml):):g" > latex/array_data_others.tex

cat result.data | awk -f plot.awk > latex/plot_data_all.tex
cat result.data | awk -f plot.awk -v SML=1 | sed "s:(SML/NJ)::g" | sed "s:, SML/NJ):):g" > latex/plot_data_sml.tex
cat result.data | awk -f plot.awk -v OCAML=1 | sed "s:(OCaml)::g" | sed "s:, OCaml):):g" > latex/plot_data_ocaml.tex
cat result.data | awk -f plot.awk -v OTHERS=1 | sed "s:(OCaml)::g" | sed "s:, OCaml):):g" > latex/plot_data_others.tex

BEGIN { FS="\t" }
/begin-benchmark/ {
  NAME=$2

  # set OCAML, SML or OTHERS with (awk -v SML=1)
  if (OCAML && $2 !~ /OCaml/) { KEEP=0; next }
  else if (SML && $2 !~ /SML/) { KEEP=0; next }
  else if (OTHERS && ($2 ~ /OCaml/ || $2 ~ /SML/)) { KEEP=0; next }

  KEEP=1
}
/header/ && KEEP { print "\\textbf{"$2"}" }
/point/ && KEEP {
  SEC=int($3)/1000
  if (SEC < 60) {
    TIME=SEC"s"
  } else {
    MIN=int(SEC/60)
    SEC=SEC-MIN*60
    TIME=MIN"m"SEC"s"
  }
  print "& "TIME
}
/end-benchmark/ && KEEP { print "\\\\" }

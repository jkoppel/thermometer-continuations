BEGIN { FS="\t" }
/begin-benchmark/ {
  NAME=$2

  # set OCAML, SML or OTHERS with (awk -v SML=1)
  if (OCAML && $2 !~ /OCaml/) { KEEP=0; next }
  else if (SML && $2 !~ /SML/) { KEEP=0; next }
  else if (OTHERS && ($2 ~ /OCaml/ || $2 ~ /SML/)) { KEEP=0; next }

  KEEP=1

  # # We used per-language marking
  # # in the hope of showing all languages together,
  # # but the resulting plot is way too crowded.
  # if (NAME ~ /SML/) {
  #     MARK="square";
  # } else if (NAME ~ /OCaml/) {
  #     MARK="triangle";
  # } else {
  #     MARK="o";
  # }

  if (NAME ~ /Indir/) {
      COLOR="black";
      MARK="-";
  } else if (NAME ~ /Replay/) {
      COLOR="orange";
      MARK="triangle";
  } else if (NAME ~ /OptTherm/) {
      COLOR="purple";
      MARK="oplus";
  } else if (NAME ~ /Therm/) {
      COLOR="red";
      MARK="otimes";
  } else if (NAME ~ /Fil/) {
      COLOR="blue";
      MARK="star";
  } else {
      COLOR="brown";
      MARK="asterisk";
  }

  print "\\addplot[color="COLOR",mark="MARK"]"
  print "coordinates {"
  X=0
  Y=0
  LABEL=""
}
/header/ && KEEP { LABEL=$2 }
/point/ && KEEP {
  X=$2
  Y=(int($3)/1000)
  print "("X", "Y")"
}
/end-benchmark/ && KEEP {
  print "};"

  # gather time label to put on last node
  N=X
  SEC=Y
  if (SEC < 60) {
    TIME=SEC"s"
  } else {
    MIN=int(SEC/60)
    SEC=SEC-MIN*60
    TIME=MIN"m"SEC"s"
  }
  # if (ONLYSML || NOSML) {
  #   print "\\node[above] at (axis cs: "N", "Y") {"TIME"};"
  # }
  print "\\addlegendentry{"LABEL"}"
  print ""
}

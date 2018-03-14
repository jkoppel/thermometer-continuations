BEGIN { FS="\t" }
/begin-benchmark/ {
  NAME=$2

  # set ONLYSML and/or NOSML with (awk -v NOSML=1)
  if (ONLYSML && $2 !~ /SML/) { KEEP=0; next }
  if (NOSML && $2 ~ /SML/) { KEEP=0; next }

  KEEP=1

  if (NAME ~ /SML/) {
      MARK="square"; 
  } else if (NAME ~ /OCaml/) {
      MARK="triangle";
  } else {
      MARK="o";
  }

  if (NAME ~ /Indir/) {
      COLOR="black";
  } else if (NAME ~ /Replay/) {
      COLOR="orange";
  } else if (NAME ~ /OptTherm/) {
      COLOR="purple";
  } else if (NAME ~ /Therm/) {
      COLOR="red";
  } else if (NAME ~ /Fil/) {
      COLOR="blue";
  } else {
      COLOR="brown";
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
  # print "\\node[above] at (axis cs: "X", "Y") {"Y"};"
  print "\\addlegendentry{"LABEL"}"
  print ""
}


parallel -u " savilerow -boundvars -in-eprime sarays.eprime -in-param {} -out-minion {.}.minion " ::: *.param
parallel -u " minion {} | tee {.}.minion-out " ::: *.minion

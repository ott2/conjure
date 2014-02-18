#!/bin/bash


function conjureInDir() {
    pushd "$1"
    echo "conjureInDir working in directory: $(pwd)"
    conjure --mode df --in *.essence --no-dontCare --out noDontCare   +RTS -s 2> "noDontCare.stderr"   | tee "noDontCare.stdout"
    conjure --mode df --in *.essence               --out usesDontCare +RTS -s 2> "usesDontCare.stderr" | tee "usesDontCare.stdout"
    popd
}
export -f conjureInDir


function conjureInAllDirs() {
    parallel conjureInDir ::: Set-ExplicitVarSize/01 Set-ExplicitVarSize/02 Set-ExplicitVarSize/03 Set-ExplicitVarSize/04
}
export -f conjureInAllDirs


function srOne() {
    BASE="$1"
    echo "Running Savile Row on ${BASE}"
    savilerow                                                           \
        -boundvars                                                      \
        -deletevars                                                     \
        -run-minion     minion                                          \
        -in-eprime      "${BASE}.eprime"                                \
        -out-minion     "${BASE}.minion"                                \
        -out-info       "${BASE}.info"                                  \
        -out-solution   "${BASE}.eprime-solution" 2> "${BASE}.stderr" | tee "${BASE}.stdout"
    rm -f "${BASE}.minion.aux" "${BASE}.infor"
}
export -f srOne


function srAll() {
    parallel srOne {.} ::: $(find Set-ExplicitVarSize -name "*.eprime")
}
export -f srAll


function report_nodes() {
    grep MinionNodes Set-ExplicitVarSize/*/*/*.info
}
export -f report_nodes


function report_minionTimes() {
    grep MinionSolveTime Set-ExplicitVarSize/*/*/*.info
}
export -f report_minionTimes


function recompute() {
    echo "recomputing..."
    conjureInAllDirs
    srAll
    echo "recomputed, happy?"
}
export -f recompute


function clean() {
    rm -rf Set-ExplicitVarSize/*/*DontCare
    rm -f Set-ExplicitVarSize/*/*.stdout
    rm -f Set-ExplicitVarSize/*/*.stderr
}
export -f


#!/bin/bash

#set -x
set -e
set -o pipefail #abort if left command on a pipe fails

function usage {
  echo "Usage: $0 [-h] [--repo-user USER] [--repo-protocol PROTOCOL] [--repo-repo REPO] [--no-update] [--no-compil]"
  echo "               [--no-exec] [--no-comp] [--no-remove] [--force] [--commit SHA] [--ref REF]"
  echo "               [--only-model MODEL] [--no-enable-gh-pages] [--perf PERF] [--no-doc-gen]"
  echo "               [--hostname HOSTNAME] [--mail MAIL]"
  echo "--repo-user USER"
  echo "                user hosting the pyft repository on github,"
  echo "                defaults to the env variable PYFTREPOuser (=$PYFTREPOuser)"
  echo "--repo-protocol PROTOCOL"
  echo "                protocol (https or ssh) to reach the pyft repository on github,"
  echo "                defaults to the env variable PYFTREPOprotocol (=$PYFTREPOprotocol)"
  echo "--repo-repo REPO"
  echo "                repository name defaults to the env variable PYFTREPOrepo (=$PYFTREPOrepo)"
  echo "--no-update     do not update the tools"
  echo "--force         perform the test even if github commit status already exists"
  echo "--commit SHA    use the commit with sha SHA instead of the last one"
  echo "--ref REF       ref to use (defaults to refs/heads/main)"
  echo "--no-enable-gh-pages"
  echo "                dont't try to enable the project pages on github"
  echo "--hostname HOSTNAME"
  echo "                the context is built using the provided hostname instead of the real hostname"
  echo "                This can be usefull when running on a cluster node which can vary from run to run"
  echo "--mail MAIL     comma-separated list of e-mail addresses (no spaces); if not provided, mail is not sent"
  echo ""
  echo "This script provides functionality for automated tests."
  echo "It can be run with cron to periodically test the last commit on the pyft repository"
  echo "(eg '00 22 * * * bash -l -c \"SHELL=/bin/bash PYFTWORKDIR=~/PYFTTESTING ~/PYFTTESTING/pyft/bin/testing.sh \\"
  echo "                             --repo-user UMR-CNRM --repo-protocol ssh --repo-repo pyft user@domain\"')"
  echo "The repository must be hosted on github as it relies on github project pages and github statuses."
  echo "A github token must be set in the .netrc file."
  echo ""
  echo "All the work is done within the \${PYFTWORKDIR} directory (it defaults to ~/PYFTTESTING)."
}

MAIL=""
PYFTREPOuser=${PYFTREPOuser:=UMR-CNRM}
PYFTREPOrepo=${PYFTREPOrepo:=pyft}
PYFTREPOprotocol=${PYFTREPOprotocol:=ssh}
REF="refs/heads/main"
WORKDIR=${PYFTWORKDIR:=${HOME}/PYFTTESTING}
update=1
commit=""
SHA=0
force=0
enableghpages=1
contextHostname=${HOSTNAME}

allargs="$@"
while [ -n "$1" ]; do
  case "$1" in
    '-h') usage; exit;;
    '--repo-user') export PYFTREPOuser=$2; shift;;
    '--repo-protocol') export PYFTREPOprotocol=$2; shift;;
    '--repo-repo') export PYFTREPOrepo=$2; shift;;
    '--no-update') update=0;;
    '--force') force=1;;
    '--commit') SHA=$2; shift;;
    '--ref') REF=$2; shift;;
    '--no-enable-gh-pages') enableghpages=0;;
    '--hostname') contextHostname=$2; shift;;
    '--mail') MAIL=$2; shift;;
    *) echo "Unexpected argument"; exit 1;;
  esac
  shift
done
[ "${models}" == "" ] && models="ial mesonh testprogs lmdz"

[ ! -d ${WORKDIR} ] && mkdir -p ${WORKDIR}

#stdout and stderr redirection
logfile="${WORKDIR}/logfile_${contextHostname}"
if [ -f "${logfile}" ]; then
  mv "${logfile}" "${logfile}.old"
fi
exec > "${logfile}" 2>&1

#context for statuses
context="continuous-integration/${contextHostname}"

#Interactions with github
if [ "${PYFTREPOprotocol}" == 'ssh' ]; then
  PYFTREPOgiturl="git@github.com:${PYFTREPOuser}/${PYFTREPOrepo}.git"
else
  PYFTREPOgiturl="https://github.com/${PYFTREPOuser}/${PYFTREPOrepo}.git"
fi
TOKEN=$(python3 -c "import netrc, socket; print(netrc.netrc().authenticators('github.com')[2])")

function get_last_commit {
  git ls-remote "${PYFTREPOgiturl}" "${REF}" | cut -f1
}

function enable_gh_pages {
  result=$(curl -L --netrc --insecure \
                -H "Authorization: Bearer $TOKEN" \
                -H "Accept: application/vnd.github+json" \
                -H "X-GitHub-Api-Version: 2022-11-28" \
                "https://api.github.com/repos/${PYFTREPOuser}/${PYFTREPOrepo}/pages")
  if [ $(echo $result | grep 'Not Found' | wc -l) -eq 1 ]; then
    log 1 "Github project pages not yet enabled"
    #Pages are not yet activated
    curl -L --netrc --insecure \
         -X POST \
         -H "Authorization: Bearer $TOKEN" \
         -H "Accept: application/vnd.github+json" \
         -H "X-GitHub-Api-Version: 2022-11-28" \
         "https://api.github.com/repos/${PYFTREPOuser}/${PYFTREPOrepo}/pages" \
         -d '{"source":{"branch":"master","path":"/docs"}}'
  fi
}

function get_statuses {
  SHA="$1"
  curl -L --netrc --insecure \
    -H "Accept: application/vnd.github+json" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "https://api.github.com/repos/${PYFTREPOuser}/${PYFTREPOrepo}/commits/${SHA}/statuses"
}

function add_status {
  error=$1
  ret=$2
  SHA="$3"
  comment="$4"
  if [ $ret -eq 0 ]; then
    state="success"
  else
    if [ $error -eq 1 ]; then
      state="error"
    else
      state="failure"
    fi
  fi
  url="https://${PYFTREPOuser}.github.io/${PYFTREPOrepo}/displayparam.html?"
  url=${url}$(content=$(echo -e "$comment") python3 -c "import urllib.parse, os; print(urllib.parse.quote('<pre>' + os.environ['content'] + '</pre>', safe=':/='))")
  curl -L --insecure \
    -X POST \
    -H "Accept: application/vnd.github+json" \
    -H "Authorization: Bearer $TOKEN" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "https://api.github.com/repos/${PYFTREPOuser}/${PYFTREPOrepo}/statuses/${SHA}" \
    -d '{"state":"'${state}'","target_url":"'${url}'","context":"'${context}'"}'
}

#reporting
function send_mail {
  message="$1"
  if [ "$MAIL" != "" ]; then
    if command -v mail; then
      mail -s "$context" "$MAIL" <<EOF
$(echo -e ${message})
EOF
    else
      sendmail "$MAIL" <<EOF
Subject: $context
$(echo -e ${message})
EOF
    fi
  fi
}

header="${context}\n\n$(date)"
message=""
function report {
  error=$1
  ret=$2
  if [ ${ret} -eq 0 ]; then
    error_msg=""
  else
    error_msg="XXXXXXXXXXXXXXXXXXXX ERROR ${ret} XXXXXXXXXXXXXXXXXXXX"
    error_msg="${error_msg}\n\n"
  fi
  message="${header}\n${message}\n\n${error_msg}$(date)"
  if [ ${ret} -ne 0 ]; then
    send_mail "${message}"
  fi
  if [ "${SHA}" != 0 ]; then
    add_status $error $ret "${SHA}" "${message}"
  fi
}

log_message=""
function exit_error {
  ret=$1
  if [ ${ret} -ne 0 ]; then
    message="__ ABNORMAL EXIT ${ret} __\n${log_message}\n${message}"
    message="${message}\n\nMore information can be found in ${HOSTNAME}:${logfile}"
    report 1 ${ret}
  fi
}
trap 'exit_error $?' EXIT

function log {
  level=$1; shift
  echo "$@"
  if [ ${level} -eq 0 ]; then
    message="${message}\n$@"
  fi
  log_message="${log_message}\n$@"
}

#Test
if [ "${SHA}" -eq 0 ]; then
  log 1 "Getting last commit hash"
  SHA=$(get_last_commit)
  log 1 "Commit hash is ${SHA}"
fi
if [ ${force} -eq 1 -o $(get_statuses "${SHA}" | grep -w "${context}" | wc -l) -eq 0 ]; then
  log 1 "This commit has not been tested (or --force is provided)"
  ret=0
  
  #Checkout tools, set PATH and use the last version of the testing script
  currentdir="${PWD}"
  if [ ${update} -eq 1 ]; then
    currentMD5=$(md5sum "${BASH_SOURCE[0]}"  | cut -d\  -f1)
    if [ ! -d "${WORKDIR}/pyft" ]; then
      log 1 "Clonig pyft in ${WORKDIR}/pyft"
      git clone "${PYFTREPOgiturl}" "${WORKDIR}/pyft"
    else
      log 1 "Checkout commit ${SHA}"
      cd "${WORKDIR}/pyft"
      git fetch "${PYFTREPOgiturl}"
      git checkout "${SHA}"
      cd "${currentdir}"
    fi
    if [ -f "${WORKDIR}/pyft/bin/testing.sh" ]; then
      if [ "${currentMD5}" != $(md5sum "${WORKDIR}/pyft/bin/testing.sh" | cut -d\  -f1) ]; then
        log 1 "Script has changed, running the new version" #This log and the previous ones are lost
        exec "${WORKDIR}/pyft/bin/testing.sh" $allargs
      fi
    fi
    log 1 "Installing/updating pyft"
    ${WORKDIR}/pyft/bin/INSTALL.sh
  fi
  . "${WORKDIR}/pyft/bin/env.sh"

  #Enable the gihub project pages
  if [ $enableghpages -eq 1 ]; then
    log 1 "Test if github project pages are enabled"
    enable_gh_pages
  fi

  cd "${WORKDIR}/pyft"

  #Check 'version' consistency
  log 1 "Check 'version' consistency"
  set +e
  vinit=$(python3 -c "$(grep __version__ src/pyft/__init__.py); print(__version__)")
  retval1=$?
  vtag=$(git describe --abbrev=0)
  retval2=$?
  set -e
  if [ $retval1 -ne 0 -o $retval2 -ne 0 ]; then
    ret=1
    log 0 "  'version' consistency: ERROR"
  else
    if [ "${vinit}" == "${vtag}" ]; then
      log 0 "  'version' consistency: OK"
    else
      ret=1
      log 0 "  'version' consistency: problem"
    fi
  fi

  #Check pylint
  log 1 "Check pylint"
  set +e
  score=$(pylint -d R0912,C0209,R0915,R1702,C0302,R0913,R0914,W1202,R0904,R0902 \
          --persistent=n -f parseable bin/*.py src/pyft/ | \
          grep 'Your code has been rated at' | cut -d\  -f 7 | cut -d/ -f 1)
  set -e
  if [ $(python3 -c "print(0 if ${score} >= 9.8 else 1)") -ne 0 ]; then
    ret=1
    log 0 "  pylint score: problem"
  else
    log 0 "  pylint score: OK"
  fi

  #Check flake8
  log 1 "Check flake8"
  set +e
  score=$(flake8 bin/*.py src/pyft/ | wc -l)
  retval=$?
  set -e
  if [ $retval -ne 0 ]; then
    ret=1
    log 0 "  flake8 score: ERROR"
  else
    if [ ${score} -ne 0 ]; then
      ret=1
      log 0 "  flake8 score: problem"
    else
      log 0 "  flake8 score: OK"
    fi
  fi

  #Test examples
  cd examples
  log 1 "Test examples"
  set +e
  if [ "$(stat -L -c %d:%i $(which pyft_tool.py))" != "$(stat -L -c %d:%i ../bin/pyft_tool.py)" ]; then
    ret=1
    log 0 "  test examples: ERROR (PATH not correctly set)"
  else
    if [ "$(stat -L -c %d:%i $(python3 -c 'import pyft; print(pyft.__file__)'))" != \
         "$(stat -L -c %d:%i ../src/pyft/__init__.py)" ]; then
      ret=1
      log 0 "  test examples: ERROR (PYTHONPATH not correctly set)"
    else
      ./tests.sh 2>&1 > /dev/null
      retval=$?
      set -e
      if [ ${retval} -ne 0 ]; then
        ret=1
        log 0 "  test examples: problem"
      else
        log 0 "  test examples: OK"
      fi
    fi
  fi
  set -e

  if [ $ret -eq 0 ]; then
    log 0 "global result: OK"
  else
    log 0 "global result: PROBLEM"
  fi

  #Report result
  report 0 ${ret}
fi

#! /bin/bash

main() {
    set -eu

    # Global for the cleanup. We make this random, to make sure that
    # parallel build of the same package file, or parallel CI jobs
    # do not interfere
    CONTAINER=$(make_uuid | tr -d -- '-')
    CLEANUPIMAGE=
    CLEANUPFILES=('dummy')
    CLEANUPKEEP=false
    trap cleanup 0

    echo "R-hub Linux builder script v0.10.0 (c) R Consortium, 2018-2019"
    echo

    # Parse arguments
    local image=rhub/debian-gcc-devel
    local envvars=""
    local checkargs=""
    local artifacts="."
    while getopts ":hi:e:c:a:kd:" opt; do
	case $opt in
	    i)  image="$OPTARG"     ;;
	    e)  envvars="$OPTARG"   ;;
	    c)  checkargs="$OPTARG" ;;
	    a)  artifacts="$OPTARG" ;;
	    k)  CLEANUPKEEP=true    ;;
	    d)  CONTAINER="$OPTARG" ;;
	    h)  help; exit 1        ;;
	    \?) >&2 echo "Invalid option: -$OPTARG"; usage; exit 2  ;;
	    :)  >&2 echo "Option -$OPTARG requires an argument. :(";
		usage; exit 2 ;;
	esac
    done

    shift $((OPTIND-1))
    if (( $# != 1 )); then
	>&2 echo "No package :( Specify one package fileor URL to build."
	help
	exit 3;
    fi
    declare package="$1"

    echo Package: "$package"
    echo Docker image: "$image"
    echo Env vars: "$envvars"
    echo R CMD check arguments: "$checkargs"

    check_requirements || exit $?

    download_package "$package" || exit $?
    package="$REPLY"

    detect_platform "$image"
    local platform="$REPLY"
    echo "Sysreqs platform: $platform"
    export RHUB_PLATFORM="$platform"

    # Install sysreqs and create a image from it
    install_sysreqs "$package" "$image" "$CONTAINER" "$platform"
    image="$REPLY"

    create_env_file "$package" "$envvars" "$checkargs"
    local envfile=$REPLY

    run_check "$package" "$image" "$CONTAINER" "$envfile"

    get_artifacts $CONTAINER $artifacts

    # Cleanup is automatic
}

usage() {
    >&2 echo
    >&2 echo "Usage: $0 [-h] [-k] [-i image] [-e env] [-c checkargs] [-a path] package-file"
    >&2 echo
    >&2 echo "Options:"
    >&2 echo "  -k            Keep build container"
    >&2 echo "  -i image      Docker image to use [default: rhub/debian-gcc-devel]"
    >&2 echo "  -e env        Environment variables to set, VAR=VALUE, newline separated"
    >&2 echo "  -c checkargs  Arguments for 'R CMD check'"
    >&2 echo "  -a path       Where to store check artifacts [default:.]"
    >&2 echo "  -h            Print help message"
}

help() {
    usage
    >&2 echo
    >&2 echo "Run 'R CMD check' on an R package, within a Docker container."
    >&2 echo "'package-file' should be a local R source package, or an URL to one."
    >&2 echo "Calls 'R CMD build' automatically, if needed."
}

make_bad_uuid()  {
    local N B C='89ab'
    for (( N=0; N < 16; ++N ))
    do
	B=$(( $RANDOM%256 ))
	case $N in
	    6)
		printf '4%x' $(( B%16 ))
		;;
	    8)
		printf '%c%x' ${C:$RANDOM%${#C}:1} $(( B%16 ))
		;;
	    3 | 5 | 7 | 9)
		printf '%02x-' $B
		;;
	    *)
		printf '%02x' $B
		;;
	esac
    done
    echo
}

make_uuid() {
    cat /proc/sys/kernel/random/uuid 2>/dev/null ||
	uuidgen 2>/dev/null || make_bad_uuid
}

check_requirements() {
    # Check for Docker, R and the packages we need
    if ! docker --version >/dev/null 2>/dev/null; then
	>&2 echo "Cannot find Docker :("
	>&2 echo "Make sure that Docker installed and it is in the PATH."
	>&2 echo "You can install Docker from https://www.docker.com/"
	return 1
    fi
    if ! R --slave -e 'x <- 1' >/dev/null 2>/dev/null; then
	>&2 echo "Cannot find R :("
	>&2 echo "Make sure that R installed and it is in the PATH"
	>&2 echo "You can install R from https://cran.r-project.org/"
	return 1
    fi
    if ! R --slave -e 'library(sysreqs)' >/dev/null 2>/dev/null; then
	>&2 echo "Cannot load the sysreqs package :(  Install it with"
	>&2 echo "R -q -e \"source('https://install-github.me/r-hub/sysreqs')\""
	return 2
    fi
}

download_package() {
    declare -r package="$1"
    REPLY=$(mktemp).tar.gz
    CLEANUPFILES+=("$REPLY")
    if [[ "$package" =~ ^https?:// ]]; then
	echo
	echo ">>>>>==================== Downloading package file"
	if ! wget -O "$REPLY" "$package"; then
	    >&2 echo "Cannot download package file :("
	    return 3
	fi
    else
	cp "$package" "$REPLY"
    fi
}

detect_platform() {
    declare -r image="$1"
    REPLY=$(docker run --user docker \
		   --rm ${image} \
		   sh -c 'echo $RHUB_PLATFORM')
}

get_desc() {
    declare -r package="$1"
    local desc=$(tar tzf "$package" | grep "^[^/]*/DESCRIPTION$")
    local dir=$(mktemp -d)
    CLEANUPFILES+=("$dir")
    tar xzf "$package" -C "$dir" "$desc"
    REPLY="${dir}/${desc}"
}

install_sysreqs() {
    declare -r package="$1" image="$2" container="$3" platform="$4"
    # Default is doing nothing
    REPLY="$image"

    # If there is no RHUB_PLATFORM, skip sysreqs
    if [[ -z "$platform" ]]; then
	echo "Unknown platform, skipping installation of system requirements"
	return
    fi

    # If there is nothing to install we just use the stock image
    get_desc "$package"
    local desc=$REPLY
    local cmd="library(sysreqs); cat(sysreq_commands(\"$desc\"))"
    local sysreqs=$(Rscript -e "$cmd")

    # Install them, if there is anything to install
    if [[ -z "${sysreqs}" ]]; then
	echo "No system requirements"
    fi

    echo
    echo ">>>>>==================== Installing system requirements"
    local sysreqsfile=$(mktemp)
    CLEANUPFILES+=("$sysreqsfile")
    echo "${sysreqs}" > "$sysreqsfile"
    docker create --user root --name "${container}-1" \
	   "$image" bash /root/sysreqs.sh
    docker cp "$sysreqsfile" "${container}-1:/root/sysreqs.sh"
    docker start -i -a "${container}-1"
    REPLY=$(docker commit "${container}-1")
    CLEANUPIMAGE="$image"
}

create_env_file() {
    declare -r package="$1" envvars="$2" checkargs="$3"
    local envfile=$(mktemp)
    CLEANUPFILES+=("$envfile")

    # These can be overriden by the user supplied env vars
    echo R_REMOTES_STANDALONE=true              >> "$envfile"
    echo R_REMOTES_NO_ERRORS_FROM_WARNINGS=true >> "$envfile"
    echo TZ=Europe/London                       >> "$envfile"
    local mirror="${RHUB_CRAN_MIRROR:-https://cloud.r-project.org}"
    echo RHUB_CRAN_MIRROR="$mirror"             >> "$envfile"
    echo _R_CHECK_FORCE_SUGGESTS_=${_R_CHECK_FORCE_SUGGESTS_:-false} \
	 >> "$envfile"

    # User supplied env vars
    echo "$envvars"  >> "$envfile"

    # These canot be overriden
    echo checkArgs="${checkargs}" >> "$envfile"  # note the uppercase!
    local basepackage=$(basename "$package")
    echo package="$basepackage"   >> "$envfile"
    REPLY="$envfile"
}

run_check() {
    echo
    echo ">>>>>==================== Starting Docker container"
    declare package="$1" image="$2" container="$3" envfile="$4"
    local basepackage=$(basename "$package")

    docker create -i --user docker --env-file "$envfile" \
	   --name "${container}-2" "$image" /bin/bash -l /tmp/build.sh \
	   "/tmp/$basepackage"
    docker cp rhub-linux-docker.sh "${container}-2:/tmp/build.sh"
    docker cp "$package" "${container}-2:/tmp/$basepackage"
    docker start -i -a "${container}-2"
}

get_artifacts() {
    declare container="$1" artifacts="$2"
    local tmp=$(mktemp -d ./tmp.XXXXXXXXX)
    CLEANUPFILES+=("$tmp")

    if ! docker cp "${container}-2:/tmp/artifacts" "$tmp"; then
	>&2 echo "No artifacts were saved :("
	return
    fi

    local packagename=$(cat "${tmp}/artifacts/packagename")
    local output=${JOB_BASE_NAME:-${artifacts}/${packagename}-${container}}
    if [[ -e "$output" ]]; then
	local i=1
	while [[ -e "${output}-${i}" ]]; do i=$((i+1)); done
	output="${output}-${i}"
    fi

    mv "${tmp}/artifacts" "${output}"
    echo Saved artifacts in "${output}"

    RHUB_ARTIFACTS=${RHUB_ARTIFACTS:-none}
    if [[ "x$RHUB_ARTIFACTS" = "xlocal" ]]; then
	cp -r "$output" /artifacts/
    fi
}

cleanup() {
    # Containers
    docker rm -f -v "${CONTAINER}-1" >/dev/null 2>/dev/null || true

    if [[ "$CLEANUPKEEP" != "true" ]]; then
	docker rm -f -v "${CONTAINER}-2" >/dev/null 2>/dev/null || true
    else
	docker kill "${CONTAINER}-2" >/dev/null 2>/dev/null || true
	if !  docker inspect "${CONTAINER}-2" >/dev/null 2>/dev/null; then
	    echo Build container is "${CONTAINER}-2"
	fi
    fi

    # Image
    docker rmi -f "$CLEANUPIMAGE"  >/dev/null 2>/dev/null || true

    # Temp files
    for i in ${CLEANUPFILES[@]}; do
	rm -rf "$i" 2>/dev/null || true
    done
}

[[ "$0" == "$BASH_SOURCE" ]] && main "$@"

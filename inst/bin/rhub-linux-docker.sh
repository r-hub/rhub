#! /bin/bash


main() {

    set -eu

    # The default might not be the home directory, but /
    cd ~

    # We create this up front so it always exists
    mkdir -p /tmp/artifacts
    local oldpackage=${package}

    config_all "$package"

    build_package "$package"
    package="$REPLY"
    cp "$package" /tmp/artifacts/
    echo "$package" >/tmp/artifacts/packagename

    install_remotes

    install_deps "$package"
    cp *.tar.gz /tmp/artifacts/

    run_check "$package" || true
    cp -r *.Rcheck /tmp/artifacts/ 2>/dev/null >/dev/null || true
}

config_all() {
    # Configure R, local package library, and also CRAN and BioConductor
    # The container might define $RBINARY
    declare -r package="$1"
    export RBINARY="${RBINARY:-R}"
    export PATH="$(ls /opt/R-* -d)/bin:$PATH"
    export R_LIBS=~/R
    mkdir -p ~/R
    echo "options(repos = c(CRAN = \"$RHUB_CRAN_MIRROR\"))" >> ~/.Rprofile
    "$RBINARY" -q -e "source('https://bioconductor.org/biocLite.R')"
    echo "options(repos = BiocInstaller::biocinstallRepos())" >> ~/.Rprofile
    echo "unloadNamespace('BiocInstaller')" >> ~/.Rprofile
    cp "/tmp/${package}" .
}

get_desc() {
    declare -r package="$1"
    local desc=$(tar tzf "$package" | grep "^[^/]*/DESCRIPTION$")
    local dir=$(mktemp -d)
    tar xzf "$package" -C "$dir" "$desc"
    REPLY="${dir}/${desc}"
}

build_package() {
    declare -r package="$1"
    get_desc "$package"
    local desc="$REPLY"
    if grep -q "^Packaged:" "$desc"; then
	local name=$(grep "^Package:" "$desc" | sed 's/^Package:[ ]*//' | tr -d ' \r')
	local vers=$(grep "^Version:" "$desc" | sed 's/^Version:[ ]*//' | tr -d ' \r')
	REPLY="${name}_${vers}.tar.gz"
	if [[ "$package" != "$REPLY" ]]; then
	    mv "$package" "$REPLY"
	fi
	return
    fi

    echo
    echo ">>>>>==================== Running R CMD build"
    mkdir -p build
    (
	cd build
	tar xzf ../"${package}"
	local pkgname=$(ls | head -1 | sed 's/\///')
	"$RBINARY" CMD build "${pkgname}"
	cd ..
    )
    REPLY=$(basename $(ls build/*.tar.gz | head -1))
    cp "build/${REPLY}" .
}

install_remotes() {
    echo
    echo ">>>>>==================== Installing remotes package"
    # Download the single file install script from r-lib/remotes
    # We cannot do this from R, because some R versions do not support
    # HTTPS. Then we install a proper 'remotes' package with it.
    curl -O https://raw.githubusercontent.com/r-lib/remotes/r-hub/install-github.R
    xvfb-run -a "$RBINARY" -q -e \
	     "source(\"install-github.R\")\$value(\"r-lib/remotes@r-hub\")"
}

install_deps() {
    echo
    echo ">>>>>==================== Installing package dependencies"
    declare -r package="$1"

    # Install the package, so its dependencies will be installed
    # This is a temporary solution, until remotes::install_deps works on a
    # package bundle
    local cmd="remotes::install_local(\"$package\", dependencies = TRUE, INSTALL_opts = \"--build\")"
    xvfb-run -a "$RBINARY" -q -e "$cmd"

    ## If installation fails, then we do not run the check at all
    local pkgname=$(echo $package | sed 's/_.*$//')
    if ! $RBINARY -q -e "library($pkgname)"; then
	>&2 echo "Failed to isntall package, cannot check it :("
	exit 1
    fi
}

run_check() {
    echo
    echo ">>>>>==================== Running R CMD check"
    declare -r package="$1"

    RHUB_CHECK_COMMAND="${RHUB_CHECK_COMMAND:-$RBINARY CMD check $checkArgs}"

    echo About to run xvfb-run $RHUB_CHECK_COMMAND "$package"
    xvfb-run -a $RHUB_CHECK_COMMAND "$package"

    echo
    echo "<<<<<==================== Running R CMD check done"
}

[[ "$0" == "$BASH_SOURCE" ]] && main "$@"

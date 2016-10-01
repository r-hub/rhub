
context("platforms")

json <- '[
    {
	"name": "debian-gcc-devel",
	"description": "Debian Linux, R-devel, GCC",
	"cran-name": "r-devel-linux-x86_64-debian-gcc",
	"rversion": "r-devel",
	"os-type": "Linux",
	"cpu-type": "x86_64",
	"os-info": "Debian GNU/Linux testing",
	"compilers": "GCC 5.4.0 (Debian 5.4.0-4)",
	"docker-image": "debian-gcc-devel",
	"sysreqs-platform": "linux-x86_64-debian-gcc"
    },

    {
	"name": "debian-gcc-release",
	"description": "Debian Linux, R-release, GCC",
	"cran-name": "r-release-linux-x86_64",
	"rversion": "r-release",
	"os-type": "Linux",
	"cpu-type": "x86_64",
	"os-info": "Debian GNU/Linux testing",
	"compilers": "GCC 5.3.1 (Debian 5.3.1-14)",
	"docker-image": "debian-gcc-release",
	"sysreqs-platform": "linux-x86_64-debian-gcc"
    },

    {
	"name": "debian-gcc-patched",
	"description": "Debian Linux, R-patched, GCC",
	"cran-name": "r-patched-linux-x86_64",
	"rversion": "r-patched",
	"os-type": "Linux",
	"cpu-type": "x86_64",
	"os-info": "Debian GNU/Linux testing",
	"compilers": "GCC 5.4.0 (Debian 5.4.0-4)",
	"docker-image": "debian-gcc-patched",
	"sysreqs-platform": "linux-x86_64-debian-gcc"
    },

    {
	"name": "fedora-gcc-devel",
	"description": "Fedora Linux, R-devel, GCC",
	"cran-name": "r-devel-linux-x86_64-fedora-gcc",
	"rversion": "r-devel",
	"os-type": "Linux",
	"cpu-type": "x86_64",
	"os-info": "Fedora 24",
	"compilers": "GCC 6.1.1",
	"docker-image": "fedora-gcc-devel",
	"sysreqs-platform": "linux-x86_64-fedora-gcc"
    },

    {
	"name": "fedora-clang-devel",
	"description": "Fedora Linux, R-devel, clang, gfortran",
	"cran-name": "r-devel-linux-x86_64-fedora-clang",
	"rversion": "r-devel",
	"os-type": "Linux",
	"cpu-type": "x86_64",
	"os-info": "Fedora 24",
	"compilers": "clang version 3.8.0; GNU Fortran 6.1.1",
	"docker-image": "fedora-clang-devel",
	"sysreqs-platform": "linux-x86_64-fedora-clang"
    },

    {
	"name": "ubuntu-gcc-devel",
	"description": "Ubuntu Linux 16.04 LTS, R-devel, GCC",
	"cran-name": null,
	"rversion": "r-devel",
	"os-type": "Linux",
	"cpu-type": "x86_64",
	"os-info": "Ubuntu 16.04 LTS",
	"compilers": "GCC 5.3.1",
	"docker-image": "ubuntu-gcc-devel",
	"sysreqs-platform": "linux-x86_64-ubuntu-gcc"
    },

    {
	"name": "ubuntu-gcc-release",
	"description": "Ubuntu Linux 16.04 LTS, R-release, GCC",
	"cran-name": null,
	"rversion": "r-release",
	"os-type": "Linux",
	"cpu-type": "x86_64",
	"os-info": "Ubuntu 16.04 LTS",
	"compilers": "GCC 5.3.1",
	"docker-image": "ubuntu-gcc-release",
	"sysreqs-platform": "linux-x86_64-ubuntu-gcc"
    },

    {
	"name": "linux-x86_64-centos6-epel",
	"description": "CentOS 6, stock R from EPEL",
	"cran-name": null,
	"rversion": "r-release",
	"os-type": "Linux",
	"cpu-type": "x86_64",
	"os-info": "CentOS 6",
	"compilers": "GCC 4.4.x",
	"docker-image": "centos6-epel",
	"sysreqs-platform": "linux-x86_64-centos6-epel"
    },

    {
	"name": "linux-x86_64-centos6-epel-rdt",
	"description": "CentOS 6 with Redhat Developer Toolset, R from EPEL",
	"cran-name": null,
	"rversion": "r-release",
	"os-type": "Linux",
	"cpu-type": "x86_64",
	"os-info": "CentOS 6",
	"compilers": "GCC 5.2.1",
	"docker-image": "centos6-epel-rdt",
	"sysreqs-platform": "linux-x86_64-centos6-epel"
    },

    {
	"name": "linux-x86_64-rocker-gcc-san",
	"description": "Debian Linux, R-devel, GCC ASAN/UBSAN",
	"cran-name": null,
	"rversion": "r-devel",
	"os-type": "Linux",
	"cpu-type": "x86_64",
	"os-info": "Debian GNU/Linux testing",
	"compilers": "GCC 5.4.0 (Debian 5.4.0-4)",
	"docker-image": "rocker-gcc-san",
	"sysreqs-platform": "linux-x86_64-debian-gcc"
    },

    {
	"name": "windows-x86_64-oldrel",
	"description": "Windows Server 2008 R2 SP1, R-oldrel, 64 bit",
	"cran-name": null,
	"rversion": "r-oldrel",
	"os-type": "Windows",
	"cpu-type": "x86_64",
	"os-info": "Windows Server 2008 R2 SP1",
	"compilers": "GCC 4.6.3, Rtools 3.3",
	"docker-image": null,
	"sysreqs-platform": "windows-2008"
    }
]'

test_that("platforms", {

  with_mock(
    `rhub::query` = function(...) json,
    {
      expect_silent(p <- platforms())
      expect_true("rhub_platforms" %in% class(p))
      expect_true("data.frame" %in% class(p))
      expect_equal(nrow(p), 11)
    }
  )
})

test_that("print.rhub_platforms", {

  with_mock(
    `rhub::query` = function(...) json,
    expect_output(
      print(platforms()),
      "debian-gcc-patched:.*Debian Linux, R-patched, GCC"
    )
  )
})

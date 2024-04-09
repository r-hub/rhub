# rhub_check

    Code
      rhub_check("https://github.com/r-lib/ps", platforms = c("linux", "clang18"))
    Message
      
      v Check started: linux, clang18 (kleptomaniac-harlequinbug).
        See <https://github.com/r-lib/ps/actions> for live output!

---

    Code
      rhub_check("https://github.com/r-lib/ps", platforms = c("linux", "clang18"))
    Condition
      Error:
      ! :( Failed to start check: I am so, so sorry!.
      i If you think this is a bug in the rhub package, please open an issues at <https://github.com/r-hub/rhub/issues>.

---

    Code
      rhub_check(platforms = c("linux", "clang18"))
    Message
      
      v Check started: linux, clang18 (kleptomaniac-harlequinbug).
        See <https://github.com/r-lib/ps/actions> for live output!


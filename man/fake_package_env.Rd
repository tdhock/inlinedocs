\name{fake_package_env}
\alias{fake_package_env}
\title{fake package env}
\description{Create a fake package environment in a way that keeps S4 working (so
there is a .packageName) and also conforms to byte-code interpreter
requirements on environment structure, particularly ensuring that the
created environment is a namespace.  A similar procedure (with the
exception of not deleting objects) is now in testthat (test_pkg_env).}
\usage{fake_package_env()}



\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus MÃ¼ller [aut], R Core Team [ctb, cph]}






name := "tagless-final"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.5"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",    // source files are in UTF-8
  "-deprecation",          // warn about use of deprecated APIs
  "-unchecked",            // warn about unchecked type parameters
  "-feature",              // warn about misused language features
  // "-Xlint",                // enable handy linter warnings
  // "-Xfatal-warnings",      // turn compiler warnings into errors
  // "-Xlog-implicits",       // log resolution of implicits
  "-Ypartial-unification"  // allow the compiler to unify type constructors of different arities
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

// addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
